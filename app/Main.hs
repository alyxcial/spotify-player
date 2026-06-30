-- | spotplay — a small terminal Spotify player built on the hespot library.
--
-- It launches mpv in idle mode with a JSON IPC socket, streams tracks through
-- hespot (fetch -> AES-CTR decrypt -> strip Spotify's Ogg header), and feeds the
-- decrypted Ogg to mpv. A queue with play / pause / next / prev and a live
-- now-playing line is driven over the IPC socket. mpv does the audio; hespot does
-- the protocol, crypto and download.
module Main (main) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception       (SomeException, try)
import           Control.Monad           (forever, when)
import           Data.Aeson              (FromJSON (..), Value (..), decodeStrict, encode, object,
                                          toJSON, withObject, (.:?), (.=))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as BL
import           Data.IORef
import           Data.List               (isInfixOf)
import           Network.Socket            hiding (connect)
import qualified Network.Socket            as Net (connect)
import           Network.Socket.ByteString (recv, sendAll)
import           System.Directory        (doesFileExist, removeFile)
import           System.Environment      (lookupEnv)
import           System.IO               (BufferMode (LineBuffering), hFlush, hSetBuffering, isEOF,
                                          stdout)
import           System.Process          (createProcess, proc, terminateProcess)

import           Spotify
import           Spotify.Audio.Decrypt   (audioDecrypt)
import           Spotify.Audio.Fetch     (fetchEncryptedFile)
import           Spotify.Auth.Cache      (defaultCachePath, loadCredentials)
import           Spotify.Id              (SpotifyId, fileIdRaw, idToRaw, parseTrackUri)
import           Spotify.Metadata        (TrackInfo (..), afFileId, fetchAlbumTracks, fetchTrack,
                                          pickBestOgg)

-- ---------------------------------------------------------------------------
-- types
-- ---------------------------------------------------------------------------

type QItem = (SpotifyId, String)   -- (track id, label — "" until first played)

data Mpv   = Mpv { mpvSock :: Socket, mpvLock :: MVar () }
data Cache = Cache { cPos :: Maybe Double, cDur :: Maybe Double, cPaused :: Bool }
data Cmd   = Play String | Add String | TogglePause | Next | Prev | Stop | Advance

data Player = Player
  { plSess  :: Session
  , plMpv   :: Mpv
  , plChan  :: Chan Cmd
  , plQ     :: MVar ([QItem], Int)   -- queue + current index (-1 = nothing)
  , plCache :: IORef Cache
  , plCtr   :: IORef Int             -- temp-file counter
  }

data MpvMsg = MpvMsg
  { mEvent  :: Maybe String
  , mName   :: Maybe String
  , mData   :: Maybe Value
  , mReason :: Maybe String
  }

instance FromJSON MpvMsg where
  parseJSON = withObject "msg" $ \o ->
    MpvMsg <$> o .:? "event" <*> o .:? "name" <*> o .:? "data" <*> o .:? "reason"

j :: String -> Value
j = toJSON

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  mCreds <- loadCredentials =<< defaultCachePath
  case mCreds of
    Nothing    -> putStrLn "No cached hespot credentials — run `hespot oauth-login` first."
    Just creds -> do
      let sockPath = "/tmp/spotplay.sock"
      removeIfExists sockPath
      aoEnv <- lookupEnv "SPOTPLAY_AO"   -- e.g. SPOTPLAY_AO=null for a headless run
      (_, _, _, ph) <- createProcess (proc "mpv"
        ([ "--idle=yes", "--no-video", "--no-terminal", "--really-quiet"
         , "--input-ipc-server=" <> sockPath ] <> maybe [] (\v -> ["--ao=" <> v]) aoEnv))
      started <- waitForSocket sockPath 40
      if not started
        then putStrLn "mpv didn't start." >> terminateProcess ph
        else do
          sock <- openMpvSocket sockPath 20
          mpv  <- Mpv sock <$> newMVar ()
          mapM_ (\(i, p) -> mpvSend mpv [j "observe_property", toJSON (i :: Int), j p])
                [ (1 :: Int, "time-pos"), (2, "duration"), (3, "pause") ]
          putStrLn "Connecting to Spotify…"
          sess  <- connect defaultConfig creds
          cache <- newIORef (Cache Nothing Nothing False)
          q     <- newMVar ([], -1)
          ctr   <- newIORef 0
          chan  <- newChan
          let pl = Player sess mpv chan q cache ctr
          _ <- forkIO (readerLoop sock cache chan)
          _ <- forkIO (forever (readChan chan >>= handleCmd pl))
          banner
          repl pl
          _ <- try (mpvSend mpv [j "quit"]) :: IO (Either SomeException ())
          close sock
          terminateProcess ph
          disconnect sess
          putStrLn "bye"

-- ---------------------------------------------------------------------------
-- mpv IPC
-- ---------------------------------------------------------------------------

mpvSend :: Mpv -> [Value] -> IO ()
mpvSend m cmd = withMVar (mpvLock m) $ \_ -> do
  _ <- try (sendAll (mpvSock m) (BL.toStrict (encode (object ["command" .= cmd])) <> "\n"))
         :: IO (Either SomeException ())
  pure ()

waitForSocket :: FilePath -> Int -> IO Bool
waitForSocket _ 0 = pure False
waitForSocket p n = do
  ok <- doesFileExist p
  if ok then pure True else threadDelay 100000 >> waitForSocket p (n - 1)

openMpvSocket :: FilePath -> Int -> IO Socket
openMpvSocket path tries = do
  s <- socket AF_UNIX Stream defaultProtocol
  r <- try (Net.connect s (SockAddrUnix path)) :: IO (Either SomeException ())
  case r of
    Right _           -> pure s
    Left _ | tries > 0 -> close s >> threadDelay 100000 >> openMpvSocket path (tries - 1)
    Left e            -> ioError (userError ("mpv socket: " <> show e))

-- read mpv's event/response stream, updating the cache and auto-advancing on EOF
readerLoop :: Socket -> IORef Cache -> Chan Cmd -> IO ()
readerLoop sock cache chan = go ""
  where
    go buf = do
      r <- try (recv sock 8192) :: IO (Either SomeException ByteString)
      case r of
        Left _      -> pure ()
        Right chunk
          | BS.null chunk -> pure ()
          | otherwise     -> do
              let dat   = buf <> chunk
                  parts = BC.split '\n' dat
              mapM_ handleLine (init parts)
              go (last parts)
    handleLine line
      | BS.null line = pure ()
      | otherwise = case decodeStrict line :: Maybe MpvMsg of
          Nothing  -> pure ()
          Just msg -> case mEvent msg of
            Just "end-file"
              | mReason msg == Just "eof" -> writeChan chan Advance
            Just "property-change"        -> updateCache cache msg
            _                             -> pure ()

updateCache :: IORef Cache -> MpvMsg -> IO ()
updateCache ref msg = case mName msg of
  Just "time-pos" -> modifyIORef' ref (\c -> c { cPos = asDouble (mData msg) })
  Just "duration" -> modifyIORef' ref (\c -> c { cDur = asDouble (mData msg) })
  Just "pause"    -> modifyIORef' ref (\c -> c { cPaused = asBool (mData msg) })
  _               -> pure ()
  where
    asDouble (Just (Number n)) = Just (realToFrac n)
    asDouble _                 = Nothing
    asBool   (Just (Bool b))   = b
    asBool   _                 = False

-- ---------------------------------------------------------------------------
-- command handling (single player thread owns the queue)
-- ---------------------------------------------------------------------------

handleCmd :: Player -> Cmd -> IO ()
handleCmd pl = \case
  Play url -> do
    its <- resolve (plSess pl) url
    if null its then putStrLn "  nothing to play (bad URL?)"
    else modifyMVar_ (plQ pl) (\_ -> pure (its, 0)) >> playIdx pl
  Add url -> do
    its <- resolve (plSess pl) url
    if null its then putStrLn "  nothing to queue"
    else do
      start <- modifyMVar (plQ pl) $ \(xs, i) ->
        if i < 0 then pure ((xs ++ its, 0), True) else pure ((xs ++ its, i), False)
      putStrLn ("  queued " <> show (length its))
      when start (playIdx pl)
  Next        -> step pl 1
  Advance     -> step pl 1
  Prev        -> step pl (-1)
  Stop        -> do mpvSend (plMpv pl) [j "stop"]
                    modifyMVar_ (plQ pl) (\(xs, _) -> pure (xs, -1))
  TogglePause -> do c <- readIORef (plCache pl)
                    mpvSend (plMpv pl) [j "set_property", j "pause", toJSON (not (cPaused c))]

step :: Player -> Int -> IO ()
step pl d = do
  moved <- modifyMVar (plQ pl) $ \(xs, i) ->
    let i' = i + d
    in if i' >= 0 && i' < length xs then pure ((xs, i'), True) else pure ((xs, i), False)
  if moved then playIdx pl
           else putStrLn (if d > 0 then "  (end of queue)" else "  (start of queue)")

playIdx :: Player -> IO ()
playIdx pl = do
  (xs, i) <- readMVar (plQ pl)
  if i < 0 || i >= length xs then pure () else do
    let (sid, _) = xs !! i
    r <- fetchOgg pl sid
    putStrLn ""   -- end the "buffering NN%" progress line
    case r of
      Left e -> putStrLn ("  skip (" <> e <> ")") >> step pl 1
      Right (path, label) -> do
        modifyMVar_ (plQ pl) (\(ys, k) -> pure (setAt i (sid, label) ys, k))
        mpvSend (plMpv pl) [j "loadfile", j path, j "replace"]
        mpvSend (plMpv pl) [j "set_property", j "pause", toJSON False]
        putStrLn ("\9654 " <> show (i + 1) <> "/" <> show (length xs) <> "  " <> label)

-- resolve a URL to a list of queue items (a single track, or an album's tracks)
resolve :: Session -> String -> IO [QItem]
resolve sess url = case parseTrackUri (BC.pack url) of
  Left _    -> pure []
  Right sid
    | "album" `isInfixOf` url -> do
        er <- fetchAlbumTracks sess sid
        pure (either (const []) (\(_, ts) -> map (\s -> (s, "")) ts) er)
    | otherwise -> pure [(sid, "")]

-- fetch + decrypt a track to a temp Ogg; returns (path, "Artist - Title")
fetchOgg :: Player -> SpotifyId -> IO (Either String (FilePath, String))
fetchOgg pl sid = do
  et <- fetchTrack (plSess pl) sid
  case et of
    Left e   -> pure (Left ("metadata: " <> e))
    Right ti -> case pickBestOgg (tiFiles ti) of
      Nothing   -> pure (Left "no Ogg stream")
      Just best -> do
        ek <- requestAudioKey (plSess pl) (idToRaw sid) (fileIdRaw (afFileId best))
        case ek of
          Left e    -> pure (Left ("key: " <> e))
          Right key -> do
            ef <- fetchEncryptedFile (plSess pl) (afFileId best)
                    (\g t -> putStr ("\r  buffering " <> show (g * 100 `div` max 1 t) <> "%   ")
                             >> hFlush stdout)
            case ef of
              Left e    -> pure (Left ("fetch: " <> e))
              Right enc -> case audioDecrypt key enc of
                Left e    -> pure (Left ("decrypt: " <> e))
                Right dec -> do
                  n <- readIORef (plCtr pl)
                  writeIORef (plCtr pl) (n + 1)
                  let path  = "/tmp/spotplay-" <> show n <> ".ogg"
                      ogg   = maybe dec (`BS.drop` dec) (findVorbisStart dec)
                      label = BC.unpack (BS.intercalate ", " (tiArtists ti)) <> " - "
                              <> BC.unpack (tiName ti)
                  BS.writeFile path ogg
                  removeIfExists ("/tmp/spotplay-" <> show (n - 1) <> ".ogg")
                  pure (Right (path, label))

-- ---------------------------------------------------------------------------
-- REPL
-- ---------------------------------------------------------------------------

repl :: Player -> IO ()
repl pl = go
  where
    go = do
      putStr "spotplay> "
      hFlush stdout
      eof <- isEOF
      if eof then putStrLn "" else do
        line <- getLine
        keep <- dispatch (words line)
        when keep go
    post = writeChan (plChan pl)
    dispatch ws = case ws of
      []             -> pure True
      ("play"  : u : _) -> post (Play u) >> pure True
      ("queue" : u : _) -> post (Add u)  >> pure True
      ("add"   : u : _) -> post (Add u)  >> pure True
      ["pause"]      -> post TogglePause >> pure True
      ["p"]          -> post TogglePause >> pure True
      ["resume"]     -> post TogglePause >> pure True
      ["next"]       -> post Next  >> pure True
      ["n"]          -> post Next  >> pure True
      ["prev"]       -> post Prev  >> pure True
      ["b"]          -> post Prev  >> pure True
      ["stop"]       -> post Stop  >> pure True
      ["now"]        -> showNow   pl >> pure True
      ["ls"]         -> showQueue pl >> pure True
      ["help"]       -> help >> pure True
      ["?"]          -> help >> pure True
      ["quit"]       -> pure False
      ["q"]          -> pure False
      ["exit"]       -> pure False
      _              -> putStrLn "  unknown — try `help`" >> pure True

showNow :: Player -> IO ()
showNow pl = do
  (xs, i) <- readMVar (plQ pl)
  c       <- readIORef (plCache pl)
  if i < 0 || i >= length xs then putStrLn "  (nothing playing)" else do
    let label    = snd (xs !! i)
        pos      = maybe 0 id (cPos c)
        dur      = maybe 0 id (cDur c)
        barw     = 26 :: Int
        filled   = if dur <= 0 then 0 else min barw (round (pos / dur * fromIntegral barw))
        bar      = replicate filled '\9608' <> replicate (barw - filled) '\9617'
        state    = if cPaused c then "paused " else "playing"
    putStrLn ("  " <> show (i + 1) <> "/" <> show (length xs) <> "  "
              <> (if null label then "track" else label))
    putStrLn ("  " <> bar <> "  " <> fmtT pos <> " / " <> fmtT dur <> "  " <> state)

showQueue :: Player -> IO ()
showQueue pl = do
  (xs, i) <- readMVar (plQ pl)
  if null xs then putStrLn "  (queue empty)"
  else mapM_ (\(k, (_, lbl)) ->
               putStrLn ((if k == i then "  \9654 " else "    ") <> show (k + 1) <> ". "
                         <> (if null lbl then "track" else lbl)))
             (zip [0 ..] xs)

help :: IO ()
help = mapM_ putStrLn
  [ "  play <url>     play a track or album now (replaces the queue)"
  , "  queue <url>    add a track or album to the queue   (alias: add)"
  , "  pause          toggle pause / resume               (alias: p, resume)"
  , "  next | n       skip to the next track"
  , "  prev | b       go to the previous track"
  , "  stop           stop playback"
  , "  now            show the current track + progress"
  , "  ls             list the queue"
  , "  help | ?       this help"
  , "  quit | q       quit"
  ]

banner :: IO ()
banner = mapM_ putStrLn
  [ "spotplay — a terminal Spotify player (on hespot + mpv)"
  , "type `help` for commands; `play <url>` to start."
  ]

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

findVorbisStart :: ByteString -> Maybe Int
findVorbisStart d =
  let (pre, post) = BS.breakSubstring "\1vorbis" d
  in if BS.null post then Nothing else Just (scanBack (BS.length pre - 4))
  where
    scanBack i
      | i <= 0                             = 0
      | BS.isPrefixOf "OggS" (BS.drop i d) = i
      | otherwise                          = scanBack (i - 1)

fmtT :: Double -> String
fmtT t = let s = round t :: Int in show (s `div` 60) <> ":" <> pad2 (s `mod` 60)
  where pad2 n = if n < 10 then '0' : show n else show n

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs <> [x] <> drop (i + 1) xs

removeIfExists :: FilePath -> IO ()
removeIfExists fp = do e <- doesFileExist fp; when e (removeFile fp)
