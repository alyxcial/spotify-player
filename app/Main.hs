-- | spotplay — a terminal Spotify player (TUI) built on the hespot library.
--
-- A brick/vty terminal UI: a now-playing panel with cover art (rendered as
-- Unicode half-blocks), a progress bar and a queue list, with keyboard controls.
-- mpv plays the audio over its JSON IPC socket; hespot fetches + decrypts the
-- tracks (over the CDN). Output goes to the UI, not stdout.
module Main (main) where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception         (SomeException, try)
import           Control.Monad             (forever, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (get, modify)
import           Data.Aeson                (FromJSON (..), Value (..), decodeStrict, encode, object,
                                            toJSON, withObject, (.:?), (.=))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as BL
import           Data.IORef
import           Data.List                 (isInfixOf)
import           Data.Maybe                (fromMaybe, isNothing)
import           Network.Socket            hiding (connect)
import qualified Network.Socket            as Net (connect)
import           Network.Socket.ByteString (recv, sendAll)
import           System.Directory          (doesFileExist, removeFile)
import           System.Environment        (lookupEnv)
import           System.Process            (createProcess, proc, terminateProcess)

import qualified Brick                     as Bk
import           Brick                     (App (..), BrickEvent (..), Widget, (<=>))
import           Brick.AttrMap             (AttrMap, attrMap, attrName)
import           Brick.BChan               (BChan, newBChan, writeBChan)
import qualified Brick.Widgets.Border      as B
import           Brick.Widgets.Center      (center)
import           Brick.Widgets.Core        (hBox, hLimit, padRight, raw, str, vBox, vLimit,
                                            withAttr)
import qualified Graphics.Vty              as V
import           Codec.Picture             (convertRGB8, decodeImage, imageHeight, imageWidth,
                                            pixelAt)
import           Codec.Picture.Types       (PixelRGB8 (..))

import           Spotify
import           Spotify.Audio.Cdn         (fetchEncryptedFileCdn)
import           Spotify.Audio.Decrypt     (audioDecrypt)
import           Spotify.Audio.Fetch       (fetchEncryptedFile)
import           Spotify.Auth.Cache        (defaultCachePath, loadCredentials)
import           Spotify.Auth.Modern       (modernTokens)
import           Spotify.Id                (SpotifyId, fileIdRaw, idToHex, idToRaw, parseTrackUri)
import           Spotify.Metadata          (TrackInfo (..), afFileId, fetchAlbumTracks, fetchTrack,
                                            pickBestOgg)
import           Spotify.Net.ApResolve     (resolveSpclient)
import           Spotify.WebApi            (fetchImage)

-- ---------------------------------------------------------------------------
-- backend types
-- ---------------------------------------------------------------------------

type QItem = (SpotifyId, String)

data Mpv   = Mpv { mpvSock :: Socket, mpvLock :: MVar () }
data Cache = Cache { cPos :: Maybe Double, cDur :: Maybe Double, cPaused :: Bool }
data Cmd   = Play String | Add String | TogglePause | Next | Prev | Stop | Advance | Jump Int

data Player = Player
  { plSess  :: Session
  , plMpv   :: Mpv
  , plChan  :: Chan Cmd
  , plQ     :: MVar ([QItem], Int)
  , plCache :: IORef Cache
  , plCdn   :: Maybe (ByteString, ByteString, String)
  , plUi    :: BChan Ev
  }

data MpvMsg = MpvMsg
  { mEvent :: Maybe String, mName :: Maybe String, mData :: Maybe Value, mReason :: Maybe String }

instance FromJSON MpvMsg where
  parseJSON = withObject "msg" $ \o ->
    MpvMsg <$> o .:? "event" <*> o .:? "name" <*> o .:? "data" <*> o .:? "reason"

j :: String -> Value
j = toJSON

-- ---------------------------------------------------------------------------
-- UI types
-- ---------------------------------------------------------------------------

data Ev = Tick | Status String | Cover SpotifyId V.Image

data UiState = UiState
  { uiPl     :: Player
  , uiQueue  :: [QItem]
  , uiIdx    :: Int
  , uiCache  :: Cache
  , uiCover  :: Maybe V.Image
  , uiCovFor :: Maybe SpotifyId
  , uiSel    :: Int
  , uiStatus :: String
  , uiInput  :: Maybe (Bool, String)   -- (isPlay, text); Nothing = not entering
  }

type Name = ()

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  mCreds <- loadCredentials =<< defaultCachePath
  case mCreds of
    Nothing    -> putStrLn "No cached hespot credentials — run `hespot oauth-login` first."
    Just creds -> do
      putStrLn "spotplay: starting (launching mpv, connecting to Spotify)…"
      let sockPath = "/tmp/spotplay.sock"
      removeIfExists sockPath
      aoEnv <- lookupEnv "SPOTPLAY_AO"
      (_, _, _, ph) <- createProcess (proc "mpv"
        ([ "--idle=yes", "--no-video", "--no-terminal", "--really-quiet"
         , "--input-ipc-server=" <> sockPath ] <> maybe [] (\v -> ["--ao=" <> v]) aoEnv))
      started <- waitForSocket sockPath 40
      if not started then putStrLn "mpv didn't start." >> terminateProcess ph else do
        sock <- openMpvSocket sockPath 20
        mpv  <- Mpv sock <$> newMVar ()
        mapM_ (\(i, p) -> mpvSend mpv [j "observe_property", toJSON (i :: Int), j p])
              [ (1 :: Int, "time-pos"), (2, "duration"), (3, "pause") ]
        sess  <- connect defaultConfig creds
        mcdn  <- do et <- modernTokens "0123456789abcdef0123456789abcdef01234567" creds
                    case et of
                      Right (tok, ct) -> do (sp, _) <- resolveSpclient; pure (Just (tok, ct, sp))
                      Left _          -> pure Nothing
        cache <- newIORef (Cache Nothing Nothing False)
        q     <- newMVar ([], -1)
        chan  <- newChan
        bchan <- newBChan 32
        let pl = Player sess mpv chan q cache mcdn bchan
        _ <- forkIO (readerLoop sock cache chan)
        _ <- forkIO (forever (readChan chan >>= handleCmd pl))
        _ <- forkIO (forever (threadDelay 500000 >> writeBChan bchan Tick))
        let st0 = UiState pl [] (-1) (Cache Nothing Nothing False) Nothing Nothing 0
                          (if isNothing mcdn then "(CDN unavailable — slow channel)" else "ready") Nothing
        _ <- Bk.customMainWithDefaultVty (Just bchan) app st0
        _ <- try (mpvSend mpv [j "quit"]) :: IO (Either SomeException ())
        close sock
        terminateProcess ph
        disconnect sess

-- ---------------------------------------------------------------------------
-- brick app
-- ---------------------------------------------------------------------------

app :: App UiState Ev Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = Bk.neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "cur",  V.defAttr `V.withStyle` V.bold `V.withForeColor` V.green)
  , (attrName "sel",  V.defAttr `V.withStyle` V.reverseVideo)
  , (attrName "dim",  V.defAttr `V.withForeColor` V.brightBlack)
  , (attrName "bar",  V.defAttr `V.withForeColor` V.green)
  ]

drawUI :: UiState -> [Widget Name]
drawUI s = [ top <=> B.hBorder <=> queue <=> B.hBorder <=> statusLine ]
  where
    top = hBox [ coverBox, padRight (Bk.Pad 2) (str " "), nowBox ]
    coverBox = B.border $ vLimit 12 $ hLimit 24 $ case uiCover s of
      Just img -> raw img
      Nothing  -> center (withAttr (attrName "dim") (str "no cover"))
    nowBox =
      let (artist, title) = nowArtistTitle s
          c   = uiCache s
          pos = fromMaybe 0 (cPos c); dur = fromMaybe 0 (cDur c)
          st  = if uiIdx s < 0 then "stopped" else if cPaused c then "paused" else "playing"
      in vBox
         [ withAttr (attrName "dim") (str "Now playing")
         , withAttr (attrName "cur") (str (take 48 title))
         , str (take 48 artist)
         , str " "
         , withAttr (attrName "bar") (str (progressBar 30 pos dur))
         , str (fmtT pos <> " / " <> fmtT dur <> "   " <> st)
         ]
    queue =
      let items = uiQueue s
      in if null items then withAttr (attrName "dim") (str " (queue empty — press a to add, or play <url>)")
         else vBox [ row k it | (k, it) <- zip [0 ..] items ]
      where
        row k (_, lbl) =
          let mark = if k == uiIdx s then " > " else "   "
              line = mark <> show (k + 1) <> ". " <> (if null lbl then "track" else lbl)
              base = if k == uiIdx s then withAttr (attrName "cur") (str line) else str line
          in if k == uiSel s then withAttr (attrName "sel") base else base
    statusLine = case uiInput s of
      Just (isPlay, txt) -> str ((if isPlay then "play> " else "queue> ") <> txt <> "_")
      Nothing            -> withAttr (attrName "dim") (str (" " <> uiStatus s
                            <> "   |  space pause · n/p next/prev · ↑↓ select · ⏎ play · a add · q quit"))

handleEvent :: BrickEvent Name Ev -> Bk.EventM Name UiState ()
handleEvent (AppEvent ev) = case ev of
  Tick        -> do
    s <- get
    (qs, i) <- liftIO (readMVar (plQ (uiPl s)))
    c       <- liftIO (readIORef (plCache (uiPl s)))
    modify (\st -> st { uiQueue = qs, uiIdx = i, uiCache = c
                      , uiSel = if uiSel st < 0 then 0 else min (uiSel st) (max 0 (length qs - 1))
                      , uiCover = if uiCovFor st == cur qs i then uiCover st else Nothing })
  Status m    -> modify (\st -> st { uiStatus = m })
  Cover sid i -> modify (\st -> st { uiCover = Just i, uiCovFor = Just sid })
  where cur qs i = if i >= 0 && i < length qs then Just (fst (qs !! i)) else Nothing
handleEvent (VtyEvent (V.EvKey key mods)) = do
  s <- get
  case uiInput s of
    Just (isPlay, txt) -> case key of
      V.KEsc        -> modify (\st -> st { uiInput = Nothing })
      V.KEnter      -> do modify (\st -> st { uiInput = Nothing })
                          liftIO (writeChan (plChan (uiPl s)) ((if isPlay then Play else Add) txt))
      V.KBS         -> modify (\st -> st { uiInput = Just (isPlay, dropLast txt) })
      V.KChar ch    -> modify (\st -> st { uiInput = Just (isPlay, txt <> [ch]) })
      _             -> pure ()
    Nothing -> case key of
      V.KChar 'q'   -> Bk.halt
      V.KEsc        -> Bk.halt
      V.KChar ' '   -> send s TogglePause
      V.KChar 'n'   -> send s Next
      V.KChar 'p'   -> send s Prev
      V.KChar 'b'   -> send s Prev
      V.KChar 's'   -> send s Stop
      V.KChar 'a'   -> modify (\st -> st { uiInput = Just (False, "") })
      V.KChar 'o'   -> modify (\st -> st { uiInput = Just (True,  "") })
      V.KChar '/'   -> modify (\st -> st { uiInput = Just (True,  "") })
      V.KChar 'j'   -> modify (\st -> st { uiSel = clampSel st (uiSel st + 1) })
      V.KChar 'k'   -> modify (\st -> st { uiSel = clampSel st (uiSel st - 1) })
      V.KDown       -> modify (\st -> st { uiSel = clampSel st (uiSel st + 1) })
      V.KUp         -> modify (\st -> st { uiSel = clampSel st (uiSel st - 1) })
      V.KEnter      -> send s (Jump (uiSel s))
      V.KChar 'c'   | V.MCtrl `elem` mods -> Bk.halt
      _             -> pure ()
  where
    send st c = liftIO (writeChan (plChan (uiPl st)) c)
    clampSel st n = max 0 (min n (length (uiQueue st) - 1))
handleEvent _ = pure ()

dropLast :: [a] -> [a]
dropLast [] = []
dropLast xs = init xs

nowArtistTitle :: UiState -> (String, String)
nowArtistTitle s
  | uiIdx s < 0 || uiIdx s >= length (uiQueue s) = ("", "—")
  | otherwise = case snd (uiQueue s !! uiIdx s) of
      lbl -> case break (== '-') lbl of
        (a, '-' : t) -> (trim a, trim t)
        _            -> ("", if null lbl then "track" else lbl)
  where trim = f . f where f = reverse . dropWhile (== ' ')

progressBar :: Int -> Double -> Double -> String
progressBar w pos dur =
  let filled = if dur <= 0 then 0 else min w (round (pos / dur * fromIntegral w))
  in replicate filled '\9608' <> replicate (w - filled) '\9617'

-- ---------------------------------------------------------------------------
-- mpv IPC + backend (output routed to the UI via the BChan)
-- ---------------------------------------------------------------------------

mpvSend :: Mpv -> [Value] -> IO ()
mpvSend m cmd = withMVar (mpvLock m) $ \_ -> do
  _ <- try (sendAll (mpvSock m) (BL.toStrict (encode (object ["command" .= cmd])) <> "\n"))
         :: IO (Either SomeException ())
  pure ()

waitForSocket :: FilePath -> Int -> IO Bool
waitForSocket _ 0 = pure False
waitForSocket p n = do ok <- doesFileExist p
                       if ok then pure True else threadDelay 100000 >> waitForSocket p (n - 1)

openMpvSocket :: FilePath -> Int -> IO Socket
openMpvSocket path tries = do
  s <- socket AF_UNIX Stream defaultProtocol
  r <- try (Net.connect s (SockAddrUnix path)) :: IO (Either SomeException ())
  case r of
    Right _            -> pure s
    Left _ | tries > 0 -> close s >> threadDelay 100000 >> openMpvSocket path (tries - 1)
    Left e             -> ioError (userError ("mpv socket: " <> show e))

readerLoop :: Socket -> IORef Cache -> Chan Cmd -> IO ()
readerLoop sock cache chan = go ""
  where
    go buf = do
      r <- try (recv sock 8192) :: IO (Either SomeException ByteString)
      case r of
        Left _ -> pure ()
        Right chunk
          | BS.null chunk -> pure ()
          | otherwise -> do let parts = BC.split '\n' (buf <> chunk)
                            mapM_ handleLine (init parts)
                            go (last parts)
    handleLine line
      | BS.null line = pure ()
      | otherwise = case decodeStrict line :: Maybe MpvMsg of
          Just msg -> case mEvent msg of
            Just "end-file" | mReason msg == Just "eof" -> writeChan chan Advance
            Just "property-change"                      -> updateCache cache msg
            _                                           -> pure ()
          Nothing -> pure ()

updateCache :: IORef Cache -> MpvMsg -> IO ()
updateCache ref msg = case mName msg of
  Just "time-pos" -> modifyIORef' ref (\c -> c { cPos = asD (mData msg) })
  Just "duration" -> modifyIORef' ref (\c -> c { cDur = asD (mData msg) })
  Just "pause"    -> modifyIORef' ref (\c -> c { cPaused = asB (mData msg) })
  _               -> pure ()
  where asD (Just (Number n)) = Just (realToFrac n); asD _ = Nothing
        asB (Just (Bool b))   = b;                   asB _ = False

say :: Player -> String -> IO ()
say pl = writeBChan (plUi pl) . Status

handleCmd :: Player -> Cmd -> IO ()
handleCmd pl = \case
  Play url -> do its <- resolve (plSess pl) url
                 if null its then say pl "nothing to play (bad URL?)"
                 else modifyMVar_ (plQ pl) (\_ -> pure (its, 0)) >> playIdx pl
  Add url  -> do its <- resolve (plSess pl) url
                 if null its then say pl "nothing to queue"
                 else do start <- modifyMVar (plQ pl) $ \(xs, i) ->
                           if i < 0 then pure ((xs ++ its, 0), True) else pure ((xs ++ its, i), False)
                         say pl ("queued " <> show (length its))
                         when start (playIdx pl)
  Jump n      -> do modifyMVar_ (plQ pl) (\(xs, i) -> pure (xs, if n >= 0 && n < length xs then n else i))
                    playIdx pl
  Next        -> step pl 1
  Advance     -> step pl 1
  Prev        -> step pl (-1)
  Stop        -> do mpvSend (plMpv pl) [j "stop"]; modifyMVar_ (plQ pl) (\(xs, _) -> pure (xs, -1))
  TogglePause -> do c <- readIORef (plCache pl)
                    mpvSend (plMpv pl) [j "set_property", j "pause", toJSON (not (cPaused c))]

step :: Player -> Int -> IO ()
step pl d = do
  moved <- modifyMVar (plQ pl) $ \(xs, i) ->
    let i' = i + d in if i' >= 0 && i' < length xs then pure ((xs, i'), True) else pure ((xs, i), False)
  if moved then playIdx pl else say pl (if d > 0 then "end of queue" else "start of queue")

playIdx :: Player -> IO ()
playIdx pl = do
  (xs, i) <- readMVar (plQ pl)
  when (i >= 0 && i < length xs) $ do
    let (sid, _) = xs !! i
    say pl "buffering…"
    r <- fetchOgg pl sid
    case r of
      Left e -> say pl ("skip: " <> e) >> step pl 1
      Right (path, label) -> do
        modifyMVar_ (plQ pl) (\(ys, k) -> pure (setAt i (sid, label) ys, k))
        mpvSend (plMpv pl) [j "loadfile", j path, j "replace"]
        mpvSend (plMpv pl) [j "set_property", j "pause", toJSON False]
        say pl label

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
            ef <- case plCdn pl of
              Just (tok, ct, sp) -> do
                r <- fetchEncryptedFileCdn tok ct sp (afFileId best)
                either (const (fetchEncryptedFile (plSess pl) (afFileId best) (\_ _ -> pure ())))
                       (pure . Right) r
              Nothing -> fetchEncryptedFile (plSess pl) (afFileId best) (\_ _ -> pure ())
            case ef of
              Left e    -> pure (Left ("fetch: " <> e))
              Right enc -> case audioDecrypt key enc of
                Left e    -> pure (Left ("decrypt: " <> e))
                Right dec -> do
                  let path  = "/tmp/spotplay-" <> BC.unpack (idToHex sid) <> ".ogg"
                      ogg   = maybe dec (`BS.drop` dec) (findVorbisStart dec)
                      label = BC.unpack (BS.intercalate ", " (tiArtists ti)) <> " - "
                              <> BC.unpack (tiName ti)
                  BS.writeFile path ogg
                  when (tiCoverId ti /= "") $ void $ forkIO $ do
                    ci <- fetchImage (tiCoverId ti)
                    case ci of
                      Right bytes -> maybe (pure ()) (writeBChan (plUi pl) . Cover sid)
                                           (renderCover 24 11 bytes)
                      Left _      -> pure ()
                  pure (Right (path, label))

resolve :: Session -> String -> IO [QItem]
resolve sess url = case parseTrackUri (BC.pack url) of
  Left _ -> pure []
  Right sid
    | "album" `isInfixOf` url -> do er <- fetchAlbumTracks sess sid
                                    pure (either (const []) (\(_, ts) -> map (\x -> (x, "")) ts) er)
    | otherwise -> pure [(sid, "")]

-- ---------------------------------------------------------------------------
-- cover art: JPEG -> vty Image of half-blocks
-- ---------------------------------------------------------------------------

renderCover :: Int -> Int -> ByteString -> Maybe V.Image
renderCover cols rows jpeg = case decodeImage jpeg of
  Left _    -> Nothing
  Right dyn ->
    let img = convertRGB8 dyn
        w = imageWidth img; h = imageHeight img
        sample px py = let PixelRGB8 r g b = pixelAt img (clamp (w - 1) (px * w `div` cols))
                                                         (clamp (h - 1) (py * h `div` (rows * 2)))
                       in V.rgbColor (fromIntegral r :: Int) (fromIntegral g) (fromIntegral b)
        clamp hi v = max 0 (min hi v)
        cell x y = V.char (V.defAttr `V.withForeColor` sample x (2 * y)
                                     `V.withBackColor` sample x (2 * y + 1)) '\9600'
        line y = V.horizCat [ cell x y | x <- [0 .. cols - 1] ]
    in if w <= 0 || h <= 0 then Nothing else Just (V.vertCat [ line y | y <- [0 .. rows - 1] ])

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

findVorbisStart :: ByteString -> Maybe Int
findVorbisStart d =
  let (pre, post) = BS.breakSubstring "\1vorbis" d
  in if BS.null post then Nothing else Just (scanBack (BS.length pre - 4))
  where scanBack i | i <= 0                             = 0
                   | BS.isPrefixOf "OggS" (BS.drop i d) = i
                   | otherwise                          = scanBack (i - 1)

fmtT :: Double -> String
fmtT t = let s = round t :: Int in show (s `div` 60) <> ":" <> pad2 (s `mod` 60)
  where pad2 n = if n < 10 then '0' : show n else show n

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs <> [x] <> drop (i + 1) xs

removeIfExists :: FilePath -> IO ()
removeIfExists fp = do e <- doesFileExist fp; when e (removeFile fp)
