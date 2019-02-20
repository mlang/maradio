{-# LANGUAGE BlockArguments, DeriveGeneric, FlexibleContexts, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Applicative (optional)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad (join, when, void)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class (lift)
import Data.GI.Base.ManagedPtr (castTo)
import Data.GI.Base.Properties (getObjectPropertyString, setObjectPropertyInt64, setObjectPropertyString)
import Data.List.PointedList.Circular
import Data.Maybe (fromJust)
import Data.Text (pack, unpack)
import Data.Text.IO
import Dhall hiding (maybe)
import qualified GI.GLib as GLib
import qualified GI.Gst as Gst
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Input (inputForConfig, Key(..), Event(..), Modifier(..), Input(..))
import Lens.Micro
import Network.URI.TLD
import Options.Applicative hiding (auto)
import System.Console.Haskeline
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir
import System.Process (readProcess)
import Text.CLD2

type URL = String

data Config = Config {
  mpvOptions :: [String]
, stations :: [Station]
} deriving (Eq, Generic, Show)
instance Interpret Config

data Station = Station {
  name :: Prelude.String
, url :: URL
} deriving (Eq, Generic, Show)
instance Interpret Station

args :: Config -> Parser Options
args Config{..} =
  Options <$> optional (strOption (  long "duration"
                                  <> metavar "[[hh:]mm:]ss"
                                  <> help "Only play for specified duration"))
          <*> (station stations <|> pure stations)
 where
  station stations = subparser . mconcat $
    map (\s@Station{..} -> command name $ info (pure [s]) mempty) stations

data Options = Options {
  duration :: Maybe String
, station :: [Station]
} deriving (Eq, Show)

main :: IO ()
main = do
  cfg <- getConfig
  gmain cfg =<< execParser (opts cfg)

opts cfg = info (args cfg <**> helper) $ mempty

getConfig :: IO Config
getConfig = do
  configFile <- getUserConfigFile "maradio" "config"
  ifM (doesFileExist configFile) (inputFile auto configFile) $ do
    statusMsg <- input auto "https://mlang.github.io/maradio/mpv-term-status-msg"
    let mpvOptions = ["-no-video", "-quiet", statusMsg]
    stations <- input auto "https://mlang.github.io/maradio/stations"
    pure $ Config{..}

gmain :: Config -> Options -> IO ()
gmain Config{..} o@Options{..} = do
  runInputT defaultSettings (hline o)

hline Options{..} = do
  externalPrint <- getExternalPrint
  void $ Gst.init Nothing
  glibLoop <- GLib.mainLoopNew Nothing True
  liftIO . forkIO $ #run glibLoop
  playBin <- fromJust <$> Gst.elementFactoryMake "playbin" (Just "Player")
  --pipeline <- liftIO $ fromJust <$> castTo Gst.Pipeline playBin
  bus <- fromJust <$> #getBus playBin
  watchId <- #addWatch bus GLib.PRIORITY_DEFAULT (busCall externalPrint playBin)
  void $ #setState playBin Gst.StateNull
  case fromList station of
    Nothing -> pure ()
    Just pointedStation -> do
      let playingUrl = url $ pointedStation ^. focus
      liftIO $ do
        externalPrint $ "Setting URL: " <> playingUrl
        setObjectPropertyInt64 playBin "buffer-duration" (3 * fromIntegral Gst.SECOND)
        setObjectPropertyString playBin "uri" (Just . pack $ playingUrl)
        externalPrint $ "Playing..."
      #setState playBin Gst.StatePlaying

      repl `runReaderT` playBin `evalStateT` pointedStation

  _ <- GLib.sourceRemove watchId
  Gst.objectUnref bus
  #setState playBin Gst.StateNull
  Gst.objectUnref playBin
  #quit glibLoop

data REPLCommand = Radio Radio
data Radio = Play | Pause | Toggle | Next | Prev | URL Text

radio :: Parser Radio
radio = flag' Play (long "play")
    <|> flag' Pause (long "pause")
    <|> flag' Next (long "next")
    <|> flag' Prev (long "prev")
    <|> URL . pack <$> strArgument (metavar "URL")
    <|> pure Toggle

replCmd :: Parser REPLCommand
replCmd = hsubparser $
  command "radio" (info (Radio <$> radio) mempty)

setURI :: (MonadReader Gst.Element m, MonadIO m) => Text -> m ()
setURI uri = do
  playBin <- ask
  void $ #setState playBin Gst.StateNull
  liftIO $ setObjectPropertyString playBin "uri" (Just uri)
  void $ #setState playBin Gst.StatePlaying

pause, play, toggle :: (MonadReader Gst.Element m, MonadIO m) => m ()
pause = do
  playBin <- ask
  void $ #setState playBin Gst.StatePaused

play = do
  playBin <- ask
  void $ #setState playBin Gst.StatePlaying

toggle = do
  playBin <- ask
  (stateReturn, state, _) <- #getState playBin Gst.CLOCK_TIME_NONE
  case stateReturn of
    Gst.StateChangeReturnSuccess -> case state of
      Gst.StateReady -> play
      Gst.StatePaused -> play
      Gst.StatePlaying -> pause
      _ -> pure ()
    _ -> pure ()
    
type REPL = ReaderT Gst.Element (StateT (PointedList Station) (InputT IO))

repl :: REPL ()
repl = do
  minput <- lift . lift $ getInputLine "% "
  case minput of
    Nothing -> pure ()
    Just input -> case execParserPure Options.Applicative.defaultPrefs (info (replCmd <**> helper) mempty) (words input) of
      Success cmd -> do
        case cmd of
          Radio rcmd -> case rcmd of
            Play -> play
            Pause -> pause
            Toggle -> toggle
            Next -> do
              modify next
              s <- gets (^. focus)
              lift . lift . outputStrLn $ "Now playing " <> name s
              setURI (pack . url $ s)
            Prev -> do
              modify previous
              s <- gets (^. focus)
              lift . lift . outputStrLn $ "Now playing " <> name s
              setURI (pack . url $ s)
            URL url -> setURI url
        repl
      Failure failure -> do
        lift . lift . outputStrLn . fst $ renderFailure failure ""
        repl
      _ -> repl

busCall :: (String -> IO ()) -> Gst.Element -> Gst.Bus -> Gst.Message -> IO Bool
busCall externalPrint playBin bus message = do
  messageTypes <- Gst.getMessageType message
  when (Gst.MessageTypeEos `elem` messageTypes) $ do
    externalPrint "End of stream"
  when (Gst.MessageTypeTag `elem` messageTypes) $ do
    tagList <- #parseTag message
    #foreach tagList $ \tl t -> case t of
      "title" -> do
        (ok, title) <- #getString tl t
        if ok
          then do
            externalPrint (unpack title)
            tld <- fmap (^. _3) . join . fmap parseTLDText <$>
                   getObjectPropertyString playBin "current-uri"
            speak tld title
          else pure ()
      _ -> pure () --print t
  when (Gst.MessageTypeBuffering `elem` messageTypes) $ do
    percent <- #parseBuffering message
    --externalPrint $ "Buffering... " <> show percent <> "%"
    void $ #setState playBin if percent < 100 then Gst.StatePaused else Gst.StatePlaying
  when (Gst.MessageTypeError `elem` messageTypes) $ do
    (gerror,_debug) <- #parseError message
    errorMsg <- Gst.gerrorMessage gerror
    externalPrint $ "Error: " <> unpack errorMsg
    --GLib.mainLoopQuit loop
  pure True

speak :: Maybe Text -> Text -> IO ()
speak tld text = do
  let hints = defaultHints { hintTLD = unpack <$> tld }
  let voice = case resultSimple $ detectLanguage text True hints of
                Cld2Language_CZECH   -> ["-v", "cs"]
                Cld2Language_DANISH  -> ["-v", "da"]
                Cld2Language_DUTCH   -> ["-v", "nl"]
                Cld2Language_ENGLISH -> ["-v", "en"]
                Cld2Language_FINNISH -> ["-v", "fi"]
                Cld2Language_FRENCH  -> ["-v", "fr-fr"]
                Cld2Language_GERMAN  -> ["-v", "de"]
                Cld2Language_ITALIAN -> ["-v", "it"]
                Cld2Language_SPANISH -> ["-v", "es"]
                Cld2Language_WELSH   -> ["-v", "cy"]
                _                    -> []
  void . forkIO . void $ readProcess "espeak" (voice <> ["-a", show 200]) (unpack text)
