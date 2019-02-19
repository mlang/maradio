{-# LANGUAGE BlockArguments, DeriveGeneric, OverloadedLabels, OverloadedStrings, RecordWildCards #-}
module Main where


import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Data.GI.Base.ManagedPtr (castTo)
import Data.GI.Base.Properties (setObjectPropertyInt64, setObjectPropertyString)
import Data.Int (Int64)
import Data.List.PointedList.Circular
import Data.Maybe (fromJust)
import Data.Text (pack, unpack)
import Data.Text.IO
import qualified GI.GLib as GLib
import GI.Gst as Gst
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Input (inputForConfig, Key(..), Event(..), Modifier(..), Input(..))
import System.Console.Haskeline
import Lens.Micro


import Control.Applicative (optional)
import Control.Exception (onException)
import Control.Monad (void)
import Control.Monad.Extra (ifM)
import Dhall hiding (maybe)
import Options.Applicative hiding (auto)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir

type URL = Prelude.String

data Config = Config {
  mpvOptions :: [Prelude.String]
, stations :: [Station]
} deriving (Eq, Generic, Show)
instance Interpret Config

data Station = Station {
  name :: Prelude.String
, url :: URL
} deriving (Eq, Generic, Show)
instance Interpret Station

args :: Config -> Parser Options
args Config {..} =
  Options <$> optional (strOption (  long "duration"
                                  <> metavar "[[hh:]mm:]ss"
                                  <> help "Only play for specified duration"))
          <*> (station stations <|> pure stations)
 where
  station stations = subparser . mconcat $
    map (\s@Station{..} -> command name $ info (pure [s]) mempty) stations

data Options = Options {
  duration :: Maybe Prelude.String
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
  ifM (doesFileExist configFile) (inputFile auto configFile) do
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
  liftIO . forkIO $ GLib.mainLoopRun glibLoop
  playBin <- fromJust <$> Gst.elementFactoryMake "playbin" (Just "Player")
  bus <- fromJust <$> #getBus playBin
  watchId <- #addWatch bus GLib.PRIORITY_DEFAULT (busCall playBin externalPrint)
  void $ #setState playBin Gst.StateNull
  case fromList station of
    Nothing -> pure ()
    Just pointedStation -> do
      let playingUrl = url $ pointedStation ^. focus
      liftIO . externalPrint $ "Setting URL: " <> playingUrl
      liftIO $ setObjectPropertyInt64 playBin "buffer-duration" (3 * fromIntegral Gst.SECOND)
      liftIO $ setObjectPropertyString playBin "uri" (Just . pack $ playingUrl)
      liftIO . externalPrint $ "Playing..."
      void $ #setState playBin Gst.StatePlaying

      hlineLoop playBin pointedStation

  _ <- GLib.sourceRemove watchId
  Gst.objectUnref bus
  #setState playBin Gst.StateNull
  Gst.objectUnref playBin

setPlayBinURI :: MonadIO m => Gst.Element -> Text -> m ()
setPlayBinURI playBin uri = do
  void $ #setState playBin Gst.StateNull
  liftIO $ setObjectPropertyString playBin "uri" (Just uri)
  void $ #setState playBin Gst.StatePlaying

hlineLoop playBin pointedStation = do
  minput <- getInputLine "% "
  case minput of
    Nothing -> pure ()
    Just "next" -> do
      let pointedStation' = next pointedStation
      outputStrLn $ "Now playing " <> name (pointedStation' ^. focus)
      setPlayBinURI playBin (pack . url $ pointedStation' ^. focus)
      hlineLoop playBin pointedStation'
    Just "pause" -> do
      void $ #setState playBin Gst.StatePaused
      hlineLoop playBin pointedStation
    Just "play" -> do
      void $ #setState playBin Gst.StatePlaying
      hlineLoop playBin pointedStation
    Just "quit" -> pure ()
    Just input -> do
      outputStrLn $ "Input was: " <> input
      hlineLoop playBin pointedStation

busCall :: Gst.Element -> (String -> IO ()) -> Gst.Bus -> Gst.Message -> IO Bool
busCall playBin externalPrint bus message = do
  messageTypes <- Gst.getMessageType message
  when (MessageTypeEos `elem` messageTypes) $ do
    externalPrint "End of stream"
  when (MessageTypeTag `elem` messageTypes) $ do
    tagList <- #parseTag message
    #foreach tagList $ \tl t -> case t of
      "title" -> do
        (ok, title) <- #getString tl t
        if ok then externalPrint (unpack title) else pure ()
      _ -> pure () --print t
  when (MessageTypeBuffering `elem` messageTypes) $ do
    percent <- #parseBuffering message
    --externalPrint $ "Buffering... " <> show percent <> "%"
    if percent < 100
    then void $ #setState playBin Gst.StatePaused
    else void $ #setState playBin Gst.StatePlaying
  when (MessageTypeError `elem` messageTypes) $ do
    (gerror,_debug) <- #parseError message
    errorMsg <- Gst.gerrorMessage gerror
    externalPrint $ "Error: " <> unpack errorMsg
    --GLib.mainLoopQuit loop
  pure True
