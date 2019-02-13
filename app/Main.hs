{-# LANGUAGE BlockArguments, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Applicative (optional)
import Control.Exception (onException)
import Control.Monad (void)
import Control.Monad.Extra (ifM)
import Dhall hiding (maybe)
import Options.Applicative hiding (auto)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir
import System.Process (rawSystem)

type URL = String

data Config = Config {
  mpvOptions :: [String]
, stations :: [Station]
} deriving (Eq, Generic, Show)
instance Interpret Config

data Station = Station {
  name :: String
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
  duration :: Maybe String
, station :: [Station]
} deriving (Eq, Show)

main :: IO ()
main = do
  cfg <- getConfig
  play cfg =<< execParser (opts cfg)

opts cfg = info (args cfg <**> helper) $ mempty

play :: Config -> Options -> IO ()
play Config{..} Options{..} = void $
  rawSystem "mpv" $
  mpvOptions <> maybe [] mpvLength duration <> map url station
 where
  mpvLength d = ["--length=" <> d]

getConfig :: IO Config
getConfig = do
  configFile <- getUserConfigFile "maradio" "config"
  ifM (doesFileExist configFile) (inputFile auto configFile) do
    let mpvOptions = ["-no-video", "-quiet"]
    stations <- input auto "https://mlang.github.io/maradio/stations"
    pure $ Config{..}
