{-# LANGUAGE BlockArguments, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Exception (onException)
import Control.Monad (void)
import Control.Monad.Extra (ifM)
import Dhall
import Options.Applicative hiding (auto)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir
import System.Process (rawSystem)

type URL = String

data Config = Config {
  mpvOptions :: [String]
, stations :: [Station]
} deriving (Generic, Show)
instance Interpret Config

data Station = Station {
  name :: String
, url :: URL
} deriving (Generic, Show)
instance Interpret Station

station :: [Station] -> Parser Station
station = subparser . mconcat . map \s@Station {..} ->
  command name $ info (pure s) mempty

main :: IO ()
main = do
  cfg <- getConfig
  Station {..} <- execParser (opts cfg)
  play cfg url

opts cfg = info (station (stations cfg) <**> helper) $ mempty

play :: Config -> URL -> IO ()
play Config{..} url = void $ rawSystem "mpv" (mpvOptions <> [url])

defaultConfig :: IO Config
defaultConfig = do
  let mpvOptions = ["-no-video", "-quiet"]
  stations <- input auto "https://mlang.github.io/maradio/stations"
  pure $ Config { .. }

getConfig :: IO Config
getConfig = do
  configFile <- getUserConfigFile "maradio" "config"
  ifM (doesFileExist configFile)
    (inputFile auto configFile)
    defaultConfig
