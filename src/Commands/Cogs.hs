{-# LANGUAGE OverloadedStrings #-}
module Main where

----------------------------------------------------------------
--                                                    2016.08.24
-- |
-- Module      :  Main
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Cogs Executable
--
----------------------------------------------------------------

import System.IO

import Cogs.Preprocessor
import Cogs.Language.SystemT.SystemT
import Cogs.Language.SystemT.Parser

import Control.Monad.Reader
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import Options.Applicative

data Options =
  Options { debug    :: Bool
          , input    :: String
          , output   :: Maybe String
          } deriving Show

main :: IO ()
main = parseOpts >>= runReaderT runCogs

options :: Parser Options
options = Options
  <$> switch ( long "debug"
             <> short 'D'
             <> help "Prints debug information" )
  <*> strArgument (metavar "INPUT" <> help "Input program")
  <*> (optional $ strOption (short 'o' <> metavar "OUTPUT" <> help "output FILE"))

parseOpts :: IO Options
parseOpts = execParser $ info (helper <*> options)
                       $ fullDesc <> progDesc "Cogs Compiler"

runCogs :: ReaderT Options IO ()
runCogs =
  ask >>= \opts ->
  lift $ do
    lang <- getLang (input opts)
    case lang of
      SystemT -> do prog <- readFromFile $ input opts
                    case parseProg prog of
                      Left err -> putStrLn . show $ err
                      Right prog' -> do
                        when (debug opts) $
                          BS.putStrLn prog
                        BS.putStrLn . BS.pack . show $ prog'
                        BS.putStrLn . BS.pack . show . evalClosedTerm $ prog'
      _ -> return ()

readFromFile :: FilePath -> IO BS.ByteString
readFromFile "-" = BS.getContents
readFromFile fp  = BS.readFile fp

writeToFile :: FilePath -> BS.ByteString -> IO ()
writeToFile "-" = BS.putStrLn
writeToFile fp  = BS.writeFile fp
