module Main where

----------------------------------------------------------------
--                                                    2016.08.24
-- |
-- Module      :  Main
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zachsully@gmail.com
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Cogs Executable
--
----------------------------------------------------------------

import System.Environment

import Language.Cogs.Evaluation.Simplify
import Language.Cogs.Evaluation.Evaluate

import Control.Monad.Reader
import Options.Applicative

data Options =
 Options { debug    :: Bool
         , function :: String
         , input    :: String
         } deriving Show

main :: IO ()
main = parseOpts >>= runReaderT runCogs

options :: Parser Options
options = Options
  <$> switch ( long "debug"
             <> short 'D'
             <> help "Prints debug information" )
  <*> strArgument (metavar "FUNCTION" <> help "How to evaluate the program")
  <*> strArgument (metavar "INPUT" <> help "Input program")



parseOpts :: IO Options
parseOpts = execParser $ info (helper <*> options)
                       $ fullDesc <> progDesc "Cogs compiler"


runCogs :: ReaderT Options IO ()
runCogs = undefined
