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

import System.IO

import Language.Cogs.Evaluation.Simplify
import Language.Cogs.Evaluation.Evaluate
import Language.Cogs.Syntax.AST
import Language.Cogs.Parser

import Control.Monad.Reader
import Options.Applicative

data Options =
 Options { debug    :: Bool
         , mode     :: String
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
  <*> strArgument (metavar "MODE"
                  <> help "How to evaluate the program, either 'symbolic' or 'numeric'")
  <*> strArgument (metavar "INPUT" <> help "Input program")
  <*> (optional $ strOption (short 'o' <> metavar "OUTPUT" <> help "output FILE"))



parseOpts :: IO Options
parseOpts = execParser $ info (helper <*> options)
                       $ fullDesc <> progDesc "Cogs Compiler"


runCogs :: ReaderT Options IO ()
runCogs =
  do opts <- ask
     lift $ do
       prog <- readFromFile $ input opts
       let prog' = parseCogs prog
           out   = case output opts of
                     Nothing -> "-"
                     Just x  -> x
       when (debug opts) $ putStrLn $ show prog'
       case mode opts of
         "symbolic" -> writeToFile out . show . simplify $ prog'
         "numeric"  -> writeToFile out . show . evaluate $ prog'
         x          -> hPutStrLn stderr
                         $ show x ++ " is not a Cogs evaluation mode"

readFromFile :: String -> IO String
readFromFile "-" = getContents
readFromFile x   = readFile x

writeToFile :: String -> String -> IO ()
writeToFile "-" = putStrLn
writeToFile x   = writeFile x
