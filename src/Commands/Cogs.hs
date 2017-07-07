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
import Cogs.LanguageDef

import Control.Monad.Reader
import Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
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
  <*> (optional $ strOption (  short 'o'
                            <> metavar "OUTPUT"
                            <> help "output FILE"))

parseOpts :: IO Options
parseOpts = execParser $ info (helper <*> options)
                       $ fullDesc <> progDesc "Cogs Compiler"

runCogs :: ReaderT Options IO ()
runCogs =
  ask >>= \opts ->
  do lang <- lift (getLang (input opts))
     lift $ putStrLn $ "In " ++ show lang ++ ","
     case lang of
       SystemT -> runLanguage systemT
       SystemF -> runLanguage systemF
       DTLC    -> runLanguage dtlc
       _ -> error $ "unimplemented langauge: " ++ show lang

runLanguage :: Language syn -> ReaderT Options IO ()
runLanguage lang =
  ask >>= \opts -> lift $ do
    prog <- readFromFile $ input opts
    when (debug opts) $ TIO.putStrLn prog
    let prog' = parseLang lang prog
    TIO.putStr "· ̌̌⊢ "
    TIO.putStr . (prettyLang lang) $ prog'
    TIO.putStrLn $ " : "
                <> ((prettyLang lang) . (checkLang lang) $ prog')
    TIO.putStrLn . (prettyLang lang) . (evalLang lang) $ prog'

readFromFile :: FilePath -> IO T.Text
readFromFile "-" = TIO.getContents
readFromFile fp  = TIO.readFile fp

writeToFile :: FilePath -> T.Text -> IO ()
writeToFile "-" = TIO.putStrLn
writeToFile fp  = TIO.writeFile fp
