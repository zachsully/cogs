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
import Cogs.Language.Common.Parser
import Cogs.Language.Common.Pretty
import Cogs.Language.Common.Syntax
import Cogs.Language.SystemT.Syntax
import Cogs.Language.SystemT.Evaluate

import Control.Monad.Reader
import Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Options.Applicative

data Options
  = Options
  { debug    :: Bool
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
  ask >>= \opts -> lift $
    do { prog <- readFromFile (input opts)
       ; case parseExpr prog of
           Left err -> TIO.putStrLn err
           Right expr ->
             do { TIO.putStrLn . T.pack . show $ expr
                ; TIO.putStrLn "\n*********** Common AST ***********\n"
                ; TIO.putStrLn . T.pack . show $ expr
                ; TIO.putStrLn "\n*********** SystemT AST ***********\n"
                ; case (from :: Expr -> Maybe Term) expr of
                    Nothing -> TIO.putStrLn "cannot parse"
                    Just expr' ->
                      do { TIO.putStrLn . T.pack . show $ expr'
                         ; let expr'' = evalClosedTerm expr'
                         ; TIO.putStrLn "\n*********** Common AST ***********\n"
                         ; let expr''' = (to :: Val -> Expr) expr''
                         ; TIO.putStrLn . ppExpr $ expr'''
                         }
                }
       }

readFromFile :: FilePath -> IO T.Text
readFromFile "-" = TIO.getContents
readFromFile fp  = TIO.readFile fp

writeToFile :: FilePath -> T.Text -> IO ()
writeToFile "-" = TIO.putStrLn
writeToFile fp  = TIO.writeFile fp
