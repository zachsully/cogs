{-# LANGUAGE DataKinds,
             GADTs,
             RankNTypes #-}
module Cogs.LanguageDef where

import qualified Cogs.Language.SystemT.Pretty    as SyST
import qualified Cogs.Language.SystemT.Parser    as SyST
import qualified Cogs.Language.SystemT.TypeCheck as SyST
import qualified Cogs.Language.SystemT.Syntax    as SyST
import qualified Cogs.Language.SystemT.Evaluate  as SyST

import qualified Cogs.Language.SystemF.Pretty    as SySF
import qualified Cogs.Language.SystemF.Parser    as SySF
import qualified Cogs.Language.SystemF.TypeCheck as SySF
import qualified Cogs.Language.SystemF.Syntax    as SySF
import qualified Cogs.Language.SystemF.Evaluate  as SySF

import qualified Cogs.Language.DTLC.Pretty    as DTLC
import qualified Cogs.Language.DTLC.Parser    as DTLC
import qualified Cogs.Language.DTLC.TypeCheck as DTLC
import qualified Cogs.Language.DTLC.Syntax    as DTLC
import qualified Cogs.Language.DTLC.Evaluate  as DTLC

import Data.Text

data Language syn
  = Language
  { parseLang  :: Text -> syn
  , prettyLang :: syn -> Text
  , checkLang  :: syn -> syn
  , evalLang   :: syn -> syn
  }

data LanguageDef l
  = LanguageDef
  { lParse     :: Text -> Either Text l
  , lPretty    :: l -> Text
  , lTypeCheck :: l -> Either Text l
  , lEvaluate  :: l -> l
  , lCompile   :: l -> Text
  }

--------------------------------------------------------------------------------
--                             LANGUAGES                                      --
--------------------------------------------------------------------------------

systemT :: Language (Either SyST.Type (Either SyST.Term SyST.Val))
systemT
  = Language
  { parseLang = \p -> case SyST.parseProg p of
                        Left t -> error $ show t
                        Right p' -> Right (Left p')
  , prettyLang = \p -> case p of
                         Left ty -> SyST.ppType ty
                         Right (Left t) -> SyST.ppTerm t
                         Right (Right v) -> SyST.ppVal v
  , checkLang  = \p -> case p of
                         Left _ -> p
                         Right (Left t) ->
                           case SyST.checkClosedTerm t of
                             Left err -> error $ unpack err
                             Right ty -> Left ty
                         Right (Right _) -> p
  , evalLang   = \p -> case p of
                         Left _ -> p
                         Right (Left t) -> Right . Right $ SyST.evalClosedTerm t
                         Right (Right _) -> p
  }

systemF :: Language (Either SySF.Type (Either SySF.Term SySF.Val))
systemF
  = Language
  { parseLang = \p -> case SySF.parseProg p of
                        Left t -> error $ show t
                        Right p' -> Right (Left p')
  , prettyLang = \p -> case p of
                         Left ty -> SySF.ppType ty
                         Right (Left t) -> SySF.ppTerm t
                         Right (Right v) -> SySF.ppVal v
  , checkLang  = \p -> case p of _ -> undefined
                         -- Left _ -> p
                         -- Right (Left t) -> Left (SySF.checkClosedTerm t)
                         -- Right (Right _) -> p
  , evalLang   = \p -> case p of
                         Left _ -> p
                         Right (Left t) -> Right . Right $ SySF.evalClosedTerm t
                         Right (Right _) -> p
  }

dtlc :: Language (Either DTLC.Type (Either DTLC.Term DTLC.Val))
dtlc
  = Language
  { parseLang  = undefined
  , prettyLang = \p -> case p of
                         Left ty -> DTLC.ppType ty
                         Right (Left t) -> DTLC.ppTerm t
                         Right (Right v) -> DTLC.ppVal v
  , checkLang  = \p -> case p of
                         Left _ -> p
                         Right (Left t) -> undefined -- Left (DTLC.checkClosedTerm t)
                         Right (Right _) -> p
  , evalLang   = undefined
  }
