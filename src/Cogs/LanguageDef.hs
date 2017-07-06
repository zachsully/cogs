{-# LANGUAGE DataKinds,
             GADTs,
             RankNTypes #-}
module Cogs.LanguageDef where

import qualified Cogs.Language.SystemT.Pretty    as SyST
import qualified Cogs.Language.SystemT.Parser    as SyST
import qualified Cogs.Language.SystemT.TypeCheck as SyST
import qualified Cogs.Language.SystemT.Syntax    as SyST
import qualified Cogs.Language.SystemT.Evaluate  as SyST

import Data.Text (Text)

data Language syn
  = Language
  { parseLang  :: Text -> syn
  , prettyLang :: syn -> Text
  , checkLang  :: syn -> syn
  , evalLang   :: syn -> syn
  }

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
                         Right (Left t) -> Left (SyST.checkClosedTerm t)
                         Right (Right _) -> p
  , evalLang   = \p -> case p of
                         Left _ -> p
                         Right (Left t) -> Right . Right $ SyST.evalClosedTerm t
                         Right (Right _) -> p
  }
