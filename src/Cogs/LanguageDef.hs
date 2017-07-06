module Cogs.LanguageDef where

import Data.Text

class Pretty t where
  pretty :: t -> Text

class Parseable t where
  parse :: Text -> t


class (Pretty t) => Eval t where
  evalClosedTerm :: t -> t

class (Pretty t) => Check t where
  checkClosedTerm :: t -> t
