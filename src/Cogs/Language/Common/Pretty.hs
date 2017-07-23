{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.Common.Pretty where

import Cogs.Language.Common.Syntax
import Data.Monoid
import Data.Text

--------------------------------------------------------------------------------
--                                  PRETTY                                    --
--------------------------------------------------------------------------------

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b
