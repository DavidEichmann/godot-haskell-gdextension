{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StringInterp (StringInterp (..)) where

import Data.String (IsString (..))
import Data.Text
import GHC.TypeLits as GHC
import Prelude hiding (show)

class StringInterp a where
  _' :: Text -> a

instance StringInterp Text where
  _' t = t

instance {-# OVERLAPPABLE #-} (GHC.TypeError (GHC.Text "First and last args must be Text")) => StringInterp (a -> rest) where
  _' = undefined

instance (ShowNoQuote a, StringInterp rest, t ~ Text) => StringInterp (a -> t -> rest) where
  _' accum a t = _' (accum <> showNoQuote a <> t)

class ShowNoQuote a where
  showNoQuote :: a -> Text

instance ShowNoQuote Text where
  showNoQuote = id

instance ShowNoQuote String where
  showNoQuote = fromString

instance {-# OVERLAPPABLE #-} (Show a) => ShowNoQuote a where
  showNoQuote = show

instance (StringInterp (a -> t -> rest)) => IsString (a -> t -> rest) where
  fromString str = _' (fromString str :: Text)