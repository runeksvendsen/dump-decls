{-# LANGUAGE CPP #-}
module Compat.Aeson
( lookup
, Key
, spanEnd
)
where

import Prelude hiding (lookup)

import Data.Aeson (Object, Value)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Lookup
import Data.Aeson.Key (Key)
import qualified Data.Text as T
import Data.Functor.Identity (Identity(runIdentity))
#else
import qualified Data.HashMap.Strict as Lookup
import qualified Data.Text as T
import Data.Bifunctor (bimap)
import qualified Data.Tuple
type Key = T.Text
#endif

lookup :: Key -> Object -> Maybe Value
lookup = Lookup.lookup

spanEnd
  :: (Char -> Bool)
  -> T.Text
  -> (T.Text, T.Text)
spanEnd p =
#if MIN_VERSION_text(2,0,1)
  runIdentity . T.spanEndM (pure . p)
#else
  Data.Tuple.swap . bimap T.reverse T.reverse . T.span p . T.reverse
#endif
