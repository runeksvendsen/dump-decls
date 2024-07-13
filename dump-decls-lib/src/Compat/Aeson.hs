{-# LANGUAGE CPP #-}
module Compat.Aeson
( lookup
, Key
)
where

import Prelude hiding (lookup)
import Data.Aeson (Object, Value)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Lookup
import Data.Aeson.Key (Key)
#else
import qualified Data.HashMap.Strict as Lookup
import qualified Data.Text as T
type Key = T.Text
#endif

lookup :: Key -> Object -> Maybe Value
lookup = Lookup.lookup
