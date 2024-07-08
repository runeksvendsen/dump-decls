{-# LANGUAGE CPP #-}
module Compat.Text
( spanEnd
)
where

import qualified Data.Text as T

#if MIN_VERSION_text(2,0,1)
import Data.Functor.Identity (Identity(runIdentity))
#else
import Data.Bifunctor (bimap)
import qualified Data.Tuple
#endif

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
