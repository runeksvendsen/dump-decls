{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Types.FunctionInfo
( FunctionInfo
, mkFunctionInfo
, functionInfo_function
, functionInfo_unique
)
where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Control.DeepSeq (NFData)

-- | Information about a function that's not related to its type
data FunctionInfo fun = FunctionInfo
  { functionInfo_unique :: {-# UNPACK #-} Int
    -- ^ todo
  , functionInfo_function :: fun

  } deriving (Eq, Show, Ord, Foldable, Generic, Functor)

instance (A.ToJSON a) => A.ToJSON (FunctionInfo a)
instance (A.FromJSON a) => A.FromJSON (FunctionInfo a)
instance (NFData a) => NFData (FunctionInfo a)

instance Traversable FunctionInfo where
  traverse f (FunctionInfo unique fun) =
    FunctionInfo unique <$> f fun

mkFunctionInfo :: Int -> fun -> FunctionInfo fun
mkFunctionInfo = FunctionInfo
