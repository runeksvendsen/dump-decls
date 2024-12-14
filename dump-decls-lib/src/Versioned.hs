{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module Versioned
( mkVersioned
, versionedVersion
, versionedContent
)
where

import qualified Data.Aeson as A

mkVersioned :: Integer -> a -> Versioned Integer a
mkVersioned = Versioned

versionedVersion :: Versioned version a -> version
versionedVersion = versioned_version

versionedContent :: Versioned version a -> a
versionedContent = versioned_content

data Versioned version a = Versioned
  { versioned_version :: version
  , versioned_content :: a
  }

instance (A.ToJSON version, A.ToJSON a) => A.ToJSON (Versioned version a) where
  toJSON v = A.object
    [ ("version", A.toJSON $ versioned_version v)
    , ("content", A.toJSON $ versioned_content v)
    ]

instance (A.FromJSON version, A.FromJSON a) => A.FromJSON (Versioned version a) where
  parseJSON = A.withObject "Versioned" $ \o ->
    Versioned <$> o A..: "version" <*> o A..: "content"
