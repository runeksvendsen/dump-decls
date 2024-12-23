{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Versioned
( mkVersioned
, versionedVersionInteger
, versionedContent
, versionedVersion
, Versioned
, Version(Version)
  -- * JSON
, versionedToJSON
, versionedParseJSON
, versionedJsonMigrateFile
, unversionedJsonFileToVersioned
  -- * Re-exports
, Nat, KnownNat
)
where

import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified Data.Aeson as A
import Data.Data (Proxy(Proxy))
import Control.Monad (unless)
import qualified Data.Aeson.Types as A
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

mkVersioned
  :: a
  -> Versioned version a
mkVersioned = Versioned

versionedVersionInteger
  :: forall version a.
     KnownNat version
  => Versioned version a
  -> Integer
versionedVersionInteger _ = natVal (Proxy :: Proxy version)

versionedVersion
  :: forall version a.
     Versioned version a
  -> Version version
versionedVersion _ = Version

versionedContent
  :: Versioned version a
  -> a
versionedContent = versioned_content

-- | A particular version
data Version (version :: Nat) = Version

-- | Some type @a@ of a particular version @version@.
newtype Versioned (version :: Nat) a =
  Versioned { versioned_content :: a }
    deriving (Eq, Ord, Show, Functor, Generic)

instance (NFData a) => NFData (Versioned version a)

versionedToJSON
  :: KnownNat version
  => (a -> A.Value)
  -> Versioned version a
  -> A.Value
versionedToJSON contentToJSON v =
  A.object
    [ ("version", A.toJSON $ versionedVersionInteger v)
    , ("content", contentToJSON $ versioned_content v)
    ]

versionedParseJSON
  :: forall version a.
     KnownNat version
  => (A.Value -> A.Parser a)
  -> A.Value
  -> A.Parser (Versioned version a)
versionedParseJSON contentParseJSON =
  A.withObject "Versioned" $ \o -> do
    !version <- o A..: "version"
    let expectedVersion = natVal (Proxy :: Proxy version)
    unless (version == expectedVersion) $
      fail $ "Version mismatch: expected " <> show expectedVersion <> " found " <> show version
    !content <- o A..: "content" >>= contentParseJSON
    pure $! Versioned content

versionedJsonMigrateFile
  :: forall a oldVersion newVersion.
     ( A.FromJSON (Versioned oldVersion a)
     , A.ToJSON (Versioned newVersion a)
     )
  => (Version oldVersion, FilePath) -- old version
  -> (Version newVersion, FilePath) -- new version
  -> IO (Either String a) -- either a decoding error or the decoded content
versionedJsonMigrateFile (_, fpOld) (_, fpNew) = do
  A.eitherDecodeFileStrict fpOld >>= \case
    Left e ->
      pure $ Left e
    Right !oldVersion -> do
      let newVersion = coerce oldVersion
      A.encodeFile fpNew newVersion
      pure $! Right $! versionedContent newVersion
  where
    coerce :: Versioned oldVersion a -> Versioned newVersion a
    coerce = mkVersioned . versionedContent

unversionedJsonFileToVersioned
  :: forall a version.
     ( A.FromJSON a
     , A.ToJSON (Versioned version a)
     )
  => FilePath -- unversioned file
  -> (Version version, FilePath) -- versioned file
  -> IO (Either String a) -- either a decoding error or the decoded unversioned content
unversionedJsonFileToVersioned fpOld (_, fpNew) = do
  A.eitherDecodeFileStrict fpOld >>= \case
    Left e ->
      pure $ Left e
    Right !unversioned -> do
      let versioned :: Versioned version a
          versioned = mkVersioned unversioned
      A.encodeFile fpNew versioned
      pure $ Right unversioned
