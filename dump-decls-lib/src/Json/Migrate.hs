{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Json.Migrate
( migrateV1ToV1
, unversionedJsonFileToV1
)
where

import Json
import Versioned
import Json.Version1 ()
import qualified Data.Text as T

-- | TODO: No-op. Rewrite once a v2 exists
migrateV1ToV1
  :: FilePath -- old (v1)
  -> FilePath -- new (v1)
  -> IO [DeclarationMapJson T.Text]
migrateV1ToV1 fpOld fpNew =
  versionedJsonMigrateFile (Version :: Version 1, fpOld) (Version :: Version 1, fpNew) >>= \case
    Left e -> fail $ "migrateV1ToV2: failed to parse old version: " <> e
    Right a -> pure a

unversionedJsonFileToV1
  :: FilePath -- unversioned
  -> FilePath -- v1
  -> IO [DeclarationMapJson T.Text]
unversionedJsonFileToV1 fpOld fpNew =
  unversionedJsonFileToVersioned fpOld (Version :: Version 1, fpNew) >>= \case
    Left e -> fail $ "unversionedJsonFileToV1: failed to parse unversioned: " <> e
    Right a -> pure a

