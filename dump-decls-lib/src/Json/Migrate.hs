{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Json.Migrate
( migrateV1ToV2
, migrateV2ToV3
, unversionedJsonFileToV1
)
where

import Json
import Versioned
import Json.Version1 ()
import Json.Version2 ()
import Json.Version3 ()
import qualified Data.Text as T

migrateV1ToV2
  :: FilePath -- old (v1)
  -> FilePath -- new (v2)
  -> IO [DeclarationMapJson T.Text]
migrateV1ToV2 fpOld fpNew =
  versionedJsonMigrateFile (Version :: Version 1, fpOld) (Version :: Version 2, fpNew) >>= \case
    Left e -> fail $ "migrateV1ToV2: failed to parse old version: " <> e
    Right a -> pure a

migrateV2ToV3
  :: FilePath -- old (v2)
  -> FilePath -- new (v3)
  -> IO [DeclarationMapJson T.Text]
migrateV2ToV3 fpOld fpNew =
  versionedJsonMigrateFile (Version :: Version 2, fpOld) (Version :: Version 3, fpNew) >>= \case
    Left e -> fail $ "migrateV2ToV3: failed to parse old version: " <> e
    Right a -> pure a

unversionedJsonFileToV1
  :: FilePath -- unversioned
  -> FilePath -- v1
  -> IO [DeclarationMapJson T.Text]
unversionedJsonFileToV1 fpOld fpNew =
  unversionedJsonFileToVersioned fpOld (Version :: Version 1, fpNew) >>= \case
    Left e -> fail $ "unversionedJsonFileToV1: failed to parse unversioned: " <> e
    Right a -> pure a

