{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Json.Migrate
( migrateV3ToV4
, unversionedJsonFileToV3
)
where

import Json
import Versioned
import Json.Version3 ()
import Json.Version4 ()
import qualified Data.Text as T

migrateV3ToV4
  :: FilePath -- old (v3)
  -> FilePath -- new (v4)
  -> IO [DeclarationMapJson T.Text]
migrateV3ToV4 fpOld fpNew =
  versionedJsonMigrateFile (Version :: Version 3, fpOld) (Version :: Version 4, fpNew) >>= \case
    Left e -> fail $ "migrateV1ToV2: failed to parse old version: " <> e
    Right a -> pure a

unversionedJsonFileToV3
  :: FilePath -- unversioned
  -> FilePath -- v3
  -> IO [DeclarationMapJson T.Text]
unversionedJsonFileToV3 fpOld fpNew =
  unversionedJsonFileToVersioned fpOld (Version :: Version 3, fpNew) >>= \case
    Left e -> fail $ "unversionedJsonFileToV3: failed to parse unversioned: " <> e
    Right a -> pure a

