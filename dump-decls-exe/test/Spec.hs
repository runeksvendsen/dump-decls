{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import qualified Exe
import qualified Json
import qualified Types
import System.Environment (withArgs)
import qualified System.IO.Silently
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text as T
import qualified Test.Hspec
import Test.Hspec.Expectations.Pretty (shouldNotBe, shouldBe)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

main :: IO ()
main = do
  (stdout, ()) <- System.IO.Silently.capture $
    withArgs ["/nix/store/icq948yaf8v17a464ciz38czigq0vccb-ghc-9.6.2/lib/ghc-9.6.2/lib", "base", "text"] Exe.main -- WIP!
  let stdoutBs = Data.ByteString.Lazy.Char8.pack stdout
  declarationMapJson :: [Json.DeclarationMapJson T.Text] <-
      either
        (\e -> fail $ "JSON parse failure: " <> e)
        pure
        (A.eitherDecode stdoutBs)
  Test.Hspec.hspec $ spec declarationMapJson

spec :: [Json.DeclarationMapJson T.Text] -> Test.Hspec.Spec
spec declarationMapJson =
  Test.Hspec.describe "Expected TypeInfo" $ do
    specPutStrLn declarationMapJson
    specUnsnoc declarationMapJson

-- | System.IO.putStrLn :: String -> IO ()
specPutStrLn :: [Json.DeclarationMapJson T.Text] -> Test.Hspec.Spec
specPutStrLn =
    mkSpec "base" "System.IO" "putStrLn" ftPutStrLn
  where
    tyConIO = parsePprTyCon "ghc-prim-0.10.0:GHC.Types.IO"
    tyConString = parsePprTyCon "base-4.18.0.0:GHC.Base.String"
    tyConAppIOUnit = Types.FgType_TyConApp tyConIO [Types.FgType_Unit Types.Boxed] -- IO ()

    ftPutStrLn = Json.FunctionType
            { Json.functionType_arg = Types.FgType_List $ Just $ Types.FgType_TyConApp tyConChar [] -- [Char]
            , Json.functionType_ret = tyConAppIOUnit
            }
        -- , Json.typeInfo_unexpanded = Json.FunctionType
        --     { Json.functionType_arg = Types.FgType_TyConApp tyConString [] -- String
        --     , Json.functionType_ret = tyConAppIOUnit
        --     }
        -- }

-- | Data.Text.unsnoc :: Text -> Maybe (Text, Char)
specUnsnoc :: [Json.DeclarationMapJson T.Text] -> Test.Hspec.Spec
specUnsnoc =
    mkSpec "text" "Data.Text" "unsnoc" funtionType
  where
    tyConText = parsePprTyCon "text-2.0.2:Data.Text.Internal.Text"
    fgTypeText = Types.FgType_TyConApp tyConText []
    fgTypeChar = Types.FgType_TyConApp tyConChar []
    tyConMaybe = parsePprTyCon "base-4.18.0.0:GHC.Maybe.Maybe"

    funtionType = Json.FunctionType
      { Json.functionType_arg = fgTypeText
      , Json.functionType_ret = Types.FgType_TyConApp
          tyConMaybe
          [Types.FgType_Tuple Types.Boxed 2 [fgTypeText, fgTypeChar]]
      }

mkSpec
  :: T.Text -- Package with version (e.g. @base-4.18.0.0@)
  -> T.Text -- Module name (e.g. @System.IO@)
  -> T.Text -- Definition name (e.g. @putStrLn@)
  -> Json.FunctionType (Types.FgType (Types.FgTyCon T.Text))
  -> [Json.DeclarationMapJson T.Text]
  -> Test.Hspec.Spec
mkSpec pkgName modName defnName expected declarationMapJson =
  Test.Hspec.it (T.unpack $ modName <> "." <> defnName) $ do
    let mBaseDeclarationMapJson = lookupOn ((== pkgName) . Types.fgPackageName . Json.declarationMapJson_package) declarationMapJson
        baseDeclarationMapJson = fromJust mBaseDeclarationMapJson
    mBaseDeclarationMapJson `shouldNotBe` Nothing
    let modDecls = Json.declarationMapJson_moduleDeclarations baseDeclarationMapJson
        map' = Json.moduleDeclarations_map modDecls
        mDefnMap = Map.lookup modName map'
        mTypeInfo = mDefnMap >>= Map.lookup defnName
        typeInfo = fromJust mTypeInfo
    mTypeInfo `shouldNotBe` Nothing
    IgnorePackageVersion typeInfo `shouldBe` IgnorePackageVersion expected

parsePprTyCon :: T.Text -> Types.FgTyCon T.Text
parsePprTyCon = either error id . Types.parsePprTyCon

lookupOn :: (a -> Bool) -> [a] -> Maybe a
lookupOn _ [] =  Nothing
lookupOn f  (x:xs)
    | f x = Just x
    | otherwise = lookupOn f xs

tyConChar :: Types.FgTyCon T.Text
tyConChar = parsePprTyCon "ghc-prim-0.10.0:GHC.Types.Char"

newtype IgnorePackageVersion a = IgnorePackageVersion a
  deriving (Show)

instance Eq (IgnorePackageVersion (Json.FunctionType (Types.FgType (Types.FgTyCon T.Text)))) where
  IgnorePackageVersion ti1 == IgnorePackageVersion ti2 =
    let strikePkgVersionFgPackage pkg = pkg { Types.fgPackageVersion = "" }

        strikePkgVersionFgTyCon tc = tc {
            Types.fgTyConPackage = strikePkgVersionFgPackage (Types.fgTyConPackage tc)
          }

        strikePkgVersionFgType fgt = fmap strikePkgVersionFgTyCon fgt

        strikePkgVersionTypeInfo ti = fmap strikePkgVersionFgType ti

    in strikePkgVersionTypeInfo ti1 == strikePkgVersionTypeInfo ti2
