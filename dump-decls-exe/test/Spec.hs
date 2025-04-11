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
import qualified Types.Doodle
import Data.Bifunctor (first)
import qualified Types.FunctionInfo as FunctionInfo

main :: IO ()
main = do
  (stdout, ()) <- System.IO.Silently.capture $
    withArgs ["/nix/store/icq948yaf8v17a464ciz38czigq0vccb-ghc-9.6.2/lib/ghc-9.6.2/lib", "base", "text"] Exe.main -- WIP!
  let stdoutBs = Data.ByteString.Lazy.Char8.pack stdout
  declarationMapJson :: Exe.StdoutJsonFormat <-
      either
        (\e -> fail $ "JSON parse failure: " <> e)
        pure
        (A.eitherDecode stdoutBs)
  Test.Hspec.hspec $ spec declarationMapJson

spec :: [Json.DeclarationMapJson T.Text] -> Test.Hspec.Spec
spec declarationMapJson =
  Test.Hspec.describe "Expected FunctionType" $ do
    specPutStrLn declarationMapJson
    specUnsnoc declarationMapJson

-- | System.IO.putStrLn :: String -> IO ()
specPutStrLn :: [Json.DeclarationMapJson T.Text] -> Test.Hspec.Spec
specPutStrLn =
    mkSpec "base" "System.IO" "putStrLn" ftPutStrLn
  where
    tyConIO = parsePprTyCon "ghc-prim-0.10.0:GHC.Types.IO"
    tyConAppIOUnit = Types.FgType_TyConApp tyConIO [Types.FgType_Unit Types.Boxed] -- IO ()

    ftPutStrLn =
      Types.FunctionType
        { Types.functionType_arg = Types.FgType_List $ Just $ Types.FgType_TyConApp tyConChar [] -- [Char]
        , Types.functionType_ret = tyConAppIOUnit
        }

-- | Data.Text.unsnoc :: Text -> Maybe (Text, Char)
specUnsnoc :: [Json.DeclarationMapJson T.Text] -> Test.Hspec.Spec
specUnsnoc =
    mkSpec "text" "Data.Text" "unsnoc" funtionType
  where
    tyConText = parsePprTyCon "text-2.0.2:Data.Text.Internal.Text"
    fgTypeText = Types.FgType_TyConApp tyConText []
    fgTypeChar = Types.FgType_TyConApp tyConChar []
    tyConMaybe = parsePprTyCon "base-4.18.0.0:GHC.Maybe.Maybe"

    funtionType = Types.FunctionType
      { Types.functionType_arg = fgTypeText
      , Types.functionType_ret = Types.FgType_TyConApp
          tyConMaybe
          [Types.FgType_Tuple Types.Boxed 2 [fgTypeText, fgTypeChar]]
      }

mkSpec
  :: T.Text -- Package with version (e.g. @base-4.18.0.0@)
  -> T.Text -- Module name (e.g. @System.IO@)
  -> T.Text -- Definition name (e.g. @putStrLn@)
  -> Types.FunctionType (Types.FgType (Types.FgTyCon T.Text))
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
    IgnorePackageVersion (FunctionInfo.functionInfo_function typeInfo) `shouldBe` IgnorePackageVersion (Types.Doodle.SomeFunction_Monomorphic expected)

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

instance Eq (IgnorePackageVersion (Types.FunctionType (Types.FgType (Types.FgTyCon T.Text)))) where
  IgnorePackageVersion ti1 == IgnorePackageVersion ti2 =
    let strikePkgVersionFgPackage pkg = pkg { Types.fgPackageVersion = "" }

        strikePkgVersionFgTyCon tc = tc {
            Types.fgTyConPackage = strikePkgVersionFgPackage (Types.fgTyConPackage tc)
          }

        strikePkgVersionFgType fgt = fmap strikePkgVersionFgTyCon fgt

        strikePkgVersionTypeInfo ti = fmap strikePkgVersionFgType ti

    in strikePkgVersionTypeInfo ti1 == strikePkgVersionTypeInfo ti2

instance Eq (IgnorePackageVersion (Types.Doodle.FunctionTypeForall T.Text T.Text)) where
  IgnorePackageVersion ftf1 == IgnorePackageVersion ftf2 =
    let strikePkgVersionFgPackage pkg = pkg { Types.fgPackageVersion = "" }

        strikePkgVersionFgTyCon tc = tc {
            Types.fgTyConPackage = strikePkgVersionFgPackage (Types.fgTyConPackage tc)
          }

        strikePkgVersionFgType fgt = fmap (first strikePkgVersionFgTyCon) fgt

        strikePkgVersionFtf ftf =
          ftf
            { Types.Doodle.ftf_arg = strikePkgVersionFgType (Types.Doodle.ftf_arg ftf)
            , Types.Doodle.ftf_ret = strikePkgVersionFgType (Types.Doodle.ftf_ret ftf)
            }

    in strikePkgVersionFtf ftf1 == strikePkgVersionFtf ftf2

instance Eq (IgnorePackageVersion Types.Doodle.SomeFunction) where
  IgnorePackageVersion sf1 == IgnorePackageVersion sf2 = case (sf1, sf2) of
    (Types.Doodle.SomeFunction_Monomorphic sf1', Types.Doodle.SomeFunction_Monomorphic sf2') ->
      IgnorePackageVersion sf1' == IgnorePackageVersion sf2'
    (Types.Doodle.SomeFunction_Polymorphic sf1', Types.Doodle.SomeFunction_Polymorphic sf2') ->
      IgnorePackageVersion sf1' == IgnorePackageVersion sf2'
    _ -> False
