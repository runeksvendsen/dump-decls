{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Test.Hspec.Expectations.Pretty (shouldContain, shouldNotBe)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  (stdout, ()) <- System.IO.Silently.capture $
    withArgs ["base", "text"] Exe.main

  declarationMapJson :: [Json.DeclarationMapJson T.Text] <-
      either
        (\e -> fail $ "JSON parse failure: " <> e)
        pure
        (A.eitherDecode (Data.ByteString.Lazy.Char8.pack stdout))

  let mBaseDeclarationMapJson = lookupOn ((== "base") . Json.declarationMapJson_package) declarationMapJson
      baseDeclarationMapJson = fromJust mBaseDeclarationMapJson
  mBaseDeclarationMapJson `shouldNotBe` Nothing

  let tyConIO = Types.FgTyCon
        { Types.fgTyConName = "IO"
        , Types.fgTyConModule = "GHC.Types"
        , Types.fgTyConPackageName = "ghc-prim"
        , Types.fgTyConPackageVersion = "todo"
        }
      tyConString = Types.FgTyCon
        { Types.fgTyConName = "String"
        , Types.fgTyConModule = "GHC.Base"
        , Types.fgTyConPackageName = "base"
        , Types.fgTyConPackageVersion = "todo"
        }
      tyConUnit = Types.FgTyCon
        { Types.fgTyConName = "Unit"
        , Types.fgTyConModule = "GHC.Tuple.Prim"
        , Types.fgTyConPackageName = "ghc-prim"
        , Types.fgTyConPackageVersion = "todo"
        }
      tiPutStrLn =
        Json.TypeInfo
          { Json.typeInfo_fullyQualified = Json.FunctionType
              { Json.functionType_arg =
                  Types.FgType_TyConApp tyConString [] -- String
              , Json.functionType_ret =
                  Types.FgType_TyConApp tyConIO [Types.FgType_TyConApp tyConUnit []] -- IO ()
              } -- FunctionType (FgType (FgTyCon value))
          , Json.typeInfo_tmpUnexpanded = undefined -- FunctionType (FgType (FgTyCon value)) -- ^ TODO: contains type synonyms
          }
  Json.explodeModuleDeclarations (Json.declarationMapJson_moduleDeclarations baseDeclarationMapJson)
    `shouldContain`
      [ ("System.IO", ("putStrLn", tiPutStrLn))
      ]

  pure undefined

lookupOn :: (a -> Bool) -> [a] -> Maybe a
lookupOn _ [] =  Nothing
lookupOn f  (x:xs)
    | f x = Just x
    | otherwise = lookupOn f xs
