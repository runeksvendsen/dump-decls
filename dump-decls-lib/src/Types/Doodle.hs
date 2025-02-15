module Types.Doodle

where

import Types
import Types.Forall
import Json
import qualified Data.Text as T

type FunctionTypeNoTyVar =
  FunctionType (FgType (FgTyCon T.Text))

data TyConWithVar tyVar text
  = TyCon (FgTyCon text)
  | TyVar (TyVar tyVar)

data FunctionTypeForall tyVar text = FunctionTypeForall
  { ftf_forall :: Forall T.Text
  , ftf_arg :: FgType (TyConWithVar tyVar text)
  , ftf_ret :: FgType (TyConWithVar tyVar text)
  }
