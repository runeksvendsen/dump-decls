module Types.Doodle

where

import Types
import Types.Forall
import Json
import qualified Data.Text as T

type FunctionTypeNoTyVar =
  FunctionType (FgType (FgTyCon T.Text))

data FunctionTypeForall tyVar text = FunctionTypeForall
  { ftf_forall :: Forall T.Text
  , ftf_arg :: FgType (Either (FgTyCon text) (TyVar tyVar))
  , ftf_ret :: FgType (Either (FgTyCon text) (TyVar tyVar))
  }
