{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import qualified Data.Aeson                    as A
import           Data.Foldable
import           Data.Functor.Foldable
import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as T
import qualified Data.Vector                   as V

data ValueF r
  = ObjectF (M.HashMap T.Text r)
  | ArrayF [r]
  | StringF T.Text
  | NumberF Double
  | BoolF Bool
  | NullF
  deriving (Functor)

instance A.FromJSON (Fix ValueF) where
  parseJSON = return . refix

instance A.ToJSON (Fix ValueF) where
  toJSON = refix


type instance Base A.Value = ValueF

instance Recursive A.Value where
  project (A.Object obj) = ObjectF obj
  project (A.Array  arr) = ArrayF $ toList arr
  project (A.String s  ) = StringF s
  project (A.Number n  ) = NumberF doubleN
    where doubleN = fromRational . toRational $ n
  project (A.Bool b) = BoolF b
  project A.Null     = NullF

instance Corecursive A.Value where
  embed (ObjectF o) = A.Object o
  embed (ArrayF  a) = A.Array $ V.fromList a
  embed (StringF s) = A.String s
  embed (NumberF n) = (A.Number scientificN)
    where scientificN = fromRational . toRational $ n
  embed (BoolF b) = (A.Bool b)
  embed NullF     = A.Null

