{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import           Data.Foldable
import           Data.Functor.Foldable
import qualified Data.Map                      as M

data JSON
  = Object (M.Map String JSON)
  | Array [JSON]
  | String String
  | Number Double
  | Bool Bool
  | Null

data JSONF r
  = ObjectF (M.Map String r)
  | ArrayF [r]
  | StringF String
  | NumberF Double
  | BoolF Bool
  | NullF
  deriving (Functor)

type instance Base JSON = JSONF

instance Recursive JSON where
  project (Object obj) = ObjectF obj
  project (Array  arr) = ArrayF arr
  project (String s  ) = StringF s
  project (Number n  ) = NumberF n
  project (Bool b) = BoolF b
  project Null     = NullF

instance Corecursive JSON where
  embed (ObjectF o) = Object o
  embed (ArrayF  a) = Array a
  embed (StringF s) = String s
  embed (NumberF n) = (Number n)
  embed (BoolF b) = (Bool b)
  embed NullF     = Null
