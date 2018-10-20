{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import           Data.Foldable
import           Data.Functor.Foldable
import qualified Data.Map                      as M
import           Control.Comonad.Cofree
import           Control.Comonad.Trans.Cofree             ( CofreeF )
import qualified Data.Aeson                    as A

data JSON
  = Object (M.Map String JSON)
  | Array [JSON]
  | String String
  | Number Double
  | Bool Bool
  | Null
  deriving (Eq, Show)

data JSONF r
  = ObjectF (M.Map String r)
  | ArrayF [r]
  | StringF String
  | NumberF Double
  | BoolF Bool
  | NullF
  deriving (Show, Eq, Functor)

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

paths :: JSON -> Cofree JSONF [String]
paths = cata addPath
 where
  addPath :: JSONF (Cofree JSONF [String]) -> Cofree JSONF [String]
  addPath (ObjectF o) = [] :< ObjectF (M.mapWithKey addKey o)
    where addKey k v = (k :) <$> v
  addPath (ArrayF a) = [] :< ArrayF (zipWith addIndex a [1 ..])
    where addIndex a i = (show i :) <$> a
  addPath (StringF s) = [] :< StringF s
  addPath (NumberF n) = [] :< NumberF n
  addPath (BoolF   b) = [] :< BoolF b
  addPath NullF       = [] :< NullF
