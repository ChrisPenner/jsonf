{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module JSON where

import           Data.Foldable
import           Data.Functor.Foldable
import qualified Data.Map                      as M
import           Control.Comonad.Cofree
import           Control.Comonad.Trans.Cofree             ( CofreeF )
import qualified Data.Aeson                    as A
import           Text.Show.Deriving

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

deriveShow1 ''JSONF

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
