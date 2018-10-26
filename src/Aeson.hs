{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Aeson where

import qualified Data.Aeson                    as A
import           Data.Functor.Foldable
import           JSON
import qualified Data.Vector                   as V
import qualified Data.Map                      as M
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Data.Bifunctor

instance A.ToJSON (Fix JSONF) where
  toJSON = refix

instance A.FromJSON (Fix JSONF) where
  parseJSON = return . refix

instance A.ToJSON JSON where
  toJSON = refix

instance A.FromJSON JSON where
  parseJSON = return . refix


type instance Base A.Value = JSONF

instance Recursive A.Value where
  project (A.Object obj) = ObjectF (M.fromList . fmap (first T.unpack) . HM.toList $ obj)
  project (A.Array  arr) = ArrayF $ V.toList arr
  project (A.String s  ) = StringF $ T.unpack s
  project (A.Number n  ) = NumberF doubleN
    where doubleN = fromRational . toRational $ n
  project (A.Bool b) = BoolF b
  project A.Null     = NullF

instance Corecursive A.Value where
  embed (ObjectF obj) = A.Object (HM.fromList . fmap (first T.pack) . M.toList $ obj)
  embed (ArrayF  arr) = A.Array $ V.fromList arr
  embed (StringF s) = A.String (T.pack s)
  embed (NumberF n) = (A.Number scientificN)
    where scientificN = fromRational . toRational $ n
  embed (BoolF b) = (A.Bool b)
  embed NullF     = A.Null
