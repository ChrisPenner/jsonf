{-# LANGUAGE RankNTypes #-}
module Lists.SimpleFuncs where


data ListF a r = Cons a r | Nil

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

newtype Mu f = Mu (forall a. Algebra f a -> a)
newtype Nu f = Nu (forall a. CoAlgebra f a -> a -> f (Nu f))

cata :: Algebra f a -> Mu f -> a
cata alg (Mu n) = n alg

-- ana :: CoAlgebra f a -> a -> Mu f
-- ana coalg a = Mu (\alg -> alg $ coalg a)
