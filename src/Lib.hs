module Lib (
  Vector(..),

  zip,
  zip',

  (.*),
  (.+),

  (.*.),
  (.+.)
) where

import Prelude hiding (zip)

type family Min (a :: Nat) (b :: Nat) where
  Min Zero     _        = Zero
  Min _        Zero     = Zero
  Min (Succ n) (Succ m) = Succ (Min n m)

type family Max (a :: Nat) (b :: Nat) where
  Max Zero     m        = m
  Max n        Zero     = n
  Max (Succ n) (Succ m) = Succ (Max n m)

data Nat = Zero | Succ Nat

infixr 5 :|

data Vector (n :: Nat) a where
  (:|) :: a -> Vector n a -> Vector (Succ n) a
  Nil  :: Vector Zero a

deriving instance Show a => Show (Vector n a)

zip :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zip _ Nil       Nil       = Nil
zip f (a :| as) (b :| bs) = f a b :| zip f as bs

zip' :: (a -> b -> c) -> Vector n a -> Vector m b -> Vector (Min n m) c
zip' _  Nil    _            = Nil
zip' _  _      Nil          = Nil
zip' f  (a :| as) (b :| bs) = f a b :| zip' f as bs

infixr 7 .*

(.*) :: Num a => Vector n a -> Vector n a -> Vector n a
Nil .* Nil = Nil
as  .* bs  = zip (*) as bs

infixr 7 .+

(.+) :: (n ~ Min n n, Num a) => Vector n a -> Vector n a -> Vector n a
Nil .+ Nil = Nil
as  .+ bs  = zip' (+) as bs

infixr 7 .*.

(.*.) :: Num a => Vector n a -> Vector m a -> Vector (Min n m) a
Nil .*. Nil = Nil
as  .*. bs  = zip' (*) as bs

infixr 7 .+.

(.+.) :: Num a => Vector n a -> Vector m a -> Vector (Min n m) a
Nil .+. Nil = Nil
as  .+. bs  = zip' (+) as bs
