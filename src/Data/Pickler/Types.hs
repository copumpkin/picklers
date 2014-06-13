{-# LANGUAGE GADTs, PolyKinds, DataKinds, TypeFamilies, KindSignatures, TypeOperators, ImplicitParams, ViewPatterns #-}
module Data.Pickler.Types where

import Data.Binary.Get
import Data.Binary.Builder

data HList :: [*] -> * where
  Nil  :: HList '[]
  (:>) :: t -> HList ts -> HList (t ': ts)

type family MapT (f :: a -> b) (xs :: [a]) :: [b] where
  MapT f '[] = '[]
  MapT f (a ': as) = f a ': MapT f as

type All f as = HList (MapT f as)

data Partition :: [k] -> [k] -> [k] -> * where
  End :: Partition '[] '[] '[]
  L   :: Partition xs ys zs -> Partition (x ': xs) (x ': ys) zs
  R   :: Partition xs ys zs -> Partition (x ': xs) ys (x ': zs)

type Put a = a -> Builder

data Pickler ts t = Pickler
  { get  :: HList ts -> Get t
  , put  :: t -> (HList ts, Builder)
  --, size :: Size -- Eventually track upper and lower bounds to structure size
  }

type Pickle = Pickler '[]











headH :: HList (t ': ts) -> t
headH (x :> xs) = x


partition :: Partition as bs cs -> HList as -> (HList bs, HList cs)
partition End Nil = (Nil, Nil)
partition (L p) (a :> (partition p -> (ls, rs))) = (a :> ls, rs)
partition (R p) (a :> (partition p -> (ls, rs))) = (ls, a :> rs)

assemble :: Partition as bs cs -> HList bs -> HList cs -> HList as
assemble End Nil Nil = Nil
assemble (L p) (l :> ls) rs = l :> assemble p ls rs
assemble (R p) ls (r :> rs) = r :> assemble p ls rs
