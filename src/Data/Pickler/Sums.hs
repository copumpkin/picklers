{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ExistentialQuantification #-} 
module Data.Pickler.Sums where

import Control.Applicative

import Data.Maybe
import Data.Monoid hiding (Sum, Product, All)
import Data.Binary.Get hiding (Done, Partial, skip)
import qualified Data.Binary.Get as Get
import Data.Binary.Builder
import Data.Binary.IEEE754
import qualified Data.Map as M
import Data.ByteString.Base16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Generic as G
import Control.Monad
import Debug.Trace

import Data.Int
import Data.Word

newtype (:$:) f a = F { unF :: f a }

newtype K a b = K { unK :: a }
data (:*:) f g x = f x :*: g x
data Exists f = forall a. Exists { getValue :: f a }

newtype (:<-)  r t  = Flip  { runFlip  :: t   -> r }
newtype (:<-@) r ts = NFlip { runNFlip :: ts @-> r }

type family (@->) (ts :: [*]) (r :: *) :: *
type instance '[] @-> r = r
type instance (t ': ts) @-> r = t -> (ts @-> r)

type family Eliminator (css :: [[*]]) (r :: *) :: *
type instance Eliminator '[] r = r
type instance Eliminator (cs ': css) r = (cs @-> r) -> Eliminator css r



infixr 1 :>

data All f (ts :: [a]) where
  Nil  :: All f '[]
  (:>) :: f x -> All f xs -> All f (x ': xs)

mapAll :: (forall a. f a -> g a) -> All f ts -> All g ts
mapAll f Nil = Nil
mapAll f (a :> as) = f a :> mapAll f as

mapAllF :: (forall a. f a -> b) -> All f ts -> [b]
mapAllF f Nil = []
mapAllF f (a :> as) = f a : mapAllF f as

foldlAll :: (a -> b -> a) -> a -> All ((:<-) b) ts -> (ts @-> a)
foldlAll f z Nil = z
foldlAll f z (Flip g :> gs) = \x -> foldlAll f (f z (g x)) gs

zipWithAll :: (forall a. f a -> g a -> h a) -> All f ts -> All g ts -> All h ts
zipWithAll f Nil Nil = Nil
zipWithAll f (x :> xs) (y :> ys) = f x y :> zipWithAll f xs ys

-- All glory to glguy
apN :: Applicative f => f (ts @-> r) -> All f ts -> f r
apN f Nil = f
apN f (x :> xs) = apN (f <*> x) xs

liftAn :: Applicative f => (ts @-> r) -> All f ts -> f r
liftAn = apN . pure

merge :: All ((:<-@) r) ts -> Eliminator ts r -> r
merge Nil acc = acc
merge (NFlip x :> xs) acc = merge xs (acc x)


-- incremental All? When building, we know the argument and can compute the rest, when reading, we know the value and can compute the rest

data Partial g p a ts = Partial { build :: !(g a), break :: !(ts @-> p), make :: !(ts @-> a) } -- forall xs. uncurryN make xs == runGet build . toLazyByteString $ uncurryN break xs

newtype EliminatorWrapper ts r = EliminatorWrapper { getEliminator :: Eliminator ts r }

alt :: (ts @-> a) -> All Prickler ts -> Partial Get Put a ts
alt cons shape = Partial builder breaker cons
  where
  builder = liftAn cons (mapAll get shape)
  breaker = foldlAll (<>) mempty (mapAll (Flip . put) shape)












data State a = State { written :: {-# UNPACK #-} !Int64, builder :: !Builder, contents :: a }

instance Functor State where
  fmap f (State w b v) = State w b (f v)

newtype PutM a = PutM { runPutM :: Int64 -> State a }

instance Functor PutM where
  fmap f (PutM g) = PutM (fmap f . g)

instance Applicative PutM where
  pure x = PutM (\w -> State w mempty x)
  PutM f <*> PutM x = PutM $ \w -> case f w of State w1 b1 f' -> case x w1 of State w2 b2 x' -> State w2 (b1 <> b2) (f' x')

instance Monad PutM where
  return = pure
  PutM x >>= f = PutM (\w -> case x w of State w1 b1 x' -> case f x' of PutM f' -> case f' w1 of State w2 b2 x'' -> State w2 (b1 <> b2) x'')

type Put = PutM ()

instance Monoid Put where
  mempty = pure ()
  mappend = (>>)

data Prickler a = Prickler { get :: Get a, put :: a -> Put } -- laws: get . put . get == get, put . get . put == put

expect :: Eq a => a -> Prickler a -> Prickler ()
expect x (Prickler ga pa) = Prickler (do y <- ga; if x == y then return () else fail "Expectation failed") (const (pa x))

ignore :: a -> Prickler a -> Prickler ()
ignore x (Prickler ga pa) = Prickler (() <$ ga) (const (pa x))

skip :: Prickler () -> Prickler a -> Prickler a
skip (Prickler gu pu) (Prickler ga pa) = Prickler (gu *> ga) ((pu () <>) . pa)

wrap :: (a -> b) -> (b -> a) -> Prickler a -> Prickler b
wrap f g (Prickler ga pa) = Prickler (f <$> ga) (pa . g)

pair :: Prickler a -> Prickler b -> Prickler (a, b)
pair (Prickler ga pa) (Prickler gb pb) = Prickler (liftA2 (,) ga gb) (\(x, y) -> pa x <> pb y)

type Contiguous = (:$:)
type Indexed i = (:*:) (K i)


untagged :: (forall r. a -> (ts @-> r) -> r) -> Partial Get Put a ts -> Prickler a
untagged elim (Partial build break _) = Prickler build (flip elim break)


delimitedPartial :: Integral i => Prickler i -> Partial Get Put a ts -> Partial Get Put a ts
delimitedPartial (Prickler gi pi) (Partial maker breaker cons) = Partial maker' breaker' cons
  where
  --maker' :: Get a -> Get a
  maker' = do
    len <- gi
    bs <- getLazyByteString (fromIntegral len)
    return $ runGet maker bs

  breaker' = undefined

tagged :: Ord i => Prickler i -> (forall r. a -> EliminatorWrapper ts r) -> All (Indexed i (Partial Get Put a)) ts -> Prickler a
tagged (Prickler gi pi) elim sum = Prickler getter (merged (mapAll (adjust pi) sum) . elim)
  where
  merged :: All ((:<-@) r) ts -> EliminatorWrapper ts r -> r
  merged rs (EliminatorWrapper elim) = merge rs elim

  adjust :: (i -> Put) -> (K i :*: Partial Get Put a) ts -> (Put :<-@ ts)
  adjust pi (K i :*: Partial _ breaker _) = NFlip breaker


  ps = M.fromList (mapAllF (\(K x :*: y) -> (x, Exists y)) sum)

  getter = do
    tag <- gi
    case M.lookup tag ps of
      Nothing                           -> fail $ "Invalid tag!"
      Just (Exists (Partial maker _ _)) -> maker


taggedSized :: (Ord i, Integral s) => Prickler i -> Prickler s -> (forall r. a -> EliminatorWrapper ts r) -> All (Indexed i (Partial Get Put a)) ts -> Prickler a
taggedSized (Prickler gi pi) (Prickler gs ps) elim sum = Prickler getter (merged (mapAll (adjust pi) sum) . elim)
  where
  merged :: All ((:<-@) r) ts -> EliminatorWrapper ts r -> r
  merged rs (EliminatorWrapper elim) = merge rs elim

  adjust :: (i -> Put) -> (K i :*: Partial Get Put a) ts -> (Put :<-@ ts)
  adjust pi (K i :*: Partial _ breaker _) = NFlip breaker


  ps = M.fromList (mapAllF (\(K x :*: y) -> (x, Exists y)) sum)

  getter = do
    tag  <- gi
    size <- gs
    case M.lookup tag ps of
      Nothing                           -> fail $ "Invalid tag!"
      Just (Exists (Partial maker _ _)) -> do
        bs <- getLazyByteString (fromIntegral size)
        return $ runGet maker bs

