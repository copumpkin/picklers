{-# LANGUAGE GADTs, PolyKinds, DataKinds, KindSignatures, TypeOperators, ImplicitParams, ViewPatterns #-}
module Data.Pickler where

import Data.Int
import Data.Word
import Data.List hiding (partition)
import Data.Monoid
import Data.Binary.Get
import Data.Binary.Put (execPut)
import Data.Binary.Builder
import Data.Binary.IEEE754
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Generic as G

import Control.Applicative
import Control.Monad

import Data.Pickler.Types

pickle :: Get a -> Put a -> Pickle a
pickle ga pa = Pickler (const ga) (\x -> (Nil, pa x))

xmap :: (a -> b) -> (b -> a) -> Pickler ts a -> Pickler ts b
xmap f f' (Pickler g p) = Pickler ((f <$>) . g) (p . f')



-----------------
-- Combinators
-----------------

gpair :: Partition ts us vs -> Pickler us a -> Pickler vs b -> Pickler ts (a, b)
gpair pt (Pickler ga pa) (Pickler gb pb) = Pickler g p
  where
  g (partition pt -> (ls, rs)) = liftA2 (,) (ga ls) (gb rs)
  p (pa -> (ca, ba), pb -> (cb, bb)) = (assemble pt ca cb, ba <> bb)


pair :: Pickle a -> Pickle b -> Pickle (a, b)
pair = gpair End


-- Should I just use a two-element type?
either :: Integral i => Pickler ts l -> Pickler ts r -> Pickler (i ': ts) (Either l r)
either (Pickler gl pl) (Pickler gr pr) = Pickler g p
  where
  g (0 :> cs) = Left  <$> gl cs
  g (1 :> cs) = Right <$> gr cs
  p (Left  l) = case pl l of (cs, b) -> (0 :> cs, b)
  p (Right r) = case pr r of (cs, b) -> (1 :> cs, b)



expectMsg :: Eq a => a -> (a -> String) -> Pickler ts a -> Pickler ts ()
expectMsg x msg (Pickler g p) = Pickler g' (const (p x))
  where g' cs = do y <- g cs; unless (x == y) (fail (msg y))

expect :: (Eq a, Show a) => a -> Pickler ts a -> Pickler ts ()
expect x = expectMsg x (\y -> "Expected " ++ show x ++ "; got " ++ show y)

ignore :: a -> Pickler ts a -> Pickler ts ()
ignore x (Pickler g p) = Pickler (void . g) (const (p x))


integral :: (Integral i, Integral j) => Pickler ts i -> Pickler ts j
integral = xmap fromIntegral fromIntegral 






-- Polymorphic containers

list :: Integral i => Pickle a -> Pickler '[i] [a]
list (Pickler g p) = Pickler g' p'
  where
  g' (l :> cs) = replicateM (fromIntegral l) (g cs)
  p' xs = (fromIntegral (length xs) :> Nil, foldl' (\acc q -> acc <> snd (p q)) mempty xs)


-- I'd like the inner pickler to be able to carry context, but what if
-- context differs for different inner elements?
vector :: (G.Vector v a, Integral i) => Pickle a -> Pickler '[i] (v a)
vector (Pickler g p) = Pickler g' p'
  where
  g' (l :> cs) = G.replicateM (fromIntegral l) (g cs)
  p' xs = (fromIntegral (G.length xs) :> Nil, G.foldl' (\acc q -> acc <> snd (p q)) mempty xs)




-- ByteStrings

byteString :: Integral i => Pickler '[i] B.ByteString
byteString = Pickler (getByteString . fromIntegral . headH) p
  where p bs = (fromIntegral (B.length bs) :> Nil, fromByteString bs)

lazyByteString :: Integral i => Pickler '[i] L.ByteString
lazyByteString = Pickler (getLazyByteString . fromIntegral . headH) p
  where p bs = (fromIntegral (L.length bs) :> Nil, fromLazyByteString bs)


-- "Primitives"

word8 :: Pickle Word8
word8 = pickle getWord8 singleton

word16le, word16be :: Pickle Word16
word16le = pickle getWord16le putWord16le
word16be = pickle getWord16be putWord16be

word32le, word32be :: Pickle Word32
word32le = pickle getWord32le putWord32le
word32be = pickle getWord32be putWord32be

word64le, word64be :: Pickle Word64
word64le = pickle getWord64le putWord64le
word64be = pickle getWord64be putWord64be


int8 :: Pickle Int8
int8 = integral word8

int16le, int16be :: Pickle Int16
int16le = integral word16le
int16be = integral word16be

int32le, int32be :: Pickle Int32
int32le = integral word32le
int32be = integral word32be

int64le, int64be :: Pickle Int64
int64le = integral word64le
int64be = integral word16be


float32le, float32be :: Pickle Float
float32le = pickle getFloat32le (execPut . putFloat32le)
float32be = pickle getFloat32be (execPut . putFloat32be)

float64le, float64be :: Pickle Double
float64le = pickle getFloat64le (execPut . putFloat64le)
float64be = pickle getFloat64be (execPut . putFloat64be)