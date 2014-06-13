{-# LANGUAGE GADTs, TypeFamilies, PolyKinds, DataKinds, KindSignatures, TypeOperators, ImplicitParams, ViewPatterns #-}
module Data.Pickler.Node where

import Data.Monoid
import Control.Applicative

import Data.Pickler.Types

-- Really just sort of a restricted linear programming language
data Node :: [*] -> * -> * where
  Ret  :: Node '[t] t
  Prim :: Pickler ts t -> Node ts t
  Bind :: Partition as bs cs -> Node bs s -> Node (s ': cs) t -> Node as t


fromNode :: Node ts t -> Pickler ts t
fromNode Ret = Pickler (pure . headH) (\x -> (x :> Nil, mempty))
fromNode (Prim p) = p
fromNode (Bind pt (fromNode -> Pickler g p) (fromNode -> Pickler g' p')) = Pickler g'' p''
  where
  g'' (partition pt -> (ls, rs)) = do s <- g ls; g' (s :> rs)
  p'' (p' -> ((p -> (bs, b')) :> cs, b)) = (assemble pt bs cs, b' <> b)
