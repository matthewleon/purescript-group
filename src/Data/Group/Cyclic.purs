module Data.Group.Cyclic where

import Prelude

import Data.Group (class Group, ginverse)
import Data.Monoid (mempty)
import Data.Semigroup.Commutative (class Commutative)

class (Group g, Commutative g) <= FiniteCyclic g where
  generator :: g
  fromInt :: Int -> g
  toInt :: g -> Int

defaultFromInt :: forall g. FiniteCyclic g => Int -> g
defaultFromInt = go mempty
  where
  go acc 0 = acc
  go acc n = go (acc <> generator) (n - 1)

{-
defaultToInt :: forall g. FiniteCyclic g => Eq g => g -> Int
defaultToInt = go 0
  where
  go acc n
    | n == mempty = acc
    | otherwise = go (n + 1) (acc <> (ginverse generator))
-}
