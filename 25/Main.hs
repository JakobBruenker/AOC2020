{-# LANGUAGE ScopedTypeVariables
           , ViewPatterns
           , LambdaCase
           , BlockArguments
           , StandaloneKindSignatures
           , GADTs
           , TypeOperators
           , ImportQualifiedPost
           , UndecidableInstances
           , DerivingVia
           , MultiWayIf
           , TupleSections
           , ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , NegativeLiterals
           , RankNTypes
           , AllowAmbiguousTypes
           , TypeFamilyDependencies
           , OverloadedLabels
           , DeriveGeneric
           , NamedWildCards
           , TypeApplications
           , DuplicateRecordFields
           , DisambiguateRecordFields
           , FlexibleInstances
           , PatternSynonyms
           , PartialTypeSignatures
           , StandaloneDeriving
           , DeriveFunctor
           , DeriveDataTypeable
           , InstanceSigs
           , NumDecimals
           , BangPatterns
#-}

import Control.Arrow hiding ((+++), first, second)
import Control.Comonad.Store
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Data.Array qualified as A
import Data.Array.ST qualified as STA
import Data.Bifunctor as BF
import Data.Char
import Data.Coerce
import Data.Data
import Data.Data.Lens
import Data.Monoid
import Data.Either
import Data.Functor
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Generics.Labels ()
import Data.Kind
import Data.IntMap.Strict qualified as IM
import Data.List hiding (uncons)
import Data.List.Split
import Data.Map.Strict (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Profunctor
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import GHC.Arr qualified (unsafeIndex, unsafeRangeSize)
import GHC.Stack
import Text.Read hiding (step, parens, get)
import Data.Word
import Data.Bits
import Data.Bits.Lens
import Data.Bool
import Numeric.Lens
import Control.Monad.Combinators.Expr
-- import Text.Megaparsec.Char
-- import Text.Megaparsec.Char.Lexer qualified as L
-- import Text.Megaparsec
import Data.Void
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Safe.Foldable

main :: IO ()
main = do
  [publicDoor, publicCard] <- map (read @Int) . lines <$> readFile "input"
  -- let (publicDoor, publicCard) = (17807724, 5764801) -- test input
  let privateDoor = findLoopSize publicDoor
      privateCard = findLoopSize publicCard
  print (applyN privateCard (step publicDoor) 1)
-- 14643366 is too high

step :: Int -> Int -> Int
step subject value = (value * subject) `mod` 20201227

findLoopSize :: Int -> Int
findLoopSize publicKey = go 0 1
  where
    go n v | v == publicKey = n
           | otherwise = go (n + 1) (step 7 v)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = f $ applyN (n - 1) f x
