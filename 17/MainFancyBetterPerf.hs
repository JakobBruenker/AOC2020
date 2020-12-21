{-# LANGUAGE ScopedTypeVariables
           , ViewPatterns
           , LambdaCase
           , BlockArguments
           , StandaloneKindSignatures
           , GADTs
           , TypeOperators
           , ImportQualifiedPost
           , UndecidableInstances
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
#-}

import Control.Applicative
import Control.Arrow
import Control.Comonad.Store
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Data.Array qualified as A
import Data.Array.ST qualified as STA
import Data.Bifunctor as BF
import Data.Char
import Data.Coerce
import Data.Data.Lens
import Data.Either
import Data.Functor
import Data.Generics.Labels ()
import Data.Kind
import Data.IntMap qualified as IM
import Data.List hiding (uncons)
import Data.List.Split
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Profunctor
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import Text.Read hiding (step)
import Data.Word
import Data.Bits
import Data.Bits.Lens
import Numeric.Lens

main :: IO ()
main = do
  input <- map (map (== '#')) . lines <$> readFile "input"
  print $ runST (run input 6)

-- TODO: have a variable that affects the length of the second tuple
type Arr s d = STA.STUArray s (Int, Int, Dimensions d Int) Bool

type Dimensions :: Nat -> Type -> Type
type family Dimensions d a where
  Dimensions Z a = ()
  Dimensions (S d) a = (a, Dimensions d a)

data Nat = Z | S Nat

type SNat :: Nat -> Type
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

replicateTuple :: SNat d -> a -> Dimensions d a
replicateTuple SZ x = ()
replicateTuple (SS n) x = (x, replicateTuple n x)

checks' :: index ~ Dimensions d Int => SNat d -> (index, index) -> [index]
checks' = undefined

checks :: index ~ (Int, Int, Dimensions d Int) => SNat d -> (index, index) -> [index]
checks = undefined

run :: [[Bool]] -> Int -> forall s . ST s Int
run initial steps = do
  let rep = replicateTuple (SS (SS SZ))
  let newArray :: forall s . ST s (Arr s (S (S Z)))
      newArray =
       STA.newArray
         do let s = steps in
             ( (-s, -s, rep (-s))
             , (s + width - 1, s + height - 1, rep s))
         False
  arrA <- newArray
  arrB <- newArray
  iforM_ initial \y l ->
    iforM_ l \x a ->
      STA.writeArray arrA (x, y, rep 0) a
  go 0 steps arrA arrB
  where
    width = length $ head initial
    height = length initial
    go :: Int -> Int -> forall s . Arr s (S (S Z)) -> Arr s (S (S Z)) -> ST s Int
    go n 0 arrA _ = sum . map fromEnum <$> STA.getElems arrA
    go n s arrA arrB = do
      maxBounds <- STA.getBounds arrA
      -- effectively shrink array to only cells that can be alive
      let bounds = maxBounds & _1 . template +~ s - 1 & _2 . template -~ s - 1
      forM_ (STA.range bounds) \i@(x, y, (z, (w, ()))) -> do
        let checks = [ check
                     | ds'@[dx, dy, dz, dw] <- sequence $ replicate 4 [-1..1]
                     , any (/= 0) ds'
                     , let check = (x + dx, y + dy, (z + dz, (w + dw, ())))
                     , STA.inRange bounds check
                     ]
        neighSum <- sumTo4 checks (STA.readArray arrA)
        alive <- STA.readArray arrA i
        STA.writeArray arrB i $ neighSum == 3 || alive && neighSum == 2
      go (n + 1) (s - 1) arrB arrA -- arrays are swapped

-- count up the sum but stop once it reaches 4
sumTo4 :: forall a m . Monad m => [a] -> (a -> m Bool) -> m Int
sumTo4 = go 0
  where
    go :: Int -> [a] -> (a -> m Bool) -> m Int
    go acc [] _ = pure acc
    go acc (x:xs) f | acc >= 4 = pure acc
                    | otherwise = do
                      b <- f x
                      go (acc + fromEnum b) xs f
