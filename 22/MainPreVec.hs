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
           , StandaloneDeriving
           , DeriveFunctor
           , InstanceSigs
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
import GHC.Arr qualified (unsafeIndex, unsafeRangeSize)
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
type Arr s d = STA.STUArray s (Vec (S (S d)) Int) Bool

type Dimensions :: Nat -> Type -> Type
type family Dimensions d a where
  Dimensions Z a = ()
  Dimensions (S d) a = (a, Dimensions d a)

data Nat = Z | S Nat

type SNat :: Nat -> Type
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

infixr 5 :::
type Vec :: Nat -> Type -> Type
data Vec n a where
 Nil :: Vec Z a
 (:::) :: a -> Vec n a -> Vec (S n) a

deriving instance Eq a => Eq (Vec n a)

deriving instance Ord a => Ord (Vec n a)

deriving instance Show a => Show (Vec n a)

deriving instance Functor (Vec n)

instance STA.Ix a => STA.Ix (Vec n a) where
  range :: (Vec n a, Vec n a) -> [Vec n a]
  range (Nil, Nil) = [Nil]
  range (x:::xs, y:::ys) =
    [ i ::: is | i <- STA.range (x, y), is <- STA.range (xs, ys) ]

  unsafeIndex :: (Vec n a, Vec n a) -> Vec n a -> Int
  unsafeIndex (Nil, Nil) Nil = 0
  unsafeIndex (l:::ls, u:::us) (i:::is) =
    GHC.Arr.unsafeIndex (l, u) i * GHC.Arr.unsafeRangeSize (ls, us) +
    GHC.Arr.unsafeIndex (ls, us) is

  inRange :: (Vec n a, Vec n a) -> Vec n a -> Bool
  inRange (Nil, Nil) Nil = True
  inRange (l:::ls, u:::us) (i:::is) = STA.inRange (l, u) i && STA.inRange (ls, us) is

replicateTuple :: SNat d -> a -> Dimensions d a
replicateTuple SZ x = ()
replicateTuple (SS n) x = (x, replicateTuple n x)

replicateVec :: SNat n -> a -> Vec n a
replicateVec SZ _ = Nil
replicateVec (SS n) x = x ::: replicateVec n x

-- TODO OverloadedLists

zipWithVec :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWithVec _ Nil Nil = Nil
zipWithVec f (x:::xs) (y:::ys) = f x y ::: zipWithVec f xs ys

run :: [[Bool]] -> Int -> forall s . ST s Int
run initial steps = do
  -- let rep = replicateTuple (SS (SS SZ))
  let newArray :: forall s . ST s (Arr s (S (S Z)))
      newArray =
       STA.newArray
         do let s = steps in
             ( ((-s):::(-s):::rep (-s))
             , (s + width - 1:::s + height - 1:::rep s))
         False
  arrA <- newArray
  arrB <- newArray
  iforM_ initial \y l ->
    iforM_ l \x a ->
      STA.writeArray arrA (x:::y:::rep 0) a
  go 0 steps arrA arrB
  where
    rep = replicateVec (SS (SS SZ))
    repAll = replicateVec (SS (SS (SS (SS SZ))))
    width = length $ head initial
    height = length initial
    go :: Int -> Int -> forall s . Arr s (S (S Z)) -> Arr s (S (S Z)) -> ST s Int
    go n 0 arrA _ = sum . map fromEnum <$> STA.getElems arrA
    go n s arrA arrB = do
      maxBounds <- STA.getBounds arrA
      -- effectively shrink array to only cells that can be alive
      -- let bounds = maxBounds & _1 . template +~ s - 1 & _2 . template -~ s - 1
      let bounds = maxBounds -- XXX
      forM_ (STA.range bounds) \i@(x:::y:::z:::w:::Nil) -> do
        -- let checks = [ check
        --              | ds'@[dx, dy, dz, dw] <- replicateM 4 [-1..1]
        --              , let check = (x + d:::y + dy:::z + dz:::w + dw:::Nil)
        --              , STA.inRange bounds check
        --              ]
        let checks = filter (STA.inRange bounds) . map (zipWithVec (+) i)
              $ STA.range (repAll (-1), repAll 1)
        neighSum <- sumTo5 checks (STA.readArray arrA)
        alive <- STA.readArray arrA i
        -- sum includes current cell so rules are adjusted to account for that
        STA.writeArray arrB i $ neighSum == 3 || alive && neighSum == 4
      go (n + 1) (s - 1) arrB arrA -- arrays are swapped

-- count up the sum but stop once it reaches 4
sumTo5 :: forall a m . Monad m => [a] -> (a -> m Bool) -> m Int
sumTo5 = go 0
  where
    go :: Int -> [a] -> (a -> m Bool) -> m Int
    go acc [] _ = pure acc
    go acc (x:xs) f | acc >= 5 = pure acc
                    | otherwise = do
                      b <- f x
                      go (acc + fromEnum b) xs f
