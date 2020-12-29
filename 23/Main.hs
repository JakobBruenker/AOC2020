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
import Data.Either
import Data.Functor
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Generics.Labels ()
import Data.Kind
import Data.IntMap.Strict qualified as IM
import Data.List hiding (uncons)
import Data.List.Split
import Data.Map (Map)
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

-- trying to see what I can do with immutable data structures, a mutable array
-- instead of the IntMap would be faster of course
type Nexts = IM.IntMap Int

main :: IO ()
main = do
  input <- map (read @Int . pure) . init <$> readFile "input"
  let max1 = length input
      cups1 = initialize max1 input
      max2 = 1e6
      cups2 = initialize max2 input
  putStrLn . map intToDigit . init . toOrderedList . snd . applyN 100 (step max1) $
    (1, cups1)
  print . (\cups -> let one = cups IM.! 1
                        two = cups IM.! one
    in one * two) . snd . applyN 1e7 (step max2) $ (1, cups2)

initialize :: Int -> [Int] -> Nexts
initialize maxC cups = IM.insert (if length cups < maxC then maxC else last cups)
  (head cups) $
  IM.fromList $ zip <*> tail $ cups <> [length cups + 1 .. maxC]

toOrderedList :: Nexts -> [Int]
toOrderedList nexts = go 1
  where go i = let n = nexts IM.! i in n : if n == 1 then [] else go n

step :: Int -> (Int, Nexts) -> (Int, Nexts)
-- Bang pattern here is very important
step maxC (!current, nexts) = (next,) $ IM.union
  do IM.fromList [ (current, next)
                 , (dropped3, nexts IM.! target)
                 , (target, dropped1)
                 ]
  nexts
  where
    dropped1 = nexts IM.! current
    dropped2 = nexts IM.! dropped1
    dropped3 = nexts IM.! dropped2
    next = nexts IM.! dropped3
    target = dest current
    dest ((\i -> if i <= 1 then maxC else i - 1) -> i) =
      if i `elem` [dropped1, dropped2, dropped3]
         then dest i
         else i

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = let !r = f $ applyN (n - 1) f x in
  bool r (traceShow n r) (n `mod` 1e5 == 0)

-- old slow versions
-- main :: IO ()
-- main = do
--   input <- map (read @Int . pure) . init <$> readFile "input"
--   putStrLn . map intToDigit . concat . reverse . splitOn [1] $ applyN 100 step1 input
--   let million = Seq.fromList . take 1e6 $ input <> tail [maximum input..]
--   -- print . extractProduct $ applyN 1e2 (step2 1e6) million
--   pure ()
--   print $ applyN 100 step1 input

-- extractProduct :: Seq Int -> Int
-- extractProduct seq = let (l, r) = Seq.splitAt (fromJust (Seq.elemIndexL 1 seq) + 1) seq in
--   product . Seq.take 2 $ r <> l

-- -- old inefficient implementation
-- step1 :: [Int] -> [Int]
-- step1 (current :< (splitAt 3 -> (dropped, rest))) = new
--   where
--     dest = fromMaybe (maximum rest) . maximumMay $ filter (< current) rest
--     [l, r] = split (keepDelimsR $ oneOf [dest]) rest
--     new = l <> dropped <> r <> [current]

-- step2 :: Int -> Seq Int -> Seq Int
-- step2 maxC (current :< (Seq.splitAt 3 -> (dropped, rest))) = new
--   where
--     destIndex = targetIndex current maxC dropped rest
--     (l, r) = Seq.splitAt (destIndex + 1) rest
--     new = l <> dropped <> r <> Seq.singleton current

-- targetIndex :: Int -> Int -> Seq Int -> Seq Int -> Int
-- targetIndex cur maxC dropped cs = let target = ((cur - 2) `mod` maxC) + 1
--   in if | target `elem` dropped -> targetIndex target maxC dropped cs
--         | otherwise             -> fromJust $ Seq.elemIndexL target cs
