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

data Neighbor = E
              | SE
              | SW
              | W
              | NW
              | NE
              deriving (Show, Eq, Ord, Enum, Bounded)

type BlackTiles = M.Map (Int, Int) Bool

parse :: String -> [Neighbor]
parse [] = []
parse ('e':s) = E : parse s
parse ('s':'e':s) = SE : parse s
parse ('s':'w':s) = SW : parse s
parse ('w':s) = W : parse s
parse ('n':'w':s) = NW : parse s
parse ('n':'e':s) = NE : parse s

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input"
  let lobby = makeFloor input
      run f = print . alaf Sum foldMap fromEnum $ f lobby
  run id
  run $ applyN 100 day

makeFloor :: [[Neighbor]] -> BlackTiles
makeFloor = M.unionsWith (/=) . map (flip M.singleton True . neighborsToCoord)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = f $ applyN (n - 1) f x

-- part 2 idea:
-- 1. after every day, filter so you're only left with tiles that are alive (black)
-- 2. after that, make a new False entry for every neighbor of a black tile that's
-- not already in the Map
-- 3. go through all tiles in the Map and apply the rule
-- -> go back to step one

day :: BlackTiles -> BlackTiles
day !tiles = imap rule neighbored
  where
    alive = M.filter id tiles
    neighbored = M.unionsWith (||) (imap addNeighbors alive)
    neighbors (a, b) = map (\(neighborToCoord -> (x, y)) -> (a + x, b + y)) [minBound..]
    addNeighbors i b = M.fromList $ (i, b) : map (,False) (neighbors i)
    getNeighbors i = sumTo 3 $
      map (fromEnum . (fromMaybe False . (alive M.!?))) (neighbors i)
    rule i = let ns = getNeighbors i in ($ ns) . bool (==2) \x -> x > 0 && x < 3

-- sum numbers in a list but don't bother continuing once they cross a
-- threshold
sumTo :: Int -> [Int] -> Int
sumTo n = go 0
  where
    go acc _ | acc >= n = acc
    go acc [] = acc
    go acc (x:xs) = go (x + acc) xs

neighborsToCoord :: [Neighbor] -> (Int, Int)
neighborsToCoord = coerce @(Sum Int, Sum Int) . foldMap (coerce neighborToCoord)

neighborToCoord :: Neighbor -> (Int, Int)
neighborToCoord = \case
  E  -> ( 1,  0)
  SE -> ( 1, -1)
  SW -> ( 0, -1)
  W  -> (-1,  0)
  NW -> (-1,  1)
  NE -> ( 0,  1)
