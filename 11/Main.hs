{-# LANGUAGE ScopedTypeVariables
           , ViewPatterns
           , LambdaCase
           , BlockArguments
           , StandaloneKindSignatures
           , GADTs
           , TypeOperators
           , ImportQualifiedPost
           , MultiWayIf
           , TupleSections
           , ConstraintKinds
           , FlexibleContexts
           , RankNTypes
           , OverloadedLabels
           , DeriveGeneric
           , NamedWildCards
           , TypeApplications
           , DuplicateRecordFields
           , DisambiguateRecordFields
           , FlexibleInstances
#-}

import Control.Comonad.Store
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Bifunctor
import Data.Profunctor
import Data.Kind
import Data.Coerce
import Data.Generics.Labels ()
import GHC.Generics hiding (to)
import Data.List.Split
import Data.List hiding (uncons)
import Control.Monad.State
import Control.Lens
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Debug.Trace

data Seat = Floor | Vacant | Occupied deriving (Show, Eq)

parseSingle :: Char -> Seat
parseSingle '.' = Floor
parseSingle 'L' = Vacant
parseSingle '#' = Occupied

neighbors :: [(Int, Int)]
neighbors = [(x,y) | let xs = [-1..1], x <- xs, y <- xs, any (/=0) [x, y]]

countSeat :: Seat -> Int
countSeat Occupied = 1
countSeat _ = 0

-- could cache the visible seats for each seat but, meh, this works
nextSeat :: Bool -> Vector (Vector Seat) -> (Int, Int) -> (Int, Int) -> Seat
nextSeat see ss (x, y) (dx, dy) = go 1
  where
    go n =
      let s = fromMaybe Vacant (ss ^? ix (y + dy * n) ^? _Just . ix (x + dx * n))
      in if | s == Floor && see -> go (n + 1)
            | otherwise -> s

sumNeighs :: Bool -> Vector (Vector Seat) -> (Int, Int) -> Int
sumNeighs see ss xs = sum . map (countSeat . nextSeat see ss xs) $ neighbors

step :: Bool -> Int -> Vector (Vector Seat) -> Vector (Vector Seat)
step see thresh ss = imap (\y row -> imap (\x seat ->
  let s = sumNeighs see ss (x, y)
  in if | seat == Floor -> Floor
        | s == 0 -> Occupied
        | s >= thresh -> Vacant
        | otherwise -> seat) row) ss

instance {-# OVERLAPPING #-} Show (Vector (Vector Seat)) where
  show ss = unlines . V.toList $ fmap (V.toList . fmap \case {Floor -> '.'; Vacant -> 'L'; Occupied -> '#'}) ss

main :: IO ()
main = do
  input <- V.fromList . map (V.fromList . map parseSingle) . lines <$> readFile "input"
  let task see thresh = print . sum . fmap (sum . fmap countSeat) . snd . last . takeWhile (uncurry (/=)) $ zip`ap`tail $ iterate (step see thresh) input
  task False 4
  task True 5

-- You can speed this up (found out by looking at an animation of the
-- iterations as well as a bit of thinking and glguys help) by doing a
-- frontier-based approach, where in each iteration, you only look at the seats
-- which have changed in the previous iteration, or whose neighbors have.
-- Buuut I'm too lazy to implement it right now
