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
import Data.Sequence qualified as S
import Debug.Trace

-- successfully calculates 19208 for inputTest
naive :: [Int] -> Int
naive (a:as) = go a as
  where
    go _ [] = 1
    go n (x:xs) | x - n <= 3 = if | null xs   -> 1
                                  | otherwise -> go x xs + go n xs
                | otherwise  = 0

-- find adapters that have to be used and split between them
smarter :: [Int] -> Int
smarter = product . map (naive . map fst) . splitOnMandatory . attachDiffs
  where
    splitOnMandatory = split . keepDelimsR . dropFinalBlank . whenElt $ (== 3) . snd
    attachDiffs = zipWith (\x y -> (x, y - x)) <*> tail

main :: IO ()
main = do
  input <- sort . map read . lines <$> readFile "input"
  let adapters = 0 : input <> [maximum input + 3]
  -- if there are differences of 2, this won't handle it, but my input didn't
  -- have any
  print . product . map length . group . sort . (zipWith (flip (-)) <*> tail) $ adapters
  print . smarter $ adapters

-- Better way: build a Map that records how many paths there are from each
-- adapter to the end, using sum . mapMaybe (paths ^?) [ad+1, ad+2, ad+3] or
-- something for each entry except the largest one, which has exactly 1 path.
-- That way you get some nice memoized behavior.
-- Could also use loeb, glguy did that.
-- https://github.com/glguy/advent2020/blob/master/execs/Day10.hs
-- https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day10.md
