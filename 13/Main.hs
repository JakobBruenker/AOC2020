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
           , NegativeLiterals
           , RankNTypes
           , OverloadedLabels
           , DeriveGeneric
           , NamedWildCards
           , TypeApplications
           , DuplicateRecordFields
           , DisambiguateRecordFields
           , FlexibleInstances
#-}

import Control.Applicative
import Control.Arrow
import Control.Comonad.Store
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Char
import Data.Coerce
import Data.Functor
import Data.Generics.Labels ()
import Data.Kind
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
import GHC.Generics hiding (to)
import Text.Read

parse :: String -> [Maybe Integer]
parse = map readMaybe . splitOn ","

main :: IO ()
main = do
  [read -> earliest, parse -> input] <- lines <$> readFile "input"

  -- part 1
  print . uncurry (*) . minimumBy (comparing snd) .
    map (\b -> (b, b - earliest `mod` b)) $ catMaybes input

  -- part 2
  let ((0, b0):busIds) = mapMaybe sequence $ zip [0..] input
      nums = zip <*> tail $ map (uncurry $ numsFor b0) busIds
      finalize sol = let (_, (m1, m2)) = last nums in m1 + m2 * sol
  print . finalize . head . foldl' (\sols (ks, ls) -> solutions sols ks ls) [0..] $ nums

solutions :: [Integer] -> (Integer, Integer) -> (Integer, Integer) -> [Integer]
solutions ksols (k1, k2) (l1, l2) = [s,t..] -- recompute solutions list more efficiently
  where
    (s:t:_) = lsols ksols
    lsols = map fst . filter ((0 ==) . snd) . map (\k -> ((k1 - l1) + k2 * k) `divMod` l2)

numsFor :: Integer -> Integer -> Integer -> (Integer, Integer)
numsFor firstBus offset busId = (busId * low - offset, busId * firstBus)
  where low = lowestForBus firstBus offset busId

lowestForBus :: Integer -> Integer -> Integer -> Integer
lowestForBus firstBus offset busId =
  fst . head . filter (\(x, y) -> (os + busId * x) `mod` firstBus == 0) . zip [0..] $
    [os, os + busId..]
  where os = -offset

-- Notes I made while trying to solve this:


-- 17, x, 13, 19

-- looking for a number t such that

-- t `mod` 17 == 0

-- (t + 2) `mod` 13 == 0

-- (t + 3) `mod` 19 == 0



-- if I have -2, how often do I have to add 13 to get something divisible by 17?



-- -2 + x * 13 = k * 17
-- x * 13 = k * 17 + 2
-- x = (k * 17 + 2) / 13


-- -2
-- 11
-- 24
-- ...

-- mod 17 reduces by 4 every time, because 17 - 13 = 4

-- 15 + 17    = 32 = 17 - 2 + 17
-- 32 / 4     =  8 = 32 / (17 - 13)

-- 4 * x = 15   modulo 17

-- let's see if it works with 19:
-- it *increases* by 2 every time now
-- alternatively decreases by 15 every time

-- what number do you have to multiply 15 by to get a number which is 14 modulo 17?

-- 15 * x = 14   modulo 17

-- what's the next number with that property?

-- if this works for x = 10, it will work for x = 10 + 17 * k for any k.

-- for 13:

-- 4 * x = 15   modulo 17
-- first number is 8, so it works for x = 8 + 17 * k for any k


-- so, we know:
-- we need a number that's
-- (-3) + 19 * (10 + 17 * k)
-- and
-- (-2) + 13 * (8 + 17 * l)

-- check:
-- (-3) + 19 * (10 + 17 * k) = 3417
-- 19 * (10 + 17 * k) = 3420
-- 10 + 17 * k = 180
-- 17 * k = 170
-- k = 10

-- (-2) + 13 * (8 + 17 * l) = 3417
-- 13 * (8 + 17 * l) = 3419
-- 8 + 17 * l = 263
-- 17 * l = 255
-- l = 15

-- so we're trying to find the lowest possible solution to
-- (-3) + 19 * (10 + 17 * k) = (-2) + 13 * (8 + 17 * l)
-- 187 + 323 * k = 102 + 221 * l
-- 85 + 323 * k = 221 * l
-- l = (85 + 323 * k) / 221

-- now how would we go about finding a number this works with?

-- I suppose we might be able to brute force this part

-- what does the solution space look like?

-- k can be 10, 23, 36, 49, 62... (+13 every time)
-- l can be 15, 34, 53, 72, 91... (+19 every time)

-- okay, how about if we have more than three buses though?

-- 67,7,59,61

-- lowest for 7: 48
-- lowest for 59: 50
-- lowest for 61: 33

-- so, we need
-- (-1) + 7 * (48 + 67 * k) = (-2) + 59 * (50 + 67 * l) = (-3) + 61 * (33 + 67 * m)

-- idea: do it one by one, first find all the possible solutions for f(k) = g(l),
-- and then find the solutions where that matches m

-- (-1) + 7 * (48 + 67 * k) = (-2) + 59 * (50 + 67 * l)
-- (-1) + 336 + 469 * k =  (-2) + 2950 + 3953 * l
-- 335 + 469 * k = 2948 + 3953 * l
-- -2613 + 469 * k = 3953 * l
-- (-2613 + 469 * k) / 3953 = l

-- solutions for k: 14, 73... (+59)
-- solutions for l:  1,  8... (+ 7)

-- (-2) + 59 * (50 + 67 * l) = (-3) + 61 * (33 + 67 * m)
-- 2948 + 3953 * l = 2010 + 4087 * m
-- m = (938 + 3953 * l) / 4087

-- new solutions for l: 190, 617... (+427 = 7 * 61)
--     solutions for m: 184, 597... (+413 = 7 * 59)

-- 2010 + 4087 * m
-- 2010 + 4087 * 184
-- 754018

-- which is the right solution!!
