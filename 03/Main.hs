{-# LANGUAGE ScopedTypeVariables, ViewPatterns, LambdaCase, ImportQualifiedPost #-}
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Bifunctor
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Prelude

type TreeMap = Vector (Vector Bool)

parse :: [String] -> TreeMap
parse = V.fromList . fmap (V.fromList . fmap (== '#'))

count :: TreeMap -> Int -> Int -> Int
count trees dx dy = go 0 0 0
  where
    mx = V.length (trees ! 0)
    my = V.length trees
    go x y t
      | y >= my = t
      | otherwise = go (x + dx) (y + dy)
        (t + fromEnum (trees ! y ! (x `mod` mx)))

main :: IO ()
main = do
  array <- parse . lines <$> readFile "input"
  print $ count array 3 1
  print . Prelude.product $ zipWith (count array)
    [1, 3, 5, 7, 1]
    [1, 1, 1, 1, 2]
