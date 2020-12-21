{-# LANGUAGE ScopedTypeVariables, ViewPatterns, LambdaCase #-}
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Bifunctor

parse :: String -> (Int, Int, Char, String)
parse str = (lo, hi, c, pwd)
  where
    [(lo :: Int, drop 1 -> loR)] = reads str
    [(hi :: Int, drop 1 -> hiR)] = reads loR
    Just (c, drop 2 -> pwd) = uncons hiR

one :: [(Int, Int, Char, String)] -> Int
one = length . filter one'

one' :: (Int, Int, Char, String) -> Bool
one' (lo, hi, c, pwd) = (\l -> l >= lo && l <= hi) . length . filter (== c) $ pwd

two :: [(Int, Int, Char, String)] -> Int
two = length . filter two'

two' :: (Int, Int, Char, String) -> Bool
two' (lo, hi, c, pwd) =
  (== 1) . length . filter (liftA2 (||) (== lo) (== hi)) . map (+1) . elemIndices c $
  pwd

main :: IO ()
main = print . liftA2 (,) one two . map parse . lines =<< readFile "input"
