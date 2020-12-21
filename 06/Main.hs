{-# LANGUAGE ScopedTypeVariables, ViewPatterns, LambdaCase, BlockArguments
           , StandaloneKindSignatures, GADTs, TypeOperators #-}
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Bifunctor
import Data.Profunctor
import Data.Functor.Contravariant
import Data.Kind
import Data.Coerce
import Data.List.Split
import Data.List

main :: IO ()
main = do
  input <- wordsBy null . lines <$> readFile "input"
  let go f = print . sum $ length . foldl1' f <$> input
  go union *> go intersect
