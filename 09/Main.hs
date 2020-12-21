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

window :: Int
window = 25

data Nums = Nums { prev :: [Int]
                 , curr :: Int
                 , next :: [Int]
                 } deriving (Show, Generic)

isSum :: Nums -> (Bool, Nums)
isSum (Nums (take window -> p) c (n:ns)) =
  (or [ x + y == c | (x:ys) <- tails p, y <- ys ], Nums (c : p) n ns)

-- my first try:
-- findSum :: Int -> [Int] -> [Int]
-- findSum n xs = map snd . drop 1 . head . filter ((==n) . fst . last) . map (takeWhile ((<=n) . fst) . scanl (\(acc, n) n' -> (acc + n', n')) (0, 0)) $ tails xs

findSum :: Int -> [Int] -> S.Seq Int
findSum x xs = go Empty 0 xs
  where
    go seq s xs'@(n:ns) | s == x = seq
                        | l :< seq' <- seq
                        , s > x = go seq' (s - l) xs'
                        | otherwise = go (seq |> n) (s + n) ns

main :: IO ()
main = do
  input <- map read . lines <$> readFile "input"
  let (p, c:n) = splitAt window input
      p1 = head . prev . snd . head . dropWhile fst . iterate (isSum . snd) $
        (True, Nums (reverse p) c n)
  print p1
  print . liftA2 (+) minimum maximum $ findSum p1 input
