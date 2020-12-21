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

type Color = (String, String)
type Contains = [(Color, Int)]
type Rule = (Color, Contains)

parse :: [String] -> Rule
parse xs = case xs of
  (b0:b1:_:_:rest) -> ((b0, b1), parseRest rest)

parseRest :: [String] -> Contains
parseRest [] = []
parseRest ["no","other","bags."] = []
parseRest ((read -> n):c0:c1:_:rest) = ((c0, c1), n) : parseRest rest

whichContain :: Color -> [Rule] -> [Color]
whichContain _ [] = []
whichContain c ((rcol, rcon):rs) | rcon `contains` c = rcol : whichContain c rs
                                 | otherwise = whichContain c rs

contains :: Contains -> Color -> Bool
contains [] _ = False
contains ((cc, cn):rest) c = c == cc || rest `contains` c

newtype BagTree = BagNode [(BagTree, Int)] deriving Show

whichIn :: Color -> [Rule] -> BagTree
whichIn c rs = BagNode . maybe [] (fmap \(col, n) -> (whichIn col rs, n)) $ lookup c rs

countTree :: BagTree -> Int
countTree (BagNode ns) = sum . fmap (\(tree, n) -> n * (1 + countTree tree)) $ ns

shinyGold :: Color
shinyGold = ("shiny", "gold")

main :: IO ()
main = do
  input <- map (parse . words) . lines <$> readFile "input"
  let wci = flip whichContain input
  print . length . nub . concat . takeWhile (not . null) $
    iterate (concatMap wci) (wci shinyGold)
  print . countTree . whichIn shinyGold $ input
