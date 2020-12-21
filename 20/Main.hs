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
#-}

import Control.Arrow hiding ((+++), first)
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
import Data.IntMap qualified as IM
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
import GHC.Generics (Generic)
import GHC.Arr qualified (unsafeIndex, unsafeRangeSize)
import GHC.Stack
import Text.Read hiding (step, parens)
import Data.Word
import Data.Bits
import Data.Bits.Lens
import Numeric.Lens
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec
import Data.Void

-- Notes:
-- First, notice that the entire image is invariant to flipping and rotating,
-- so instead of 144!, this lowers the number of possible combinations by...
-- a factor of 8 I think? Not great.
-- But we don't need to test all combinations.
-- We just need to take all pairs of borders and record whether they (either as
-- is or with one reversed) line up.
-- Corner tiles are just those that only have two matches borders with
-- anything
-- the correct number should be 5496 - n * 15, n being the number of sea
-- monsters

type Tile = [[Bool]]

löb :: Functor f => f (f a -> a) -> f a
löb = möb fmap

möb :: (((a -> b) -> b) -> c -> a) -> c -> a
möb f x = fix \go -> f ($ go) x

parseTile :: String -> (Int, Tile)
parseTile (lines -> ('T':'i':'l':'e':' ': (reads -> [(n, _)])):grid) =
  (n, map (map (== '#')) grid)

getBorders :: Tile -> [[Bool]]
getBorders tile = let tile' = transpose tile in sequence [head, last] =<< [tile, tile']

gridSize :: Int = 12

main :: IO ()
main = do
  tiles <- map parseTile . init . splitOn "\n\n" <$> readFile "input"
  let tileBorders :: [(Int, [[Bool]])]
      tileBorders = (map . fmap) getBorders tiles
      matches :: [(Int, Int, Bool, [Bool])]
      matches = nub [ (i, j, rev, b)
                    | ((i, bs):js) <- tails tileBorders
                    , (j, bs') <- js
                    , b <- bs
                    , b' <- bs'
                    , (rev, f) <- [(True, reverse), (False, id)]
                    , b == f b'
                    ]
      corners = map (fst . head) . filter ((== 2) . length) .
        groupBy (\x y -> fst x == fst y) . nub . sort $
        concat [ [(i, b), (j, b)] | (i, j, rev, b) <- matches ]
  print $ product corners
  let worldMap = combine . shrink . rearrange $ assignMatches matches tiles
      maps = V.fromList . map V.fromList <$> orientations worldMap
      monsters = maximum $ map countMonsters maps
  print . subtract (monsters * length (monster (0,0))) . sum . map (sum . map fromEnum) $
    worldMap

countMonsters :: V.Vector (V.Vector Bool) -> Int
countMonsters world = sum . fmap (sum . fmap fromEnum) $
  flip imap world \y v -> flip imap v \x _ -> hasMonster world (x, y)

hasMonster :: V.Vector (V.Vector Bool) -> (Int, Int) -> Bool
hasMonster world = all (\(x, y) -> fromMaybe False (world ^? ix y . ix x)) . monster

monster :: (Int, Int) -> [(Int, Int)]
monster (x, y) = map (bimap (+x) (+y)) $
     map (,0) [                                       18]
  <> map (,1) [0,       5,6,        11,12         ,17,18,19]
  <> map (,2) [  1,   4,    7,   10      ,13,   16]

lookup' :: Eq a => a -> [(a, a, b, c)] -> [(a, b, c)]
lookup' x xs =
  let f l l' = map (\t -> (t ^. l', t ^. _3, t ^. _4)) $ filter ((x ==) . view l) xs
  in f _1 _2 <> f _2 _1

assignMatches :: [(Int, Int, Bool, [Bool])] -> [(Int, Tile)]
       -> IM.IntMap (Tile, [(Int, Bool, [Bool])])
assignMatches matches = IM.fromList . map \(n, tile) -> (n, (tile, lookup' n matches))

orientations :: Tile -> [Tile]
orientations xs = let f = take 4 . iterate rotateDeasil in f xs <> f (transpose xs)
  where
    rotateDeasil = map reverse . transpose

shrink :: [[Tile]] -> [[Tile]]
shrink = imap \y l -> flip imap l \x -> map tail . tail . map init . init

combine :: [[Tile]] -> Tile
combine = concat . map (foldl1' $ zipWith (<>))

rearrange :: IM.IntMap (Tile, [(Int, Bool, [Bool])]) -> [[Tile]]
rearrange tileMap = (map . map) (^. _1) . möb (map . map) $ flip (map . map) coords \case
  -- rotate corner until matching boundaries point towards center
  (0, 0) ->
    const . (,ms) . head . dropWhile topOrLeftMatches . orientations $ tile
    where
      tile :: Tile
      m1, m2 :: [Bool]
      ms :: [(Int, Bool, [Bool])]
      (tile, ms@[(_, _, m1), (_, _, m2)]) =
        head . filter ((2 ==) . length . snd) $ toList tileMap
      topOrLeftMatches :: [[Bool]] -> Bool
      topOrLeftMatches xs = or [ m == r
                               | f <- [transpose, id]
                               , g <- [reverse, id]
                               , let r = g . head . f $ xs
                               , m <- [m1, m2]
                               ]
  (x, y) -> \case
    tiles ->
      first (errorHead . filter (liftA2 (&&) firstRowMatches firstColMatches)
        . orientations) $ tileMap IM.! tileId
      where
        ltile :: Tile
        lms :: [(Int, Bool, [Bool])]
        (ltile, lms) = tiles !! y !! (x - 1)
        lastCol = map last ltile
        ttile :: Tile
        tms :: [(Int, Bool, [Bool])]
        (ttile, tms) = tiles !! (y - 1) !! x
        lastRow = last ttile
        tileId :: Int
        border :: [Bool]
        (tileId, _, border) = errorHead $ if x == 0
          then filter (\(view _3 -> row) -> lastRow == row || reverse lastRow == row) tms
          else filter (\(view _3 -> col) -> lastCol == col || reverse lastCol == col) lms
        firstColMatches :: Tile -> Bool
        firstColMatches (map errorHead -> xs) = x == 0 || lastCol == xs
        firstRowMatches :: Tile -> Bool
        firstRowMatches (errorHead -> xs) = y == 0 || lastRow == xs
  where
    coords :: [[(Int, Int)]]
    coords = [ (,y) <$> cs | let cs = [0..gridSize - 1], y <- cs ]
      -- this shorter version doesn't work, can't be bothered to debug it
  -- (x, y) -> \case
  --   tiles ->
  --     first (errorHead . filter firstColMatches
  --       . orientations) $ tileMap IM.! tileId
  --     where
  --       f :: (forall a . [a] -> a) -> [[b]] -> [b]
  --       f g | y == 0    = map g
  --           | otherwise = g
  --       ltile :: Tile
  --       lms :: [(Int, Bool, [Bool])]
  --       (ltile, lms) | y == 0    = tiles !! y !! (x - 1)
  --                    | otherwise = tiles !! (y - 1) !! x
  --       lastCol = f last ltile
  --       tileId :: Int
  --       border :: [Bool]
  --       (tileId, _, border) = errorHead $
  --         filter (\(view _3 -> col) -> lastCol == col || reverse lastCol == col) lms
  --       firstColMatches :: Tile -> Bool
  --       firstColMatches (f errorHead -> xs) = x == 0 || lastCol == xs

errorHead :: HasCallStack => [a] -> a
errorHead = fromMaybe (error "here") . listToMaybe
