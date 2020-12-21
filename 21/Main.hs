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
-- import Text.Megaparsec.Char
-- import Text.Megaparsec.Char.Lexer qualified as L
-- import Text.Megaparsec
import Data.Void

newtype Ingredient = Ingredient String deriving (Show, Eq, Ord)
newtype Allergen = Allergen String deriving (Show, Eq, Ord)

type Food = (Set.Set Ingredient, Set.Set Allergen)
type Candidates = Map Allergen (Either Ingredient (Set.Set Ingredient))

löb :: Functor f => f (f a -> a) -> f a
löb = möb fmap

möb :: (((a -> b) -> b) -> c -> a) -> c -> a
möb f x = fix \go -> f ($ go) x

parse :: String -> Food
parse = bimap Set.fromList Set.fromList .
  coerce . second (map init . tail) . span (\case '(':_ -> False; _ -> True) . words

main :: IO ()
main = do
  foods <- map parse . lines <$> readFile "input"
  let candidates alrg = foldr1 Set.intersection . map fst $
        filter (Set.member alrg . snd) foods
      allAlrg = foldMap snd foods
      allIngr = foldMap fst foods
      nonAlrg = allIngr Set.\\ foldMap candidates allAlrg
      -- foods' = (map . first) (Set.filter (not . (`Set.member` nonAlrg))) foods
      candidateMap :: Candidates
      candidateMap = M.fromList . map (\a -> (a, Right $ candidates a)) $ toList allAlrg
      extractSingle :: Candidates -> Ingredient
      extractSingle = head . fromRight [] . fmap toList . head . toList .
        M.filter (either (const False) ((== 1) . Set.size))
      removeSingle :: Candidates -> Candidates
      removeSingle cmap = let single = extractSingle cmap in
        flip fmap cmap \mingrds -> mingrds >>= \ingrds -> if
          | toList ingrds == [single] -> Left single
          | otherwise -> Right $ Set.filter (/= single) ingrds
      algrMap = head . dropWhile (not . all isLeft) . iterate removeSingle $ candidateMap
  print . sum . map (Set.size . Set.filter (`Set.member` nonAlrg) . fst) $ foods
  putStrLn . intercalate "," . map (^?! _2 . _Left . to coerce) $ M.toAscList algrMap
