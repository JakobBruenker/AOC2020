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
import Text.Read hiding (step, parens, get)
import Data.Word
import Data.Bits
import Data.Bits.Lens
import Data.Bool
import Numeric.Lens
import Control.Monad.Combinators.Expr
-- import Text.Megaparsec.Char
-- import Text.Megaparsec.Char.Lexer qualified as L
-- import Text.Megaparsec
import Data.Void
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

type Deck = Seq.Seq Int
data Winner = P1 | P2 deriving (Eq, Show)
data Game = Game { states :: Set.Set (Deck, Deck) 
                 , players :: (Deck, Deck)
                 } deriving (Generic, Show)

main :: IO ()
main = do
  [deck1, deck2] <- map (Seq.fromList . map (read @Int) . tail . lines) . splitOn "\n\n"
    <$> readFile "input"
  let run p = print . score . snd $ evalState p (Game Empty (deck1, deck2))
  run play1
  run play2

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList

play :: MaybeT (State Game) (Winner, Deck) -> State Game (Winner, Deck)
play turn = maybe (play turn) pure =<< runMaybeT turn

play1 :: State Game (Winner, Deck)
play1 = play turn1

play2 :: State Game (Winner, Deck)
play2 = play turn2

winDecks :: ((Int, Deck), (Int, Deck)) -> Bool -> (Deck, Deck)
winDecks ((c1, d1), (c2, d2)) = bool (d1, d2 |> c2 |> c1) (d1 |> c1 |> c2, d2)

turn1 :: MaybeT (State Game) (Winner, Deck)
turn1 = zoom #players $ get >>= \case
  (p1, p2) | Empty <- p1 -> pure (P2, p2)
           | Empty <- p2 -> pure (P1, p1)
           | c1 :< d1 <- p1
           , c2 :< d2 <- p2 -> do put $ winDecks ((c1, d1), (c2, d2)) (c1 > c2)
                                  empty

turn2 :: MaybeT (State Game) (Winner, Deck)
turn2 = do
  Game ss ps <- get
  if | ps `Set.member` ss -> pure (P1, fst ps)
     | otherwise -> do
       #states %= Set.insert ps
       case ps of
         (p1, p2) | Empty <- p1 -> pure (P2, p2)
                  | Empty <- p2 -> pure (P1, p1)
                  | c1 :< d1 <- p1
                  , c2 :< d2 <- p2 -> do
                    let d1' = Seq.take c1 d1
                        d2' = Seq.take c2 d2
                        wd = winDecks ((c1, d1), (c2, d2))
                    #players .= if
                      | c1 > length d1' || c2 > length d2' -> wd $ c1 > c2
                      | otherwise -> let (w, _) = evalState play2 (Game Empty (d1', d2'))
                                     in wd $ w == P1
                    empty
