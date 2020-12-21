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
import Data.List.Split
import Data.Kind
import Data.Coerce
import Data.Generics.Labels ()
import GHC.Generics
import Data.List
import Control.Monad.State
import Control.Lens
import Data.Vector (Vector, (//))
import Data.Vector qualified as V
import Data.Set qualified as S

data Op = Acc | Jmp | Nop deriving Show

type Instruction = (Op, Int)

data CPU = CPU { insPointer :: Int
               , accum      :: Int
               , visited    :: S.Set Int
               } deriving Generic

type CPUState = State CPU

type Instructions t = (Ixed t, Index t ~ Int, IxValue t ~ Instruction)
-- type Instructions t = t ~ Vector Instruction

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen b f x | b         = f x
                | otherwise = x

parse :: String -> Instruction
parse (op0:op1:op2:' ':s:num) = (code, applyWhen (s == '-') negate $ read num)
  where code = case [op0, op1, op2] of
          "acc" -> Acc
          "jmp" -> Jmp
          "nop" -> Nop

runOnce :: Instruction -> CPUState Int
runOnce (op, n) = do
  case op of
    Acc -> do #accum      += n
              #insPointer += 1
    Jmp ->    #insPointer += n
    Nop ->    #insPointer += 1
  use #accum

run :: Instructions t => t -> CPUState (Bool, Int)
run is = do
  ip <- use #insPointer
  seen <- (ip `S.member`) <$> use #visited
  let mins = is ^? ix ip
  if | seen -> (False,) <$> use #accum
     | (Just ins) <- mins -> do
         #visited %= S.insert ip
         runOnce ins
         run is
     | otherwise -> (True,) <$> use #accum

-- technically you could skip instructions with +1, since (nop, 1) and (jmp, 1)
-- are the same
flipIns :: Instructions t => t -> Int -> Maybe t
flipIns is n = swapIxIns =<< is ^? ix n . _1
  where swapIxIns = fmap (\i -> is & ix n . _1 .~ i) . \case Acc -> Nothing
                                                             Jmp -> Just Nop
                                                             Nop -> Just Jmp

main :: IO ()
main = do
  input <- V.fromList . map parse . lines <$> readFile "input"
  let eval = (evalState ?? CPU 0 0 S.empty) . run
  let ppr = print . snd
  ppr . eval $ input
  ppr . head . filter fst . map eval . catMaybes $
    flipIns input <$> [0..V.length input - 1]

-- adaptation of jle`'s amazing lensy solution:
--https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Challenge/Day08.hs#L86
-- (The idea to use Ixed instead of Vector directly also came from him)

-- Do I know how this works? Why, I'm glad you asked, I absolutely don't.
-- Though I am slowly gaining an understanding of it.
perturbationsBy :: Conjoined p
                => Over p (Bazaar p a a) s t a a -> (a -> [a]) -> s -> [t]
perturbationsBy p f = experiment f <=< holesOf p

main' :: IO ()
main' = do
  input <- V.fromList . map parse . lines <$> readFile "input"
  let eval = flip evalState (CPU 0 0 S.empty) . run
  let ppr = print . snd
  ppr . eval $ input
  ppr . head . filter fst . map eval $ (perturbationsBy (each . _1) ?? input) \case
    Nop -> [Jmp]
    Acc -> []
    Jmp -> [Nop]
