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
import Data.Profunctor
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics hiding (to)

data Facing = North | East | South | West deriving (Eq, Ord, Enum, Show, Bounded)
data Ship = Ship { facing :: Facing
                 , location :: (Int, Int)
                 , waypoint :: (Int, Int)
                 } deriving (Generic, Show)
data Rotation = RotLeft | RotRight deriving Show
data Op = Fac Facing
        | Forward
        | Rot Rotation
        deriving Show
data Instruction = Instruction { oper :: Op
                               , val :: Int
                               } deriving Show

parse :: String -> Instruction
parse (oper:v) = flip Instruction (read v) $ oper & \case
  'N' -> Fac North
  'E' -> Fac East
  'S' -> Fac South
  'W' -> Fac West
  'F' -> Forward
  'L' -> Rot RotLeft
  'R' -> Rot RotRight

run :: (Instruction -> State Ship a) -> [Instruction] -> State Ship Int
run st is = do
  mapM_ st is
  uses #location $ sum . (^.. both . to abs)

step' :: Instruction -> Ship -> Ship
step' = execState . step

-- dirOf :: Facing -> (Int, Lens' (Int, Int) Int)
dirOf North = (1, _1)
dirOf South = (-1, _1)
dirOf East = (1, _2)
dirOf West = (-1, _2)

step :: Instruction -> State Ship ()
step (Instruction ope v) = case ope of
  Fac d -> let (sign, dir) = dirOf d in #location . dir += sign * v
  Forward -> do
    fac <- use #facing
    let (sign, dir) = dirOf fac
    #location . dir += sign * v
  Rot r -> #facing %= changeFacing r v

step2 :: Instruction -> State Ship ()
step2 (Instruction ope v) = case ope of
  Fac d -> let (sign, dir) = dirOf d in #waypoint . dir += sign * v
  Forward -> do
    let add :: Lens' (Int, Int) Int -> State Ship ()
        add l = use (#waypoint . l) >>= \w -> #location . l += v * w
    add _1
    add _2
  Rot r -> rotWaypoint r v

-- protolude has this and I think it can look nicer than pure ()
-- It also will make python programmers feel nice and cozy
pass :: Applicative f => f ()
pass = pure ()

rotWaypoint :: Rotation -> Int -> State Ship ()
rotWaypoint RotLeft = rotWaypoint RotRight . negate
rotWaypoint RotRight = (`mod` 360) >>> \case
  0   -> pass
  90  -> swap negate id
  180 -> #waypoint . both %= negate
  270 -> swap id negate
  where swap f g = do
          wp <- use #waypoint
          let go a b c = #waypoint . a .= b (wp ^. c)
          go _1 f _2
          go _2 g _1

changeFacing :: Rotation -> Int -> Facing -> Facing
changeFacing rot v fac = toEnum ((fromEnum fac + sign * v `div` 90) `mod` 4)
  where sign = case rot of
          RotRight -> 1
          RotLeft -> -1

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input"
  let runWith st = print . evalState (run st input) $ Ship East (0, 0) (1, 10)
  runWith step
  runWith step2
  -- mapM_ print . scanl' (flip step') (Ship East (0, 0)) $ input
