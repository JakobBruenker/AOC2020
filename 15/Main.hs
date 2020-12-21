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
           , NegativeLiterals
           , RankNTypes
           , OverloadedLabels
           , DeriveGeneric
           , NamedWildCards
           , TypeApplications
           , DuplicateRecordFields
           , DisambiguateRecordFields
           , FlexibleInstances
           , PatternSynonyms
           , NumDecimals
#-}

import Control.Applicative
import Control.Arrow
import Control.Comonad.Store
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor
import Data.Char
import Data.Coerce
import Data.Functor
import Data.Generics.Labels ()
import Data.Kind
import Data.IntMap qualified as IM
import Data.List hiding (uncons)
import Data.List.Split
import Data.STRef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Profunctor
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Debug.Trace
import GHC.Generics (Generic)
import Text.Read hiding (step)
import Data.Word
import Data.Bits
import Data.Bits.Lens
import Numeric.Lens

-- the bangs are very important
data Memory = Memory { turn :: !Int
                     , prev :: !Int
                     , latest :: !(IM.IntMap Int)
                     } deriving (Show, Generic)

main :: IO ()
main = do
  input <- map (read @Int) . splitOn "," . init <$> readFile "input"
  print . evalState (runTo 2020) $ initial input
  print . evalState (runTo 3e7) $ initial input

runTo :: Int -> State Memory Int
runTo t = do
  t' <- use #turn
  -- if you need progress bar
  -- if t' `mod` 100000 == 0 then traceShow t' $ pure () else pure ()
  if t' >= t
     then use #prev
     else step >> runTo t

initial :: [Int] -> Memory
initial xs = execState (mapM_ go xs) (Memory 0 0 Empty)
  where
    go :: Int -> State Memory ()
    go x = do
      p <- #prev <<.= x
      (#latest . at p .=) . Just =<< use #turn
      #turn += 1

step :: State Memory ()
step = do
  t <- #turn <<+= 1
  p <- use #prev
  ls <- use #latest
  #latest . at p .= Just t
  #prev .= maybe 0 (t -) (ls ^. at p)

forC :: Monad m => (m (), m Bool, m ()) -> m () -> m ()
forC (init, while, post) body = init >> go
  where go = while >>= (when ?? (body >> post >> go))

pass :: Applicative f => f ()
pass = pure ()

-- C SOLUTION
-- you can definitely do this a lot better than this mess in Haskell
-- this is just C translated as literally as possible (and literally s/IO/ST/g)
main' :: IO ()
main' = do
  initial' <- map (read @Int) . splitOn "," . init <$> readFile "input"
  print $ runST (result initial')

result :: [Int] -> forall s . ST s Int
result initial' = do
  let maxStep = 3e7
  latest' <- V.thaw $ V.replicate maxStep 0
  turn' <- newSTRef 0
  prev' <- newSTRef 0
  forC (pass, readSTRef turn' <&> (< length initial'), modifySTRef turn' (+1)) do
    prev'' <- readSTRef prev'
    turn'' <- readSTRef turn'
    VM.write latest' prev'' turn''
    writeSTRef prev' $ initial' !! turn''
  forC (pass, readSTRef turn' <&> (< maxStep), modifySTRef turn' (+1)) do
    prev'' <- readSTRef prev'
    turn'' <- readSTRef turn'
    pprev' <- VM.read latest' prev''
    VM.write latest' prev'' turn''
    writeSTRef prev' if pprev' /= 0 then turn'' - pprev' else 0
  readSTRef prev'
