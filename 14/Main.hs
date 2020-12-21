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
import Text.Read hiding (step)
import Data.Word
import Data.Bits
import Data.Bits.Lens
import Numeric.Lens

data Trit = O | I | X deriving (Eq, Ord, Show)
data Mask = TritMask [Trit]
          | AOMask Int Int
          deriving (Show, Eq, Ord)
data Mem = Mem { address :: Int, value :: Int } deriving (Show, Eq, Ord, Generic)
data Computer = Computer { mask :: Mask, memory :: IM.IntMap Int }
              deriving (Generic, Show)

prefix :: String -> String -> Maybe String
prefix pfx str = let (p, rest) = splitAt (length pfx) str in
  if p == pfx then Just rest else Nothing

parse :: Bool -> String -> Either Mask Mem
parse trit (prefix "mask = " -> Just msk) = Left if
  | trit      -> TritMask . map toTrit $ reverse msk
  | otherwise -> AOMask (makeMask '1') (makeMask '0')
  where
    makeMask x = replaceX x msk ^?! binary
    replaceX x = map \c -> if c == 'X' then x else c
    toTrit = \case '0' -> O
                   '1' -> I
                   'X' -> X
parse _ (prefix "mem[" -> Just (reads -> [(addr, prefix "] = " -> Just (read -> val))])) =
  Right $ Mem addr val

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  forM_ [False ..] \b ->
    print . evalState (run $ map (parse b) input) $ Computer (TritMask []) Empty

applyTritMask :: [Trit] -> Int -> [Int]
applyTritMask msk addr = ifoldl' applyTrit [addr] msk

applyTrit :: Int -> [Int] -> Trit -> [Int]
applyTrit _ as O = as
applyTrit i as I = as & each . bitAt i .~ True
applyTrit i as X = [ a & bitAt i .~ b | b <- [False ..], a <- as ]

applyAOMask :: Int -> Int -> Int -> Int
applyAOMask am om val = (val .&. am) .|. om

step :: Either Mask Mem -> State Computer ()
step (Left msk) = #mask .= msk
step (Right (Mem addr val)) = use #mask >>= \case
  TritMask tm -> forM_ (applyTritMask tm addr) \ad ->
                   #memory %= IM.insert ad val
  AOMask am om -> #memory %= IM.insert addr (applyAOMask am om val)

run :: [Either Mask Mem] -> State Computer Int
run is = mapM_ step is >> uses #memory sum
