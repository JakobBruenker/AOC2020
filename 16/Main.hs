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
import Data.Bifunctor as BF
import Data.Char
import Data.Coerce
import Data.Either
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

data Bounds = Bounds { lo :: Int
                     , hi :: Int
                     } deriving (Show, Generic)

data Range = Range { lo :: Bounds
                   , hi :: Bounds
                   } deriving (Show, Generic)

type Field = (String, Range)

type Ticket = [Int]

parseBounds :: String -> Bounds
parseBounds (splitOn "-" -> [read -> low, read -> high]) = Bounds low high

parseField :: String -> Field
parseField (splitOn ": " -> [n, map parseBounds . splitOn " or " -> [low, high]])
  = (n, Range low high)

parseTicket :: String -> [Int]
parseTicket = map read . splitOn ","

inRange :: Field -> Int -> Bool
inRange (_, Range low high) x = inBounds low x || inBounds high x

inBounds :: Bounds -> Int -> Bool
inBounds (Bounds low high) x = x >= low && x <= high

inAnyRange :: [Field] -> Int -> Bool
inAnyRange fields = (any ?? inRange <$> fields) . (&)

isValid :: [Field] -> Ticket -> Bool
isValid = all . inAnyRange

possible :: [Field] -> Ticket -> [Set.Set String]
possible fields = map (\n -> Set.fromList . map fst $ filter (inRange ?? n) fields)

manifestSingle :: forall a . Ord a => [Either (Set.Set a) a]
               -> Maybe [Either (Set.Set a) a]
manifestSingle sets = go <$> singlet
  where
    singlet :: Maybe a
    singlet = Set.findMax . fromLeft Empty
      <$> find (either ((1 ==) . Set.size) (const False)) sets

    manifest :: a -> [Either (Set.Set a) a] -> [Either (Set.Set a) a]
    manifest x = map (\set -> if set == Left (Set.singleton x) then Right x else set)

    removeSingle :: a -> [Either (Set.Set a) a] -> [Either (Set.Set a) a]
    removeSingle x = map . BF.first $ Set.delete x

    go x = removeSingle x . manifest x $ sets

main :: IO ()
main = do
  [  map parseField         -> fields
   , parseTicket . last     -> myTicket
   , map parseTicket . tail -> tickets
   ] <- map lines . splitOn "\n\n" <$> readFile "input"
  print . sum . filter (not . inAnyRange fields) $ concat tickets
  let validTickets = filter (isValid fields) tickets
      possibleFields = map (possible fields) validTickets
      probableFields =
        foldl1' (zipWith Set.intersection) possibleFields
      ticketFields = map (fromRight "") . fromJust . last . takeWhile isJust .
        iterate (>>= manifestSingle) . Just $ map Left probableFields
      myTicketFilled = zip ticketFields myTicket
  print . product . map snd . filter (isPrefixOf "departure" . fst) $ myTicketFilled
