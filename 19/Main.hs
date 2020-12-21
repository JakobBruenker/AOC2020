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

-- Bool says if it's required
data Rule = Compound [[Int]]
          | Literal Char
          deriving (Show, Read)

-- hmm notice something? I just recreated ReadS
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser g) = Parser $ (fmap . fmap . first) f g

instance Applicative Parser where
  pure x = Parser $ pure . (x,)
  Parser ff <*> Parser fx = Parser $ ff >=> \(f, str) -> first f <$> fx str

-- backtracking is built in baby
instance Alternative Parser where
  empty = Parser $ const []
  Parser f <|> Parser g = Parser $ liftA2 (<>) f g

parseRule :: String -> (Int, Rule)
parseRule (reads -> [(n, ':':str)]) = (n,) case words str of
  [['"',c,'"']] -> Literal c
  (splitOn [ "|" ] -> rhss) -> Compound $ (map . map) read rhss

-- hypothesis: For some parse trees, we succeed but and up having something
-- left in the input string. When we say eof afterwards, the parser failed,
-- even though another parse tree may not have had anything left in its input
-- Idea: don't just generate one solution in a Parser, instead generate the
-- list of all solutions
-- Turns out, this hypothesis was correct
-- keyword: unbiased choice. To get this to work with biased choice, you'd have
-- to convert all `(a <|> b) *> c` terms with `(a *> c) <|> (b *> c)` (and
-- possibly some `try`s.)
-- Apparently you can structure your parser like this relatively easily with
-- Codensity, but the details of this are currently beyond me.
main :: IO ()
main = do
  [IM.fromList . map parseRule -> grammar, input] <-
    map lines . splitOn "\n\n" <$> readFile "input"
  let run grammar =
        print . length $ mapMaybe (parse $ (makeParsers grammar IM.! 0) *> eof) input
  run grammar
  run . IM.insert 8 (Compound [[42], [42, 8]])
      . IM.insert 11 (Compound [[42, 31], [42, 11, 31]])
      $ grammar

eof :: Parser ()
eof = Parser \case
  "" -> [((),"")]
  _ -> []

parse :: Parser a -> String -> Maybe a
parse p = fmap fst . listToMaybe . runParser p

löb :: Functor f => f (f a -> a) -> f a
löb x = fix $ (x <&>) . (&)
-- löb x = fmap ($ löb x) x
-- ^ this version is worse sharing-wise - note how you're calling löb x but
-- then löb x has to be evaluated again inside the function itself
-- same reason why fix is not implemented as `fix f = f (fix f)`

-- Ayy the first time I'm using löb!
-- Btw it is genuinely useful here, not *just* doing this for fun, since it's
-- much easier to generate test parsers in ghci if you don't have to deal with
-- the Rules IntMap separately and can just use this function directly
-- Perhaps even more importantly, it means we convert each rule only once
makeParsers :: IM.IntMap Rule -> IM.IntMap (Parser String)
makeParsers = löb . fmap \rule parsers -> case rule of
  Literal c -> Parser \case
    (c':r) | c == c' -> [([c], r)]
    _ -> []
  Compound rhss -> asum (map (fmap concat . traverse (parsers IM.!)) rhss)

-- My parsec solution doesn't work in part two for reasons I don't know, so I'm
-- rolling my own parser now

-- Bool says if it's required
-- data Rule = Compound [[(Bool, Int)]]
--           | Literal Char
--           deriving (Show, Read)

-- parseRule :: String -> (Int, Rule)
-- parseRule (reads -> [(n, prefix ": " -> Just str)]) = (n,) case words str of
--   [['"',c,'"']] -> Literal c
--   (splitOn [ "|" ] -> rhss) -> Compound $ map (map $ (True,) . read) rhss

-- main :: IO ()
-- main = do
--   [IM.fromList . map parseRule -> grammar, input] <-
--     map lines . splitOn "\n\n" <$> readFile "inputTest"
--   let zero = makeParser grammar $ grammar IM.! 0
--   print . length . rights $ map (parse (zero >> eof) "input") input
--   let grammar' = IM.insert 8 (Compound [[(True, 42), (False, 8)]]) $
--                  IM.insert 11 (Compound [[(True, 42), (False, 11), (True, 31)]]) $
--                  grammar
--   let zero' = makeParser grammar' $ grammar' IM.! 0
--   print . length . rights $ map (parse (zero' >> eof) "input") input
--   mapM_ (\str -> putStrLn str >> parseTest (zero' >> eof) str >> putStrLn (replicate 80 '-')) input

-- -- could improve this by using löb I guess
-- -- I did say I want to find ways to use löb...
-- makeParser :: IM.IntMap Rule -> Rule -> Parser String
-- makeParser _ (Literal c) = pure <$> char c
-- makeParser rules (Compound rhss) = asum (map try parsers)
--   where
--     parsers :: [Parser String]
--     parsers = map (fmap concat . sequence . map lookupParser) rhss
--     lookupParser :: (Bool, Int) -> Parser (String)
--     lookupParser (req, i) = fmap (fromMaybe "") $
--       (if req then fmap Just else optional) (makeParser rules $ rules IM.! i)
