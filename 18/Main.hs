{-# LANGUAGE ScopedTypeVariables
           , ViewPatterns
           , LambdaCase
           , BlockArguments
           , StandaloneKindSignatures
           , GADTs
           , TypeOperators
           , ImportQualifiedPost
           , UndecidableInstances
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

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Comonad.Store
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
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec
import Data.Void

infixl 6 :+, :*
data Expr = Expr :+ Expr
          | Expr :* Expr
          | Lit Int
          deriving (Show, Read)

type Parser = Parsec Void String
type Fun r = forall f a list . (Applicative f, list ~ [Operator Parser Expr])
        => ((a -> f a) -> list -> [list]) -> r

exprP' :: Fun (Parser Expr)
exprP' f = makeExprParser (termP f) $ f pure
  [ InfixL $ (:+) <$ symbol "+", InfixL $ (:*) <$ symbol "*" ]

symbol :: String -> Parser String
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

termP :: Fun (Parser Expr)
termP f = lexeme (parens (exprP' f) <|> Lit <$> L.decimal)

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let run :: Fun (IO ())
      run f = let Right r = mapM (parse (exprP' f) "input") input
              in print . sum . map eval $ r
  run id
  run map

eval :: Expr -> Int
eval (Lit n) = n
eval (x :+ y) = eval x + eval y
eval (x :* y) = eval x * eval y

-- I started with megaparsec, gave up after I couldn't figure out the docs,
-- wrote a custom parser for part1, gave up on writing a custom parser for part
-- 2, went back to megaparsec and tried to figure out how to make makeExprParser
-- work with spaces for a while before giving up on that and just removing the
-- spaces from the input but then I saw in IRC that it has something to do with
-- lexeme and then it just took me a few minutes to fix it

-- -- parse right-associatively (but reverse first)
-- parse :: String -> Expr
-- parse [d] = Lit (digitToInt d)
-- parse (d:' ':'+':' ':r) = Lit (digitToInt d) :+ parse r
-- parse (d:' ':'*':' ':r) = Lit (digitToInt d) :* parse r
-- parse (')':r) = let (parse -> f, s) = extractParen r in case s of
--   "" -> f
--   ' ':'+':' ':r' -> f :+ parse r'
--   ' ':'*':' ':r' -> f :* parse r'
--   where
--     extractParen :: String -> (String, String)
--     extractParen r = go 1 "" r
--       where
--         go 0 p r = (reverse $ tail p, r)
--         go n p (')':r) = go (n + 1) (')':p) r
--         go n p ('(':r) = go (n - 1) ('(':p) r
--         go n p (c:r) = go n (c:p) r

-- main :: IO ()
-- main = do
--   input <- map (parse . reverse) . lines <$> readFile "input"
--   print . sum . map eval $ input
