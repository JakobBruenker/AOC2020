{-# LANGUAGE ScopedTypeVariables, ViewPatterns, LambdaCase, BlockArguments
           , StandaloneKindSignatures, GADTs, TypeOperators, TupleSections #-}
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Bifunctor
import Data.Profunctor
import Data.Functor.Contravariant
import Data.List.Split
import Data.Kind
import Data.Coerce
import Control.Lens
import Numeric.Lens

parse :: String -> [Bool]
parse = map (`elem` "BR")

bin :: [Bool] -> Int
bin = foldl' (\acc x -> acc * 2 + fromEnum x) 0

main :: IO ()
main = do
  ids <- map (bin . parse) . lines <$> readFile "input"
  let maxid = maximum ids
  print maxid
  print . maximum . filter (not . (`elem` ids)) $ [0 .. maxid]

-- lensy:

parse' :: String -> Int
parse' = (^?! binary) . (intToDigit . fromEnum . (`elem` "BR") <$>)

main' :: IO ()
main' = do
  ids <- map parse' . lines <$> readFile "input"
  let maxid = maximum ids
  print maxid
  print . maximum . filter (not . (`elem` ids)) $ [0 .. maxid]

-- IRC has determined you can do this in a single pass through the list if you
-- keep track of mininum, maximum, and sum at once, calculate sum [minimum ..
-- aximum] in O(1) through gauss's trick, and then the difference between that
-- and the sum you kept track of is the answer

-- needlessly trying to make bin more pointfree:

foo :: (a -> b) -> (c -> d) -> (b -> d -> e) -> a -> c -> e
foo f g op x y = f x `op` g y

bin' :: [Bool] -> Int
bin' = foldl' (foo (*2) fromEnum (+)) 0
-- bin' = foldl' (unBaz . bicontramap (*2) fromEnum $ Baz (+)) 0
-- bin' = foldl' (unTest . bicontramap (*2) fromEnum $ Test (+)) 0
-- bin' = foldl' (curry . unBar . bicontramap (*2) fromEnum $ Bar (uncurry (+))) 0

newtype Test e r s = Test { unTest :: r -> s -> e }

instance Bicontravariant (Test e) where
  bicontramap f g (Test fun) = Test \b d -> fun (f b) (g d)

type Bicontravariant :: (Type -> Type -> Type) -> Constraint
class Bicontravariant p where
  bicontramap :: (b -> a) -> (d -> c) -> p a c -> p b d

newtype Foo p c a b = Foo { unFoo :: p a b -> c }

instance (Bifunctor p) => Bicontravariant (Foo p c) where
  bicontramap f g (Foo bicov) = Foo \bifun -> bicov (bimap f g bifun)

newtype Bar p q c a b = Bar { unBar :: q (p a b) c }

-- technically I think it's enough for q (p a b) to be a contravariant functor
-- but that gets awkward
instance (Bifunctor p, Profunctor q) => Bicontravariant (Bar p q c) where
  bicontramap f g (Bar bicov) = Bar (lmap (bimap f g) bicov)

newtype Baz p c a b = Baz { unBaz :: a `p` (b `p` c) }

-- looks like we actually need the Profunctor instance here
instance Profunctor p => Bicontravariant (Baz p c) where
  bicontramap f g (Baz bicov) = Baz (dimap f (lmap g) bicov)
