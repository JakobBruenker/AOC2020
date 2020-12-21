import Control.Applicative
import Data.List
import Control.Monad

-- foo l = head [ x * y | (x:ys) <- tails, y <- ys, x + y == 2020 ]

-- more efficient sol by dminuoso:
-- bar l = head [ x * y * z | (x:ys) <- tails l, (y:ys) <- tails ys, z <- ys, x + y + z == 2020 ]
-- It's sad how simplicity and efficiency are so often opposed to one another
-- Anyway I had something like this in mind but I kind of thought laziness
-- already would give me the benefits of that
-- guess I was wrong
-- I also wasn't sure how to do and didn't think about it much before I saw his
-- solution but I was thinking about indices
bar' = map product . filter ((== 2020) . sum) . com 3

main :: IO ()
main = print . liftA2 (,) foo bar . map read . lines =<< readFile "input"

foo :: [Int] -> Int
foo xs = head [ x * y | x <- xs, y <- xs, x + y == 2020 ]

bar :: [Int] -> Int
bar xs = head [ x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020 ]

comb 0 _ = [[]]
comb _ [] = []
comb n xs = [ y:ys | y:rest <- tails xs, ys <- comb (n-1) rest ]

com n l = sequence (take n $ tails l)
foo' = map product . filter ((== 2020) . sum) . com 2

newtype TailList a = TailList {unTailList :: [a]} deriving Show

instance Semigroup (TailList a) where
  (<>) = mappend

instance Monoid (TailList a) where
  mempty = TailList []
  mappend (TailList xs) (TailList ys) = TailList (xs ++ ys)

instance Functor TailList where
  fmap _ (TailList []) = TailList []
  fmap f (TailList (x:xs)) = TailList (f x : fmap f xs)

instance Applicative TailList where
  pure = TailList . pure
  (<*>) = ap

-- just messing around seeing if we can get a better monad instance
instance Monad TailList where
  TailList [] >>= f = TailList []
  TailList (x:xs) >>= f = f x <> (TailList xs >>= f)

golf=print.((,).g 2<*>g 3).tails.map read.lines=<<readFile"input"where g n=product.head.filter((==2020).sum).sequence.take n
