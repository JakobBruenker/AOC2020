{-# LANGUAGE ScopedTypeVariables, ViewPatterns, LambdaCase, BlockArguments #-}
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Bifunctor
import Data.List.Split

parse :: String -> (String, String)
parse xs = case span (/= ':') xs of
  (name, ':' : value) -> (name, value)

req :: [(String, String -> Bool)]
req =
  [ ("byr", liftA2 (&&) (>= 1920) (<= 2002) . read)
  , ("iyr", liftA2 (&&) (>= 2010) (<= 2020) . read)
  , ("eyr", liftA2 (&&) (>= 2020) (<= 2030) . read)
  , ("hgt", \str -> case reads str of
      [(n, "cm")] -> n >= 150 && n <= 193
      [(n, "in")] -> n >= 59 && n <= 76
      _ -> False)
  , ("hcl", \case
      ('#':str) -> all isHexDigit str && length str == 6
      _ -> False)
  , ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
  , ("pid", liftA2 (&&) (all isDigit) ((== 9) . length))
  -- , ("cid", const True)
  ]

checkReq' :: [(String, String)] -> Bool
checkReq' = all (\(nam, val) -> maybe True ($val) (lookup nam req))

checkReq :: [(String, String)] -> Bool
checkReq xs = all ((`elem` map fst xs) . fst) req

main :: IO ()
main = do
  input <- map (map parse . concatMap words) . wordsBy null . lines <$> readFile "input"
  let go f = print . length . filter f $ input
  go checkReq
  go $ liftA2 (&&) checkReq checkReq'
