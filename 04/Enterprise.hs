{-# OPTIONS_GHC -Wall -Wincomplete-uni-patterns #-}
{-# LANGUAGE Haskell2010
           , OverloadedStrings
           , DerivingVia
           , LambdaCase
#-}

import Data.Char
import Data.Word
import Data.Bifunctor
import Control.Monad
-- import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Text.Read (readMaybe)

newtype Year = MkYear Int
  deriving (Eq, Ord, Show) via Int

data Height = Centimeters Int
            | Inches Int
            deriving (Show)

data HairColor = HCRGB Word8 Word8 Word8 deriving Show

data EyeColor = Amber
              | Blue
              | Brown
              | Grey
              | Green
              | Hazel
              | Other
              deriving (Show)

data Passport = MkPassport { birth :: Year
                           , issue :: Year
                           , expiration :: Year
                           , height :: Height
                           , hair :: HairColor
                           , eyes :: EyeColor
                           , pid :: Int
                           , cid :: Maybe Int
                           } deriving (Show)

type Parser = Parsec Void Text

infixl 1 =>>

(=>>) :: Monad m => m a -> (a -> m b) -> m a
m =>> f = m >>= \x -> f x >> pure x

failOutOfBounds :: (Ord a, Show a) => String -> a -> a -> a -> Parser ()
failOutOfBounds lbl lo hi val = do
  when (val < lo) $ fail $ lbl <> " must be at least " <> show lo
  when (val > hi) $ fail $ lbl <> " must be at most " <> show hi

yearBetween :: String -> Year -> Year -> Parser Year
yearBetween lbl lo hi = MkYear <$> L.decimal =>> failOutOfBounds lbl lo hi

pBirth :: Parser Year
pBirth = yearBetween "Birth year" (MkYear 1920) (MkYear 2002)

pIssue :: Parser Year
pIssue = yearBetween "Issue year" (MkYear 2010) (MkYear 2020)

pExpiration :: Parser Year
pExpiration = yearBetween "Expiration year" (MkYear 2020) (MkYear 2030)

pHeight :: Parser Height
pHeight = try (parseUnit "Height in centimeters" "cm" Centimeters 150 193)
      <|>      parseUnit "Height in inches"      "in" Inches      59  76
  where
    parseUnit
      :: (Num a, Ord a, Show a) => String -> Text -> (a -> b) -> a -> a -> Parser b
    parseUnit lbl unit conv lo hi = conv <$>
      (L.decimal <* string unit =>> failOutOfBounds lbl lo hi)

pHair :: Parser HairColor
pHair = do
  _ <- char '#'
  -- is this acceptable? Could fail if programmer makes a mistake
  -- Would be better if count returned a Vec 3 a
  [r, g, b] <- count 3 pWord8
  pure $ HCRGB r g b
  where
    pWord8 :: Parser Word8
    pWord8 = do
      a <- hexDigitChar
      b <- hexDigitChar
      pure . fromIntegral $ digitToInt a * 16 + digitToInt b

eyeColors :: [(EyeColor, Text)]
eyeColors =
  [ (Amber, "amb")
  , (Blue , "blu")
  , (Brown, "brn")
  , (Grey , "gry")
  , (Green, "grn")
  , (Hazel, "hzl")
  , (Other, "oth")
  ]

pEyes :: Parser EyeColor
pEyes = choice . map (uncurry (<*) . bimap pure string) $ eyeColors

pPID :: Parser Int
pPID = do
  Just int <- readMaybe <$> count 9 digitChar
  pure int

pCID :: Parser Int
pCID = L.decimal

-- Not going to complete the rest of this tbh
