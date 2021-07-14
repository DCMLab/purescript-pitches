module Data.Pitches.Internal where

import Prelude
import Data.Char as C
import Data.Foldable (foldl)
import Text.Parsing.StringParser as P
import Text.Parsing.StringParser.CodePoints as P
import Text.Parsing.StringParser.Combinators as P

ascii0 :: Int
ascii0 = C.toCharCode '0'

digit :: Char -> Int
digit d = C.toCharCode d - ascii0

parseInt :: P.Parser Int
parseInt = do
  signChar <- P.option '+' $ P.char '-'
  let
    sign = if signChar == '-' then -1 else 1
  dgts <- P.many1 P.anyDigit
  pure $ sign * foldl (\acc d -> 10 * acc + digit d) 0 dgts

parseInt' :: P.Parser Int
parseInt' = do
  dgts <- P.many1 P.anyDigit
  pure $ foldl (\acc d -> 10 * acc + digit d) 0 dgts
