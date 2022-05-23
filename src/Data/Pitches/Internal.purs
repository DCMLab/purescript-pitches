module Data.Pitches.Internal (parseInt, parseInt', readJSONviaParse) where

import Prelude
import Data.Char as C
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Pitches.Class (class Notation, parseNotation)
import Foreign as F
import Simple.JSON (readImpl)
import StringParser (Parser) as P
import StringParser.CodePoints (anyDigit, char) as P
import StringParser.Combinators (many1, option) as P

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

readJSONviaParse :: forall a. Notation a => String -> F.Foreign -> F.F a
readJSONviaParse typ input = do
  str <- readImpl input
  case parseNotation str of
    Nothing -> F.fail $ F.ForeignError $ "Cannot parse " <> str <> " as " <> typ <> "!"
    Just i -> pure i
