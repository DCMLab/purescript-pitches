module Data.Pitches.Spelled
  ( class Spelled
  , fifths
  , octaves
  , internalOctaves
  , degree
  , generic
  , diasteps
  , alteration
  , SInterval(..)
  , spelled
  , wholetone
  , onlyDia
  , spelledDiaChrom
  , second
  , third
  , fourth
  , tritone
  , fifth
  , sixth
  , seventh
  , SIC(..)
  , sic
  , second'
  , third'
  , fourth'
  , tritone'
  , fifth'
  , sixth'
  , seventh'
  , letter
  , SPitch
  , spelledp
  , SPC
  , spc
  , Accidental
  , flt
  , shp
  , nat
  , c
  , d
  , e
  , f
  , g
  , a
  , b
  , c'
  , d'
  , e'
  , f'
  , g'
  , a'
  , b'
  ) where

import Prelude hiding (degree)
import Control.Alt ((<|>))
import Data.Char (fromCharCode, toCharCode)
import Data.Either (hush)
import Data.Group (class Group, ginverse)
import Data.Group (power) as G
import Data.List.NonEmpty (length)
import Data.Maybe (fromMaybe)
import Data.Monoid (power) as M
import Data.Ord (abs)
import Data.Pitches.Class (class Chromatic, class Diatonic, class HasIntervalClass, class Interval, class IntervalClassOf, class ParseNotation, class ParsePitchNotation, class ShowPitch, class ToMidi, class ToMidiPitch, ImperfectInterval(..), Pitch(..), aug, chromaticSemitone, direction, down, iabs, ic, toMidi, (+^), (^*), (^-^))
import Data.Pitches.Internal (parseInt, parseInt')
import Data.String as S
import Test.QuickCheck (class Arbitrary, arbitrary)
import Text.Parsing.StringParser (Parser, fail, runParser) as P
import Text.Parsing.StringParser.CodePoints (char, oneOf) as P
import Text.Parsing.StringParser.Combinators (many1, option) as P

-- Spelled Intervals
-- -----------------
-- 
-- helpers
isPerfect :: Int -> Boolean
isPerfect = case _ of
  0 -> true
  3 -> true
  4 -> true
  _ -> false

accstr :: Int -> String -> String -> String
accstr n lu ld
  | n == 0 = ""
  | n > 0 = M.power lu n
  | otherwise = M.power ld (abs n)

qualpf :: Int -> String -> String -> String -> String
qualpf n la lp ld
  | n > 0 = M.power la n
  | n == 0 = lp
  | otherwise = M.power ld (abs n)

qualimpf :: Int -> String -> String -> String -> String -> String
qualimpf n la lmj lmn ld
  | n > 0 = M.power la n
  | n == 0 = lmj
  | n == -1 = lmn
  | otherwise = M.power ld ((-n) - 1)

fifths2degree :: Int -> Int
fifths2degree fifths = (fifths * 4) `mod` 7

-- | A common class for spelled accessors.
class Spelled i where
  fifths :: i -> Int
  octaves :: i -> Int
  internalOctaves :: i -> Int
  degree :: i -> Int
  generic :: i -> Int
  diasteps :: i -> Int
  alteration :: i -> Int

---------------
-- SInterval --
---------------
newtype SInterval
  = SInterval { fifths :: Int, octaves :: Int }

derive instance eqSInterval :: Eq SInterval

instance arbitrarySInterval :: Arbitrary SInterval where
  arbitrary = spelled <$> map (_ `mod` 100) arbitrary <*> map (_ `mod` 100) arbitrary

-- smart constructors
--
spelled :: Int -> Int -> SInterval
spelled fifths octaves = SInterval { fifths, octaves }

wholetone :: SInterval
wholetone = spelled 2 (-1)

onlyDia :: Int -> SInterval
onlyDia x = G.power wholetone x <> ginverse (G.power chromaticSemitone (2 * x))

spelledDiaChrom :: Int -> Int -> SInterval
spelledDiaChrom dia chrom = diaPart <> chromPart
  where
  diaPart = wholetone `G.power` dia

  chromPart = chromaticSemitone `G.power` (chrom - 2 * dia)

second :: ImperfectInterval SInterval
second = Impf (spelled 2 (-1) ^-^ _)

third :: ImperfectInterval SInterval
third = Impf (spelled 4 (-2) ^-^ _)

fourth :: SInterval
fourth = spelled (-1) 1

tritone :: SInterval
tritone = aug fourth

fifth :: SInterval
fifth = spelled 1 0

sixth :: ImperfectInterval SInterval
sixth = Impf (spelled 3 (-1) ^-^ _)

seventh :: ImperfectInterval SInterval
seventh = Impf (spelled 5 (-2) ^-^ _)

-- TODO: interval special values (second, third, etc)
--
instance spelledSInterval :: Spelled SInterval where
  fifths (SInterval i) = i.fifths
  octaves (SInterval i) = i.octaves + ((i.fifths * 4) `div` 7)
  internalOctaves (SInterval i) = i.octaves
  degree (SInterval i) = fifths2degree i.fifths
  generic i
    | direction i == LT = -(degree $ down i)
    | otherwise = degree i
  diasteps (SInterval i) = i.fifths * 4 + i.octaves * 7
  alteration i = (fifths (iabs i) + 1) `div` 7

instance semigroupSInterval :: Semigroup SInterval where
  append (SInterval i1) (SInterval i2) = spelled (i1.fifths + i2.fifths) (i1.octaves + i2.octaves)

instance monoidSInterval :: Monoid SInterval where
  mempty = spelled 0 0

instance groupSInterval :: Group SInterval where
  ginverse (SInterval i) = spelled (-i.fifths) (-i.octaves)

instance ordSInterval :: Ord SInterval where
  compare i1 i2 = compare [ diasteps i1, alteration i1 ] [ diasteps i2, alteration i2 ]

instance intervalSInterval :: Interval SInterval where
  octave = spelled 0 1
  direction i = compare (diasteps i) 0
  iabs i
    | direction i == LT = down i
    | otherwise = i

instance hasintervalclassSInterval :: HasIntervalClass SInterval SIC where
  ic (SInterval i) = sic i.fifths

instance intervalclassofSInterval :: IntervalClassOf SIC SInterval where
  emb (SIC fs) = spelled fs (negate $ (fs * 4) `div` 7)

instance diatonicSInterval :: Diatonic SInterval where
  isStep i = abs (diasteps i) < 2

instance chromaticSInterval :: Chromatic SInterval where
  chromaticSemitone = spelled 7 (-4)

instance tomidiSInterval :: ToMidi SInterval where
  toMidi (SInterval i) = i.fifths * 7 + i.octaves * 12

instance showSInterval :: Show SInterval where
  show i
    | direction i == LT = "-" <> show (down i)
    | otherwise = qual <> dia <> ":" <> octs
      where
      deg = degree i

      dia = show $ deg + 1

      alt = alteration i

      qual =
        if isPerfect deg then
          qualpf alt "a" "P" "d"
        else
          qualimpf alt "a" "M" "m" "d"

      octs = show $ octaves i

---------
-- SIC --
---------
newtype SIC
  = SIC Int

derive instance eqSIC :: Eq SIC

derive instance ordSIC :: Ord SIC

instance arbitrarySIC :: Arbitrary SIC where
  arbitrary = SIC <<< (_ `mod` 100) <$> arbitrary

sic :: Int -> SIC
sic = SIC

second' :: ImperfectInterval SIC
second' = Impf (sic 2 ^-^ _)

third' :: ImperfectInterval SIC
third' = Impf (sic 4 ^-^ _)

fourth' :: SIC
fourth' = sic (-1)

tritone' :: SIC
tritone' = sic 6

fifth' :: SIC
fifth' = sic 1

sixth' :: ImperfectInterval SIC
sixth' = Impf (sic 3 ^-^ _)

seventh' :: ImperfectInterval SIC
seventh' = Impf (sic 5 ^-^ _)

instance spelledSIC :: Spelled SIC where
  fifths (SIC fs) = fs
  octaves _ = 0
  internalOctaves _ = 0
  degree (SIC fs) = fifths2degree fs
  generic (SIC fs) = fifths2degree fs
  diasteps (SIC fs) = fifths2degree fs
  alteration (SIC fs) = (fs + 1) `div` 7

instance semigroupSIC :: Semigroup SIC where
  append (SIC i1) (SIC i2) = sic $ i1 + i2

instance monoidSIC :: Monoid SIC where
  mempty = sic 0

instance groupSIC :: Group SIC where
  ginverse (SIC fs) = sic (-fs)

instance intervalSIC :: Interval SIC where
  octave = sic 0
  direction i = if dia == 0 then EQ else if dia < 4 then GT else LT
    where
    dia = diasteps i
  iabs i
    | direction i == LT = down i
    | otherwise = i

instance hasintervalclassSIC :: HasIntervalClass SIC SIC where
  ic i = i

instance diatonicSIC :: Diatonic SIC where
  isStep i = deg == 0 || deg == 1 || deg == 6
    where
    deg = degree i

instance chromaticSIC :: Chromatic SIC where
  chromaticSemitone = sic 7

instance tomidiSIC :: ToMidi SIC where
  toMidi (SIC fs) = (fs * 7) `mod` 12

instance showSIC :: Show SIC where
  show i = qual <> show (dia + 1)
    where
    dia = diasteps i

    alt = alteration i

    qual =
      if isPerfect dia then
        qualpf alt "a" "P" "d"
      else
        qualimpf alt "a" "M" "m" "d"

-- spelled pitch
-- -------------
instance spelledPitch :: (Spelled i, HasIntervalClass i ic, Spelled ic) => Spelled (Pitch i) where
  fifths (Pitch i) = fifths i
  octaves (Pitch i) = octaves i
  internalOctaves (Pitch i) = internalOctaves i
  degree (Pitch i) = degree i
  generic (Pitch i) = generic i
  diasteps (Pitch i) = diasteps i
  alteration (Pitch i) = alteration $ ic i

asciiA :: Int
asciiA = toCharCode 'A'

letter :: forall i. Spelled i => i -> String
letter i = S.singleton $ S.codePointFromChar $ fromMaybe 'X' $ fromCharCode $ asciiA + ((degree i + 2) `mod` 7)

type SPitch
  = Pitch SInterval

type SPC
  = Pitch SIC

spelledp :: Int -> Int -> SPitch
spelledp fs os = Pitch $ spelled fs os

newtype Accidental
  = Acc Int

flt :: Accidental
flt = Acc (-1)

shp :: Accidental
shp = Acc 1

nat :: Accidental
nat = Acc 0

toSpelled :: Int -> Int -> Accidental -> Int -> SPitch
toSpelled fs os (Acc acc) oct = spelledp fs (os + oct) +^ (chromaticSemitone ^* acc)

c :: Accidental -> Int -> SPitch
c = toSpelled 0 0

d :: Accidental -> Int -> SPitch
d = toSpelled 2 (-1)

e :: Accidental -> Int -> SPitch
e = toSpelled 4 (-2)

f :: Accidental -> Int -> SPitch
f = toSpelled (-1) 1

g :: Accidental -> Int -> SPitch
g = toSpelled 1 0

a :: Accidental -> Int -> SPitch
a = toSpelled 3 (-1)

b :: Accidental -> Int -> SPitch
b = toSpelled 5 (-2)

instance showpitchSInterval :: ShowPitch SInterval where
  showPitch i = letter p <> accs <> show (octaves p)
    where
    p = Pitch i

    accs = accstr (alteration p) "♯" "♭"

instance tomidiSPitch :: ToMidiPitch SInterval where
  toMidiPitch i = toMidi i + 12

spc :: Int -> SPC
spc fs = Pitch $ sic fs

toSPC :: Int -> Accidental -> SPC
toSPC fs (Acc acc) = spc fs +^ (chromaticSemitone ^* acc)

c' :: Accidental -> SPC
c' = toSPC 0

d' :: Accidental -> SPC
d' = toSPC 2

e' :: Accidental -> SPC
e' = toSPC 4

f' :: Accidental -> SPC
f' = toSPC (-1)

g' :: Accidental -> SPC
g' = toSPC 1

a' :: Accidental -> SPC
a' = toSPC 3

b' :: Accidental -> SPC
b' = toSPC 5

instance showpitchSIC :: ShowPitch SIC where
  showPitch i = letter p <> accs
    where
    p = Pitch i

    accs = accstr (alteration p) "♯" "♭"

instance tomidiSPC :: ToMidiPitch SIC where
  toMidiPitch i = toMidi i + 60

-------------
-- parsing --
-------------
altAug :: P.Parser (Boolean -> P.Parser Int)
altAug = do
  as <- P.many1 $ P.char 'a'
  pure $ \_ -> pure $ length as

altDim :: P.Parser (Boolean -> P.Parser Int)
altDim = do
  ds <- P.many1 $ P.char 'd'
  pure $ \pf -> pure $ negate (length ds) - if pf then 0 else 1

altQual :: P.Parser (Boolean -> P.Parser Int)
altQual = do
  qual <- P.oneOf [ 'M', 'P', 'm' ]
  case qual of
    'P' -> pure $ \pf -> if pf then pure 0 else P.fail "used P on an imperfect interval"
    'M' -> pure $ \pf -> if pf then P.fail "used M on a perfect interval" else pure 0
    'm' -> pure $ \pf -> if pf then P.fail "used m on a perfect interval" else pure (-1)
    q -> P.fail $ "unknown qualifier " <> show q

parseDia :: P.Parser Int
parseDia = do
  falt <- altAug <|> altDim <|> altQual
  dia <- (\x -> x - 1) <$> parseInt'
  alt <- falt $ isPerfect dia
  pure $ ((dia * 2 + 1) `mod` 7) - 1 + (7 * alt)

instance parsenotationSInterval :: ParseNotation SInterval where
  parseNotation str = hush $ P.runParser parser str
    where
    parser = do
      sign <- P.option '+' $ P.char '-'
      fs <- parseDia
      _ <- P.char ':'
      os <- parseInt
      let
        i = SInterval { fifths: fs, octaves: os - ((fs * 4) `div` 7) }
      pure $ if sign == '-' then down i else i

instance parsenotationSIC :: ParseNotation SIC where
  parseNotation str = hush $ P.runParser parser str
    where
    parser = do
      sign <- P.option '+' (P.char '-')
      i <- sic <$> parseDia
      pure $ if sign == '-' then down i else i

parseAccs :: P.Parser Int
parseAccs = P.option 0 $ sharps <|> flats
  where
  munch = P.many1 <<< P.char

  sharps = length <$> (munch '♯' <|> munch '#')

  flats = negate <<< length <$> (munch '♭' <|> munch 'b')

parseName :: P.Parser Int
parseName = do
  name <- P.oneOf [ 'A', 'B', 'C', 'D', 'E', 'F', 'G' ]
  let
    dia = (toCharCode name - asciiA - 2) `mod` 7
  acc <- parseAccs
  pure $ ((dia * 2 + 1) `mod` 7) - 1 + 7 * acc

instance parsenotationSPitch :: ParsePitchNotation SInterval where
  parsePitchNotation str = hush $ P.runParser parser str
    where
    parser = do
      fs <- parseName
      os <- parseInt
      pure $ spelledp fs (os - ((fs * 4) `div` 7))

instance parsenotationSPC :: ParsePitchNotation SIC where
  parsePitchNotation str = hush $ P.runParser (spc <$> parseName) str
