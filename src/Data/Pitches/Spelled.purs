-- | This module defines pitch and interval types for spelled pitch, i.e. Western notation.
-- | 
-- | Spelled pitches and intervals are the standard types of the Western music notation system.
-- | Unlike MIDI pitches, spelled pitches distinguish between enharmonically equivalent pitches
-- | such as `E♭` and `D♯`.
-- | Similarly, spelled intervals distinguish between intervals
-- | such as `m3` (minor 3rd) and `a2` (augmented second) that are enharmonically equivalent
-- | (and would be identical in the MIDI system, for example).
-- |
-- | ### Notation
-- |
-- | Intervals and pitches can be read from string notation using the `parse*` functions
-- | (`parseSpelled`, `parseSIC`, `parseSpelledP`, and `parseSIC`).
-- | To convert a pitch or interval into string notation, use `showNotation`.
-- |
-- | The notation for spelled interval classes consist of one or more letters
-- | that indicate the quality of the interval
-- | and a number between 1 and 7 that indicates the generic interval,
-- | e.g. `P1` for a perfect unison, `m3` for a minor 3rd, or `aa4` for a double augmented 4th.
-- |
-- | ```
-- | letter quality                  
-- | ------ -------------------------
-- | dd...  diminished multiple times
-- | d      diminished               
-- | m      minor                    
-- | P      perfect                  
-- | M      major                    
-- | a      augmented                
-- | aa...  augmented multiple times 
-- | ```
-- | 
-- | Spelled intervals have the same elements as intervals but additionally take a number of octaves,
-- | written a suffix `:n`, e.g. `P1:0` or `m3:20`.
-- | By default, intervals are directed upwards. Downwards intervals are indicated by a negative sign,
-- | e.g. `-M2:1` (a major 9th down).
-- | For interval classes, downward and upward intervals cannot be distinguish,
-- | so a downward interval is represented by its complementary upward interval:
-- | 
-- | ```purescript
-- | > parseSIC "-M3"
-- | (Just m6)
-- | 
-- | > down <$> parseSIC "M3"
-- | (Just m6)
-- | ```
-- |
-- | Pitch classes are written using an upper case letter for the base pitch
-- | followed by 0 or more accidentals (`#` or `♯` for sharps, `b` or `♭` for flats),
-- | e.g. `C`, `Eb`/`E♭`, `G###`/`G♯♯♯`.
-- | Only one type of accidental is allowed per pitch (also don't mix ascii and unicode symbols).
-- | `showNotation` will use the unicode symbols.
-- |
-- | Pitches are written like pitch classes but followed by an integer octave number,
-- | e.g. `Eb4` or `C#-2`.
-- | 
-- | ### Representations of Spelled Intervals
-- | 
-- | #### Fifths and Octaves
-- | 
-- | Internally, spelled intervals are represented by, 5ths and octaves.
-- | Both dimensions are logically dependent:
-- | a major 2nd up is represented by going two 5ths up and one octave down.
-- | ```purescript
-- | > spelled 2 (-1) -- two 5ths up, one octave down
-- | M2:0
-- | ```
-- | This representation is convenient for arithmetics, which can usually be done component-wise.
-- | However, special care needs to be taken when converting to other representations.
-- | For example, the notated octave number (e.g. `:0` in `M2:0`)
-- | does *not* correspond to the internal octaves of the interval (-1 in this case).
-- | In the notation, the interval class (`M2`) and the octave (`:0`) are *independent*.
-- | 
-- | Interpreting the "internal" (or dependent) octave dimension of the interval
-- | does not make much sense without looking at the fifths.
-- | Therefore, the function `octaves` returns the "external" (or independent) octaves
-- | as used in the string representation, e.g.
-- | ```purescript
-- | > octaves <$> parseSpelled "M2:0"
-- | (Just 0)
-- | 
-- | > octaves <$> parseSpelled "M2:1"
-- | (Just 1)
-- | 
-- | > octaves <$> parseSpelled "M2:-1"
-- | (Just -1)
-- | ```
-- | If you want to look at the internal octaves, use `internalOctaves`
-- | This corresponds to looking directly at the octaves field in the representation of the interval
-- | but works on interval classes too (returning 0).
-- | 
-- | #### Diatonic Steps and Alterations
-- | 
-- | We provide a number of convenience functions to derive other properties from this representation.
-- | The generic interval (i.e. the number of diatonic steps) can be obtained using `generic`.
-- | `generic` respects the direction of the interval but is limitied to a single octave (0 to ±6).
-- | If you need the total number of diatonic steps, including octaves, use `diasteps`.
-- | The function `degree` returns the scale degree implied by the interval relative to some root.
-- | Since scale degrees are always above the root, `degree`,
-- | it treats negative intervals like their positive complements:
-- | ```purescript
-- | > generic <$> parseSpelled "-M3:1" -- some kind of 3rd down
-- | (Just -2)
-- | 
-- | > diasteps <$> parseSpelled "-M3:1" -- a 10th down
-- | (Just -9)
-- | 
-- | > degree <$> parseSpelled "-M3:1" -- scale degree VI
-- | (Just 5)
-- | ```
-- | For interval classes, all three functions are equivalent.
-- | Note that all three measures start counting from 0 (for unison/I), not 1.
-- | 
-- | Complementary to the generic interval functions,
-- | `alteration` returns the specific quality of the interval.
-- | For perfect or major intervals, it returns `0`.
-- | Larger absolute intervals return positive values,
-- | smaller intervals return negative values.
-- | 
-- | `degree` and `alteration` also work on pitches.
-- | `degree p` returns an integer corresponding to the letter (C=`0`, D=`1`, ...),
-- | while `alteration p` provides the accidentals (natural=`0`, sharps -> positive, flats -> negative).
-- | For convenience, `letter p` returns the letter of `p` as an uppercase character.
module Data.Pitches.Spelled
  ( class Spelled -- general interface for spelled types
  , fifths
  , octaves
  , internalOctaves
  , degree
  , generic
  , diasteps
  , alteration
  , SInterval(..) -- spelled intervals
  , parseSpelled
  , spelled
  , spelledDiaChrom
  , onlyDia
  , wholetone
  , second
  , third
  , fourth
  , tritone
  , fifth
  , sixth
  , seventh
  , SIC(..) -- spelled interval classes
  , parseSIC
  , sic
  , second'
  , third'
  , fourth'
  , tritone'
  , fifth'
  , sixth'
  , seventh'
  , letter -- pitches (general)
  , Accidental
  , flt
  , nat
  , shp
  , SPitch -- spelled pitches
  , parseSpelledP
  , spelledp
  , a
  , b
  , c
  , d
  , e
  , f
  , g
  , SPC -- spelled pitch classes
  , parseSPC
  , spc
  , a'
  , b'
  , c'
  , d'
  , e'
  , f'
  , g'
  ) where

import Prelude hiding (degree)
import Control.Alt ((<|>))
import Data.Char (fromCharCode, toCharCode)
import Data.Either (hush)
import Data.Group (class Group)
import Data.List.NonEmpty (length)
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (power) as M
import Data.Ord (abs)
import Data.Pitches.Class (class Chromatic, class Diatonic, class HasInterval, class HasIntervalClass, class Interval, class Notation, class NotationPitch, class ReadForeignPitch, class ToMidi, class ToMidiPitch, class WriteForeignPitch, ImperfectInterval(..), Pitch(..), aug, chromaticSemitone, direction, down, iabs, ic, parseNotation, showNotation, toMidi, (+^), (^*), (^-^))
import Data.Pitches.Internal (parseSInt, parseUInt, readJSONviaParse)
import Data.String as S
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)
import StringParser (Parser, fail, runParser) as P
import StringParser.CodePoints (char, oneOf, eof) as P
import StringParser.Combinators (many1, option) as P
import Test.QuickCheck (class Arbitrary, arbitrary)

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

-- | A common class for accessor functions of spelled types.
class Spelled i where
  -- | Return the position of the interval or pitch on the line of fifths.
  fifths :: i -> Int
  -- | Return the number of octaves spanned by the interval
  -- | (upward starting at 0, downward starting at -1).
  -- | For pitches, return the octave of the pitch.
  -- | Always `0` for pitch / interval classes.
  octaves :: i -> Int
  -- | Return the number of octaves in the internal representation.
  internalOctaves :: i -> Int
  -- | Return the scale degree of the interval or pitch (relative to C).
  degree :: i -> Int
  -- | Return the generic interval size [-6,6].
  -- | For pitches, use `degree` instead.
  generic :: i -> Int
  -- | The number of diatonic steps in the interval (unison=`0`, 2nd=`1`, ..., octave=`7`),
  -- | respecting direction and octaves.
  -- | For pitches, use `degree` instead.
  diasteps :: i -> Int
  -- | Return the number of semitones by which the interval is altered
  -- | from its the perfect or major variant.
  -- | Positive alteration always indicates augmentation,
  -- | negative alteration indicates diminution (minor or smaller) of the interval,
  -- | regardless of the direction of the interval.
  -- | For pitches, return the accidentals (positive=sharps, negative=flats, `0`=natural).
  alteration :: i -> Int

---------------
-- SInterval --
---------------
-- | A type for representing spelled intervals as a combination of fifths and octaves.
newtype SInterval = SInterval { fifths :: Int, octaves :: Int }

-- constructors for SInterval
--
-- | Construct a spelled interval directly from fifths and octaves.
spelled :: Int -> Int -> SInterval
spelled fifths octaves = SInterval { fifths, octaves }

-- | Construct a spelled interval by specifying its diatonic and chromatic steps (semitones).
-- | For example, a major third spans 2 diatonic steps and 4 semitones
-- | while a minor third also spans 2 diatonic steps but only 3 semitones
-- | ```purescript
-- | > spelledDiaChrom 2 4
-- | M3:0
-- | > spelledDiaChrom 2 3
-- | m3:0
-- | ```
spelledDiaChrom :: Int -> Int -> SInterval
spelledDiaChrom dia chrom = diaPart <> chromPart
  where
  diaPart = wholetone ^* dia

  chromPart = chromaticSemitone ^* (chrom - 2 * dia)

-- | Construct an interval that goes `n` diatonic steps up
-- | but is enharmonically equivalent to a unison.
-- | ```purescript
-- | > onlyDia 2
-- | ddd3:0
-- | ```
onlyDia :: Int -> SInterval
onlyDia n = wholetone ^* n ^-^ chromaticSemitone ^* (2 * n)

-- | A version of `parseNotation` specialized to `SInterval`.
-- | ```purescript
-- | > parseSpelled "M3:0"
-- | (Just M3:0)
-- | ```
parseSpelled :: String -> Maybe SInterval
parseSpelled = parseNotation

-- | A major second.
wholetone :: SInterval
wholetone = spelled 2 (-1)

-- | A generic second. Use with `major` or `minor` to obtain an interval.
second :: ImperfectInterval SInterval
second = Impf (spelled 2 (-1) ^-^ _)

-- | A generic third. Use with `major` or `minor` to obtain an interval.
third :: ImperfectInterval SInterval
third = Impf (spelled 4 (-2) ^-^ _)

-- | A perfect fourth.
fourth :: SInterval
fourth = spelled (-1) 1

-- | An augmented fourth.
tritone :: SInterval
tritone = aug fourth

-- | A perfect fifth.
fifth :: SInterval
fifth = spelled 1 0

-- | A generic sixth. Use with `major` or `minor` to obtain an interval.
sixth :: ImperfectInterval SInterval
sixth = Impf (spelled 3 (-1) ^-^ _)

-- | A generic seventh. Use with `major` or `minor` to obtain an interval.
seventh :: ImperfectInterval SInterval
seventh = Impf (spelled 5 (-2) ^-^ _)

-- instances for SInterval
--
derive instance Eq SInterval

instance Arbitrary SInterval where
  arbitrary = spelled <$> map (_ `mod` 100) arbitrary <*> map (_ `mod` 100) arbitrary

instance Spelled SInterval where
  fifths (SInterval i) = i.fifths
  octaves (SInterval i) = i.octaves + ((i.fifths * 4) `div` 7)
  internalOctaves (SInterval i) = i.octaves
  degree (SInterval i) = fifths2degree i.fifths
  generic i
    | direction i == LT = -(degree $ down i)
    | otherwise = degree i
  diasteps (SInterval i) = i.fifths * 4 + i.octaves * 7
  alteration i = (fifths (iabs i) + 1) `div` 7

instance Semigroup SInterval where
  append (SInterval i1) (SInterval i2) = spelled (i1.fifths + i2.fifths) (i1.octaves + i2.octaves)

instance Monoid SInterval where
  mempty = spelled 0 0

instance Group SInterval where
  ginverse (SInterval i) = spelled (-i.fifths) (-i.octaves)

instance Ord SInterval where
  compare i1 i2 = compare [ diasteps i1, alteration i1 ] [ diasteps i2, alteration i2 ]

instance Interval SInterval where
  octave = spelled 0 1
  direction i = compare (diasteps i) 0

instance HasIntervalClass SInterval SIC where
  ic (SInterval i) = sic i.fifths

instance HasInterval SInterval SInterval where
  emb i = i

instance Diatonic SInterval where
  isStep i = abs (diasteps i) < 2

instance Chromatic SInterval where
  chromaticSemitone = spelled 7 (-4)

instance ToMidi SInterval where
  toMidi (SInterval i) = i.fifths * 7 + i.octaves * 12

instance Show SInterval where
  show = showNotation

instance WriteForeign SInterval where
  writeImpl = writeImpl <<< showNotation

instance ReadForeign SInterval where
  readImpl = readJSONviaParse "spelled interval"

---------
-- SIC --
---------
--
-- | A type for representing spelled interval classes on the line of fifths.
newtype SIC = SIC Int

-- constructors for SIC
--
-- | Construct a spelled interval class directly from fifths,
-- | e.g. `sic (-1) == P4`, `sic 0 == P1`, `sic 1 == P5`, `sic 2 == M2`, ...
sic :: Int -> SIC
sic = SIC

-- | A version of `parseNotation` specialized to `SIC`.
-- | ```purescript
-- | > parseSIC "M3"
-- | (Just M3)
-- | ```
parseSIC :: String -> Maybe SIC
parseSIC = parseNotation

-- | A generic second. Use with `major` or `minor` to obtain an interval class.
second' :: ImperfectInterval SIC
second' = Impf (sic 2 ^-^ _)

-- | A generic third. Use with `major` or `minor` to obtain an interval class.
third' :: ImperfectInterval SIC
third' = Impf (sic 4 ^-^ _)

-- | A perfect fourth.
fourth' :: SIC
fourth' = sic (-1)

-- | An augmented fourth.
tritone' :: SIC
tritone' = sic 6

-- | A perfect fifth.
fifth' :: SIC
fifth' = sic 1

-- | A generic seventh. Use with `major` or `minor` to obtain an interval class.
sixth' :: ImperfectInterval SIC
sixth' = Impf (sic 3 ^-^ _)

-- | A generic seventh. Use with `major` or `minor` to obtain an interval class.
seventh' :: ImperfectInterval SIC
seventh' = Impf (sic 5 ^-^ _)

-- instances for SIC
-- 
derive instance Eq SIC

derive instance Ord SIC

instance Arbitrary SIC where
  arbitrary = SIC <<< (_ `mod` 100) <$> arbitrary

instance Spelled SIC where
  fifths (SIC fs) = fs
  octaves _ = 0
  internalOctaves _ = 0
  degree (SIC fs) = fifths2degree fs
  generic (SIC fs) = fifths2degree fs
  diasteps (SIC fs) = fifths2degree fs
  alteration (SIC fs) = (fs + 1) `div` 7

instance Semigroup SIC where
  append (SIC i1) (SIC i2) = sic $ i1 + i2

instance Monoid SIC where
  mempty = sic 0

instance Group SIC where
  ginverse (SIC fs) = sic (-fs)

instance Interval SIC where
  octave = sic 0
  direction i = if dia == 0 then EQ else if dia < 4 then GT else LT
    where
    dia = diasteps i

instance HasInterval SIC SInterval where
  emb (SIC fs) = spelled fs (negate $ (fs * 4) `div` 7)

instance HasIntervalClass SIC SIC where
  ic i = i

instance Diatonic SIC where
  isStep i = deg == 0 || deg == 1 || deg == 6
    where
    deg = degree i

instance Chromatic SIC where
  chromaticSemitone = sic 7

instance ToMidi SIC where
  toMidi (SIC fs) = (fs * 7) `mod` 12

instance Show SIC where
  show = showNotation

instance WriteForeign SIC where
  writeImpl = writeImpl <<< showNotation

instance ReadForeign SIC where
  readImpl input = readJSONviaParse "spelled interval class" input

-- spelled pitch
-- -------------
instance (Spelled i, HasIntervalClass i ic, Spelled ic) => Spelled (Pitch i) where
  fifths (Pitch i) = fifths i
  octaves (Pitch i) = octaves i
  internalOctaves (Pitch i) = internalOctaves i
  degree (Pitch i) = degree i
  generic (Pitch i) = degree i -- not well-defined for pitches
  diasteps (Pitch i) = degree i -- not well-defined for pitches
  alteration (Pitch i) = alteration $ ic i

asciiA :: Int
asciiA = toCharCode 'A'

-- | Return the letter of the given pitch.
letter :: forall i. Spelled i => i -> String
letter i = S.singleton $ S.codePointFromChar $ fromMaybe 'X' $ fromCharCode $ asciiA + ((degree i + 2) `mod` 7)

-- spelled pitches
--
-- | A type alias for spelled pitches.
type SPitch = Pitch SInterval

-- | Create a spelled pitch directly from internal fifths and octaves.
-- | Only used this if you know what you are doing.
spelledp :: Int -> Int -> SPitch
spelledp fs os = Pitch $ spelled fs os

-- | A version of `parseNotation` specialized to `SPitch`.
-- | ```purescript
-- | > parseSpelledP "Eb4"
-- | (Just E♭4)
-- | ```
parseSpelledP :: String -> Maybe SPitch
parseSpelledP = parseNotation

-- | Represent an accidental as a number of semitones upward.
newtype Accidental = Acc Int

-- | A single flat.
flt :: Accidental
flt = Acc (-1)

-- | A single sharp.
shp :: Accidental
shp = Acc 1

-- | A natural.
nat :: Accidental
nat = Acc 0

-- helper function for creating spelled pitch constructors
toSpelled :: Int -> Int -> Accidental -> Int -> SPitch
toSpelled fs os (Acc acc) oct = spelledp fs (os + oct) +^ (chromaticSemitone ^* acc)

-- | Construct a C-like pitch by providing accidentals and an octave.
c :: Accidental -> Int -> SPitch
c = toSpelled 0 0

-- | Construct a D-like pitch by providing accidentals and an octave.
d :: Accidental -> Int -> SPitch
d = toSpelled 2 (-1)

-- | Construct an E-like pitch by providing accidentals and an octave.
e :: Accidental -> Int -> SPitch
e = toSpelled 4 (-2)

-- | Construct an F-like pitch by providing accidentals and an octave.
f :: Accidental -> Int -> SPitch
f = toSpelled (-1) 1

-- | Construct a G-like pitch by providing accidentals and an octave.
g :: Accidental -> Int -> SPitch
g = toSpelled 1 0

-- | Construct an A-like pitch by providing accidentals and an octave.
a :: Accidental -> Int -> SPitch
a = toSpelled 3 (-1)

-- | Construct a B-like pitch by providing accidentals and an octave.
b :: Accidental -> Int -> SPitch
b = toSpelled 5 (-2)

instance WriteForeignPitch SInterval where
  writeImplPitch i = writeImpl $ show $ Pitch i

instance ReadForeignPitch SInterval where
  readImplPitch input = readJSONviaParse "spelled pitch" input

instance ToMidiPitch SInterval where
  toMidiPitch i = toMidi i + 12

-- spelled pitch classes
--
-- | A type alias for spelled pitch classes.
type SPC = Pitch SIC

-- | Create a spelled pitch class directly from fifths.
spc :: Int -> SPC
spc fs = Pitch $ sic fs

-- | A version of `parseNotation` specialized to `SPC`.
-- | ```purescript
-- | > parseSPC "Eb"
-- | (Just E♭)
-- | ```
parseSPC :: String -> Maybe SPC
parseSPC = parseNotation

-- helper for creating spelled pitch class constructors
toSPC :: Int -> Accidental -> SPC
toSPC fs (Acc acc) = spc fs +^ (chromaticSemitone ^* acc)

-- | Construct a C-like pitch class by providing accidentals.
c' :: Accidental -> SPC
c' = toSPC 0

-- | Construct a D-like pitch class by providing accidentals.
d' :: Accidental -> SPC
d' = toSPC 2

-- | Construct an E-like pitch class by providing accidentals.
e' :: Accidental -> SPC
e' = toSPC 4

-- | Construct an F-like pitch class by providing accidentals.
f' :: Accidental -> SPC
f' = toSPC (-1)

-- | Construct a G-like pitch class by providing accidentals.
g' :: Accidental -> SPC
g' = toSPC 1

-- | Construct an A-like pitch class by providing accidentals.
a' :: Accidental -> SPC
a' = toSPC 3

-- | Construct a B-like pitch class by providing accidentals.
b' :: Accidental -> SPC
b' = toSPC 5

instance WriteForeignPitch SIC where
  writeImplPitch i = writeImpl $ showNotation $ Pitch i

instance ReadForeignPitch SIC where
  readImplPitch input = readJSONviaParse "spelled pitch class" input

instance ToMidiPitch SIC where
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
  dia <- (\x -> x - 1) <$> parseUInt
  alt <- falt $ isPerfect dia
  pure $ ((dia * 2 + 1) `mod` 7) - 1 + (7 * alt)

instance Notation SInterval where
  parseNotation str = hush $ P.runParser parser str
    where
    parser = do
      sign <- P.option '+' $ P.char '-'
      fs <- parseDia
      _ <- P.char ':'
      os <- parseSInt
      P.eof
      let
        i = SInterval { fifths: fs, octaves: os - ((fs * 4) `div` 7) }
      pure $ if sign == '-' then down i else i
  showNotation i
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

instance Notation SIC where
  parseNotation str = hush $ P.runParser parser str
    where
    parser = do
      sign <- P.option '+' (P.char '-')
      i <- sic <$> parseDia
      P.eof
      pure $ if sign == '-' then down i else i
  showNotation i = qual <> show (dia + 1)
    where
    dia = diasteps i

    alt = alteration i

    qual =
      if isPerfect dia then
        qualpf alt "a" "P" "d"
      else
        qualimpf alt "a" "M" "m" "d"

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

instance NotationPitch SInterval where
  parsePitchNotation str = hush $ P.runParser parser str
    where
    parser = do
      fs <- parseName
      os <- parseSInt
      P.eof
      pure $ spelled fs (os - ((fs * 4) `div` 7))
  showPitchNotation i = letter p <> accs <> show (octaves p)
    where
    p = Pitch i

    accs = accstr (alteration p) "♯" "♭"

instance NotationPitch SIC where
  parsePitchNotation str = hush $ P.runParser parser str
    where
    parser = do
      fifths <- parseName
      P.eof
      pure $ sic fifths
  showPitchNotation i = letter p <> accs
    where
    p = Pitch i

    accs = accstr (alteration p) "♯" "♭"
