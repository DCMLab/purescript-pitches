-- | This module specifies the generic interface for pitch and interval types.
-- |
-- | The central type class is the `Interval` typeclass,
-- | which provides the basic operations and constants for calculating with intervals,
-- | together with a number of derived operators
-- | This main class is complemented by other type classes that provide optional functionality,
-- | such as converting to and from interval classes (`HasIntervalClass` and `IntervalClassOf`),
-- | distinguishing special intervals (`Diatonic` and `Chromatic`),
-- | or converting to and MIDI as well as to and from a standardized text notation (`ToMidi` and `Notation`).
-- |
-- | Pitches are represented as intervals that are interpreted relative to a type-specific reference point,
-- | similar to how points can be represented as a vector from an origin.
-- | This is done using the `Pitch a` newtype (where `a` is the corresponding interval type),
-- | which supports a number of operations between pitches and intervals.
-- | Specific pitch types can implement custom conversions
-- | to (and from) MIDI, text notation, and `Foreign` using auxiliary type classes.
module Data.Pitches.Class
  ( class Interval
  , octave
  , direction
  , iabs
  , unison
  , oct
  , down
  , iplus
  , (^+^)
  , iminus
  , (^-^)
  , itimes
  , (^*)
  , timesi
  , (*^)
  , class IsIntervalClassOf
  , emb
  , class HasIntervalClass
  , ic
  , pc
  , class Diatonic
  , isStep
  , class Chromatic
  , chromaticSemitone
  , class ToMidi
  , toMidi
  , class Notation
  , parseNotation
  , showNotation
  , ImperfectInterval(..)
  , minor
  , major
  , dim
  , aug
  , Pitch(..)
  , toPitch
  , toInterval
  , pto
  , pfrom
  , (^+)
  , iplusp
  , (+^)
  , pplusi
  , (-^)
  , pminusi
  , class NotationPitch
  , parsePitchNotation
  , showPitchNotation
  , class ToMidiPitch
  , toMidiPitch
  , class WriteForeignPitch
  , writeImplPitch
  , class ReadForeignPitch
  , readImplPitch
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Group (class Group, ginverse, power)
import Data.Maybe (Maybe)
import Foreign (Foreign, F)
import Simple.JSON as JSON
import Test.QuickCheck (class Arbitrary)

---------------
-- Intervals --
---------------
-- | A class for interval types (including "interval class" types).
-- Arithmetic operations (addition, subtraction, inversion) are provided by implementing `Group`.
-- On top of that, each interval type must provide an octave interval and a direction.
class
  (Group i) <= Interval i where
  -- | The perfect octave interval.
  octave :: i
  -- | Return the direction of an interval,
  -- | where up = `GT`, down = `LT`, and neutral intervals = `EQ`.
  -- | The exact semantics of this function depend on the interval type.
  direction :: i -> Ordering

-- | Links an interval class type to the corresponding (non-class) interval type.
class
  (Interval i, Interval ic, HasIntervalClass i ic) <= IsIntervalClassOf ic i | i -> ic, ic -> i where
  -- | Embed an interval class into the first upward octave (e.g. `M3` -> `M3:0`).
  emb :: ic -> i

class
  (Interval i, Interval ic) <= HasIntervalClass i ic | i -> ic where
  -- | Project an interval to the corresponding interval class.
  ic :: i -> ic

class
  (Interval i) <= Diatonic i where
  -- | Test whether the interval is within step distance
  -- | (step or smaller, e.g. unisons and seconds).
  isStep :: i -> Boolean

class
  (Interval i) <= Chromatic i where
  -- | The chromatic semitone of the respective type
  -- | (e.g. `a1`/`a1:0` for spelled intervals, `1` for MIDI).
  chromaticSemitone :: i

class ToMidi i where
  -- | Return the corresponding MIDI interval.
  toMidi :: i -> Int

-- | A class for types (intervals and pitches) that support a string notation.
-- | For `Pitch a`, the implementation of `Notation (Pitch a)` is deferred to `NotationPitch a`
-- | to support specific pitch notation for each underlying interval type.
-- | Implementations must satisfy the law `parseNotation (showNotation i) == Just i`.
-- | For the provided interval and pitch types, the string representation is standardized
-- | to be compatible with libraries for other languages.
class Notation a where
  -- | Parse a string notation of an object.
  parseNotation :: String -> Maybe a
  -- | Convert an object into a string notation.
  showNotation :: a -> String

-- | The perfect unison interval.
-- | Defined as the neutral element wrt. interval addition (`mempty`).
unison :: forall i. Interval i => i
unison = mempty

-- | Embed and interval class into a specific octave (`octs 0 = emb`).
oct :: forall i ic. IsIntervalClassOf ic i => Int -> ic -> i
oct octs ic = emb ic <> (octave ^* octs)

-- | Invert the direction of an interval (alias for `ginverse`).
-- | For downward intervals, the interval is turned up.
down :: forall i. Interval i => i -> i
down = ginverse

-- | Return the absolute interval (same size as input, always upward/neutral directed).
-- | Depends on the definition of `direction`,
-- | so for example `iabs d1:0 == d1:0` since `d1:0` is considered neutral.
iabs :: forall i. Interval i => i -> i
iabs i
  | direction i == LT = down i
  | otherwise = i

-- | Interval addition. See `(^+^)`.
iplus :: forall g. Interval g => g -> g -> g
iplus = append

-- | Interval addition (e.g. `interval1 ^+^ interval2`)
infixl 6 iplus as ^+^

-- | Interval subtraction. See `(^-^)`.
iminus :: forall g. Interval g => g -> g -> g
iminus a b = a <> down b

-- | Interval subtraction (e.g. `interval1 ^-^ interval2`).
infixl 6 iminus as ^-^

-- | Interval multiplication with an integer. See `(^*)`.
itimes :: forall g. Interval g => g -> Int -> g
itimes = power

-- | Interval multiplication with an integer (e.g. `interval ^* 3`).
infixl 7 itimes as ^*

-- | Interval multiplication with an integer (alias for `flip itimes`, see `(*^)`).
timesi :: forall g. Interval g => Int -> g -> g
timesi = flip itimes

-- | Interval multiplication with an integer (e.g. `3 *^ interval`).
infixr 7 timesi as *^

-- | A helper type for representing imperfect intervals (e.g. `third`).
-- | An imperfect interval is represented as a function that takes a (downward) modifier to the major version of the interval.
-- | To obtain the major version of the interval, pass `unison` (or call `major`);
-- | to obtain the minor version, pass `chromaticSemitone` (or call `minor`.
newtype ImperfectInterval i
  = Impf (i -> i)

-- | Return the minor version of an imperfect interval (e.g. `minor third == m3:0`).
minor :: forall i. Chromatic i => ImperfectInterval i -> i
minor (Impf int) = int chromaticSemitone

-- | Return the major version of an imperfect interval (e.g. `major third == M3:0`).
major :: forall i. Interval i => ImperfectInterval i -> i
major (Impf int) = int unison

-- | Diminish an interval by 1 semitone
-- | (e.g. `dim fourth == d4:0`, `dim (minor third) == d3:0`).
dim :: forall i. Chromatic i => i -> i
dim = (_ ^-^ chromaticSemitone)

-- | Augment an interval by 1 semitone
-- | (e.g. `aug fourth == a4:0`, `aug (major third) == a3:0`).
aug :: forall i. Chromatic i => i -> i
aug = (_ ^+^ chromaticSemitone)

-------------
-- Pitches --
-------------
-- | A generic pitch type.
-- | Wraps an interval that is interpreted as the interval between the pitch and a reference pitch.
-- | Normally, this interval is never provided or used directly.
-- | Instead, different interval types provide smart constructors that generate `Pitch` objects.
newtype Pitch a
  = Pitch a

derive newtype instance eqPitch :: (Eq a) => Eq (Pitch a)

derive newtype instance ordPitch :: (Ord a) => Ord (Pitch a)

derive newtype instance arbitraryPitch :: (Arbitrary a) => Arbitrary (Pitch a)

derive instance functorPitch :: Functor Pitch

derive instance genericPitch :: Generic (Pitch a) _

-- | Convert an interval to a pitch.
-- | The interpretation of the resulting pitch depends on the type-specific reference point.
-- | Don't use this unless you know what you are doing.
toPitch :: forall i. (Interval i) => i -> Pitch i
toPitch = Pitch

-- | Convert a pitch to an interval.
-- | The interpretation of the given pitch depends on the type-specific reference point.
-- | Don't use this unless you know what you are doing.
toInterval :: forall i. (Interval i) => Pitch i -> i
toInterval (Pitch i) = i

-- | Return the interval from the first to the second pitch (`pto p1 p2` = from `p1` to `p2`).
pto :: forall i. (Interval i) => Pitch i -> Pitch i -> i
pto (Pitch a) (Pitch b) = b <> down a

-- | Return the interval from the second to the first pitch (`pfrom p1 p2` = from `p2` to `p1`).
pfrom :: forall i. (Interval i) => Pitch i -> Pitch i -> i
pfrom (Pitch a) (Pitch b) = a <> down b

-- | Add an interval to a pitch (see `(+^)`).
pplusi :: forall i. (Interval i) => Pitch i -> i -> Pitch i
pplusi (Pitch a) b = Pitch (a <> b)

-- | Add an interval to a pitch (e.g. `pitch +^ interval`).
infixl 6 pplusi as +^

-- | Add an interval to a pitch (see `(^+)`).
iplusp :: forall i. (Interval i) => i -> Pitch i -> Pitch i
iplusp a (Pitch b) = Pitch (a <> b)

-- | Add an interval to a pitch (e.g. `interval ^+ pitch`).
infixr 6 iplusp as ^+

-- | Subtract an interval from a pitch (see `(-^)`).
pminusi :: forall i. (Interval i) => Pitch i -> i -> Pitch i
pminusi (Pitch a) b = Pitch (a <> down b)

-- | Subtract an interval from a pitch (e.g. `pitch -^ interval`).
infixl 6 pminusi as -^

-- | Return the pitch class corresponding to the pitch (e.g. `pc Eb4 == Eb`).
pc :: forall i ic. (HasIntervalClass i ic) => Pitch i -> Pitch ic
pc = map ic

-- helper classes
-- --------------
-- | A helper class for pitch notation. Used by the `Notation` instance of `Pitch`.
class NotationPitch i where
  -- | Parse the string representation of a pitch and return the corresponding internal interval.
  parsePitchNotation :: String -> Maybe i
  -- | Return the string notation of a pitch with the given internal interval.
  showPitchNotation :: i -> String

instance showPitchInst :: NotationPitch i => Show (Pitch i) where
  show (Pitch i) = showPitchNotation i

instance notationPitch :: NotationPitch i => Notation (Pitch i) where
  parseNotation = parsePitchNotation >>> map Pitch
  showNotation (Pitch i) = showPitchNotation i

-- | A helper class for converting pitches to midi. Used by the `ToMidi` instance of `Pitch`.
class ToMidiPitch i where
  -- | Return the midi number of a pitch with the given internal interval.
  toMidiPitch :: i -> Int

instance tomidiPitch :: ToMidiPitch a => ToMidi (Pitch a) where
  toMidi (Pitch i) = toMidiPitch i

-- -- | A helper class for parsing pitches. Used by the `ParseNotation` instance of `Pitch`.
-- class ParsePitchNotation a where
--   -- | Parse the string representation of a pitch with the internal interval type `a`.
--   parsePitchNotation :: String -> Maybe (Pitch a)
-- instance parsenotationPitch :: ParsePitchNotation a => ParseNotation (Pitch a) where
--   parseNotation = parsePitchNotation
-- | A helper class for converting pitches. Used by the `Simple.JSON.WriteForeign` instance of `Pitch`.
class WriteForeignPitch i where
  -- | Return the `Foreign` representation of a pitch with the given internal interval.
  writeImplPitch :: i -> Foreign

instance writeForeignPitch :: (WriteForeignPitch i) => JSON.WriteForeign (Pitch i) where
  writeImpl (Pitch i) = writeImplPitch i

-- | A helper class for converting pitches. Used by the `Simple.JSON.ReadForeign` instance of `Pitch`.
class ReadForeignPitch i where
  -- | Try to read the `Foreign` representation of a pitch with the internal interval type `a`.
  readImplPitch :: Foreign -> F (Pitch i)

instance readForeignPitch :: (ReadForeignPitch i) => JSON.ReadForeign (Pitch i) where
  readImpl f = readImplPitch f
