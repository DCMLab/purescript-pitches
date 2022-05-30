-- | This module defines intervals and pitches in the 12-tone system as used by the MIDI standard.
-- | The notation for MIDI pitch and interval types uses the integer representation
-- | together with a prefix that distinquishes the type of the value:
-- | ```
-- | prefix  type          examples
-- | i       MidiInterval  i13 (m9 up), i-4 (M3 down)
-- | ic      MidiIC        ic1 (m2 up = M7 down), ic8 (m6 up = M3 down)
-- | p       MidiPitch     p63 (Eb/D#4)
-- | pc      MidiPC        pc3 (Eb/D#)
-- | ```
module Data.Pitches.Midi
  ( MidiInterval(..)
  , MidiIC(..)
  , MidiPitch
  , MidiPC
  , midi
  , midic
  , midip
  , midipc
  , parseMidiInterval
  , parseMidiIC
  , parseMidiPitch
  , parseMidiPC
  ) where

import Prelude

import Data.Either (hush)
import Data.Group (class Group)
import Data.Maybe (Maybe)
import Data.Ord (abs)
import Data.Pitches.Class (class Chromatic, class Diatonic, class HasInterval, class HasIntervalClass, class Interval, class Notation, class NotationPitch, class ReadForeignPitch, class ToMidi, class ToMidiPitch, class WriteForeignPitch, Pitch(..), parseNotation, showNotation)
import Data.Pitches.Internal (parseSInt)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import StringParser as P
import Test.QuickCheck (class Arbitrary, arbitrary)

-- MIDI intervals
-- --------------

-- | A type for representing MIDI intervals as intergers.
newtype MidiInterval = MidiInterval Int

-- MIDI interval constructors / accessors

-- | Create a MIDI interval from an integer.
-- | ```purescript
-- | > midi 13 -- minor 9th
-- | i13
-- | ```
midi :: Int -> MidiInterval
midi = MidiInterval

-- | A version of `parseNotation` that is specialized to `MidiInterval`.
-- | ```purescript
-- | > parseMidiInterval "i13" -- minor 9th
-- | (Just i13)
-- | ```
parseMidiInterval :: String -> Maybe MidiInterval
parseMidiInterval = parseNotation

-- MIDI interval instances

derive instance Eq MidiInterval

derive instance Ord MidiInterval

instance Semigroup MidiInterval where
  append (MidiInterval i1) (MidiInterval i2) = MidiInterval (i1 + i2)

instance Monoid MidiInterval where
  mempty = MidiInterval 0

instance Group MidiInterval where
  ginverse (MidiInterval i) = MidiInterval (-i)

instance Interval MidiInterval where
  octave = MidiInterval 12
  direction (MidiInterval i) = compare i 0

instance HasInterval MidiInterval MidiInterval where
  emb i = i

instance HasIntervalClass MidiInterval MidiIC where
  ic (MidiInterval i) = midic i

instance Diatonic MidiInterval where
  isStep (MidiInterval i) = abs i <= 2

instance Chromatic MidiInterval where
  chromaticSemitone = MidiInterval 1

instance Notation MidiInterval where
  showNotation (MidiInterval i) = "i" <> show i
  parseNotation str = hush $ P.runParser parser str
    where
    parser = do
      _ <- P.char 'i'
      i <- parseSInt
      P.eof
      pure $ midi i

instance Show MidiInterval where
  show = showNotation

instance ToMidi MidiInterval where
  toMidi (MidiInterval i) = i

derive newtype instance WriteForeign MidiInterval

derive newtype instance ReadForeign MidiInterval

instance Arbitrary MidiInterval where
  arbitrary = MidiInterval <$> arbitrary

-- MIDI interval classes
-- ---------------------

-- | A type for representing MIDI interval classes as intergers (mod 12).
newtype MidiIC = MidiIC Int

-- midi interval class constructors accessors

-- | Create a MIDI interval class from an integer.
-- | The input is always taken mod 12.
-- | ```purescript
-- | > midic 13 -- minor 9th = minor 2nd
-- | ic1
-- | ```
midic :: Int -> MidiIC
midic i = MidiIC (i `mod` 12)

-- | A version of `parseNotation` that is specialized to `MidiIC`.
-- | ```purescript
-- | > parseMidiIC "ic13" -- minor 9th = minor 2nd
-- | (Just ic1)
-- | ```
parseMidiIC :: String -> Maybe MidiIC
parseMidiIC = parseNotation

-- midi interval class instances

derive instance Eq MidiIC

derive instance Ord MidiIC

instance Semigroup MidiIC where
  append (MidiIC i1) (MidiIC i2) = midic $ i1 + i2

instance Monoid MidiIC where
  mempty = MidiIC 0

instance Group MidiIC where
  ginverse (MidiIC i) = midic (-i)

instance Interval MidiIC where
  octave = MidiIC 0
  direction (MidiIC 0) = EQ
  direction (MidiIC i) = compare 6 i

instance HasInterval MidiIC MidiInterval where
  emb (MidiIC i) = MidiInterval i

instance HasIntervalClass MidiIC MidiIC where
  ic i = i

instance Diatonic MidiIC where
  isStep (MidiIC i) = i <= 2 || i < 12 && i > 9 -- no i < 12?

instance Chromatic MidiIC where
  chromaticSemitone = MidiIC 1

instance Notation MidiIC where
  showNotation (MidiIC i) = "ic" <> show i
  parseNotation str = hush $ P.runParser parser str
    where
    parser = do
      _ <- P.string "ic"
      i <- parseSInt
      P.eof
      pure $ midic i

instance Show MidiIC where
  show = showNotation

instance ToMidi MidiIC where
  toMidi (MidiIC i) = i

derive newtype instance WriteForeign MidiIC

instance ReadForeign MidiIC where
  -- make sure that Int is interpreted mod 12
  readImpl input = midic <$> readImpl input

instance Arbitrary MidiIC where
  arbitrary = midic <$> arbitrary

-- Midi Pitches and Pitch classes
-- ------------------------------

-- midi pitches

-- | A type for representing MIDI pitches as intergers.
type MidiPitch = Pitch MidiInterval

-- | Create a MIDI pitch from an integer.
-- | ```purescript
-- | > midip 63 -- Eb/D#4
-- | p63
-- | ```
midip :: Int -> MidiPitch
midip = Pitch <<< MidiInterval

-- | A version of `parseNotation` that is specialized to `MidiPitch`.
-- | ```purescript
-- | > parseMidiPitch "p63" -- Eb/D#4
-- | (Just p63)
-- | ```
parseMidiPitch :: String -> Maybe MidiPitch
parseMidiPitch = parseNotation

instance NotationPitch MidiInterval where
  showPitchNotation (MidiInterval i) = "p" <> show i
  parsePitchNotation str = hush $ P.runParser parser str
    where
    parser = do
      _ <- P.string "p"
      i <- parseSInt
      P.eof
      pure $ midi i

instance ToMidiPitch MidiInterval where
  toMidiPitch (MidiInterval i) = i

instance WriteForeignPitch MidiInterval where
  writeImplPitch (MidiInterval i) = writeImpl i

instance ReadForeignPitch MidiInterval where
  readImplPitch i = Pitch <$> readImpl i

-- midi pitch classes

-- | A type for representing MIDI pitch classes as intergers (mod 12).
type MidiPC = Pitch MidiIC

-- | Create a MIDI pitch class from an integer.
-- | The input is always taken mod 12.
-- | ```purescript
-- | > midipc 63 -- Eb/D#
-- | pc3
-- | ```
midipc :: Int -> MidiPC
midipc = Pitch <<< midic

-- | A version of `parseNotation` that is specialized to `MidiPC`.
-- | ```purescript
-- | > parseMidiPC "pc63" -- Eb/D#
-- | (Just pc3)
-- | ```
parseMidiPC :: String -> Maybe MidiPC
parseMidiPC = parseNotation

instance NotationPitch MidiIC where
  showPitchNotation (MidiIC i) = "pc" <> show i
  parsePitchNotation str = hush $ P.runParser parser str
    where
    parser = do
      _ <- P.string "pc"
      i <- parseSInt
      P.eof
      pure $ midic i

instance ToMidiPitch MidiIC where
  toMidiPitch (MidiIC i) = i

instance WriteForeignPitch MidiIC where
  writeImplPitch (MidiIC i) = writeImpl i

instance ReadForeignPitch MidiIC where
  readImplPitch i = Pitch <$> readImpl i
