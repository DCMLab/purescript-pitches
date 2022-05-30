module TestMidi
  ( testMidi
  ) where

import Data.Pitches
import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
-- import Partial.Unsafe (unsafePartial)
import Simple.JSON (readJSON_, writeJSON)
import Test.QuickCheck ((===))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

shouldBe = shouldEqual

-- unsafeJust :: forall a. Maybe a -> a
-- unsafeJust = unsafePartial f
--   where
--   f :: Partial => Maybe a -> a
--   f (Just a) = a

-- rsi :: String -> MidiInterval
-- rsi = unsafeJust <<< parseNotation

-- rsic :: String -> MidiIC
-- rsic = unsafeJust <<< parseNotation

-- rsp :: String -> SPitch
-- rsp = unsafeJust <<< parseNotation

-- rspc :: String -> MidiPC
-- rspc = unsafeJust <<< parseNotation

negateOrd :: Ordering -> Ordering
negateOrd LT = GT

negateOrd EQ = EQ

negateOrd GT = LT

testMidiIntervalProps =
  describe "MidiInterval properties" do
    describe "unison" do
      it "is mempty" $ mempty `shouldBe` (unison :: MidiInterval)
      it "is neutral wrt addition"
        $ quickCheck \i -> i ^+^ unison === (i :: MidiInterval)
    describe "negateV" do
      it "changes the direction of an interval"
        $ quickCheck \i -> direction (down i :: MidiInterval) === negateOrd (direction i)
    describe "^-^" do
      it "behaves inverse to ^+^"
        $ quickCheck \i1 i2 -> i1 ^+^ i2 ^-^ i2 === (i1 :: MidiInterval)
      it "returns unison for i ^-^ i"
        $ quickCheck \i -> (i :: MidiInterval) ^-^ i === unison
    describe "notation" do
      it "survives a notation roundtrip (intervals)"
        $ quickCheck \i -> parseNotation (showNotation i) === Just (i :: MidiInterval)
      it "survives a notation roundtrip (pitches)"
        $ quickCheck \p -> parseNotation (showNotation p) === Just (p :: MidiPitch)
    describe "JSON" do
      it "survives a JSON roundtrip (intervals)"
        $ quickCheck \i -> readJSON_ (writeJSON i) === Just (i :: MidiInterval)
      it "survives a JSON roundtrip (pitches)"
        $ quickCheck \p -> readJSON_ (writeJSON p) === Just (p :: MidiPitch)

--    describe "JSON" $ it "returns input on readJSON <<< writeJSON"
testMidiICProps =
  describe "MidiIC properties" do
    describe "octave" do
      it "is mempty"
        $ quickCheck \n -> mempty === (octave ^* n :: MidiIC)
      it "is the same for any number of octaves"
        $ quickCheck \n ->
            octave === (octave ^* n :: MidiIC)
    describe "ic / emb" do
      it "are id for ic . emb" $ quickCheck $ \i -> ic (emb i) === (i :: MidiIC)
      it "ignore octave changes in MidiInterval space"
        $ quickCheck \i n ->
            ic i === ic (i ^+^ octave ^* n :: MidiIC)
    describe "direction" do
      it "goes up for m2 to P4"
        $ for_ [ 2, 3, 4, 5 ]
        $ \n -> direction (midic n) `shouldBe` GT
      it "goes down for P5 to M7"
        $ for_ [ 7, 8, 9, 10, 11 ]
        $ \n -> direction (midic n) `shouldBe` LT
      it "is neutral for unison and tritone" do
        direction (unison :: MidiIC) `shouldBe` EQ
        direction (midic 6) `shouldBe` EQ
    describe "notation" do
      it "survives a notation roundtrip (intervals)"
        $ quickCheck \i -> parseNotation (showNotation i) === Just (i :: MidiIC)
      it "survives a notation roundtrip (pitches)"
        $ quickCheck \p -> parseNotation (showNotation p) === Just (p :: MidiPC)
    describe "JSON" do
      it "survives a JSON roundtrip (intervals)"
        $ quickCheck \i -> readJSON_ (writeJSON i) === Just (i :: MidiIC)
      it "survives a JSON roundtrip (pitches)"
        $ quickCheck \p -> readJSON_ (writeJSON p) === Just (p :: MidiPC)

testMidi =
  describe "Midi" do
    testMidiIntervalProps
    testMidiICProps

    it "constructors" do
      midi 13 `shouldBe` MidiInterval 13
      midip 63 `shouldBe` Pitch (MidiInterval 63)
      midic (-3) `shouldBe` MidiIC 9
      midipc 63 `shouldBe` Pitch (MidiIC 3)
    it "named intervals" do
      unison `shouldBe` midi 0
      octave `shouldBe` midi 12
      chromaticSemitone `shouldBe` midi 1
      unison `shouldBe` midic 0
      octave `shouldBe` midic 0
      chromaticSemitone `shouldBe` midic 1
    it "string notation parsing" do
      parseNotation "i4" `shouldBe` Just (midi 4)
      parseNotation "i-4" `shouldBe` Just (midi (-4))
      parseNotation "p63" `shouldBe` Just (midip 63)
      parseNotation "p-3" `shouldBe` Just (midip (-3))
      parseNotation "ic3" `shouldBe` Just (midic 3)
      parseNotation "ic-3" `shouldBe` Just (midic 9)
      parseNotation "pc3" `shouldBe` Just (midipc 3)
      parseNotation "pc63" `shouldBe` Just (midipc 3)
    it "printing notation" do
      showNotation (midi 3) `shouldBe` "i3"
      showNotation (midip 63) `shouldBe` "p63"
      showNotation (midic 63) `shouldBe` "ic3"
      showNotation (midipc 63) `shouldBe` "pc3"

    describe "Interval Interface" do
      it "addition / subtraction / negation" do
        (midi 3 ^+^ midi 4) `shouldBe` midi 7
        (midi 3 ^+^ midi 11) `shouldBe` midi 14
        (midi 7 ^+^ midi 7) `shouldBe` midi 14
        (midi (-3) ^+^ midi 4) `shouldBe` midi 1
        (midi 3 ^+^ midi (-4)) `shouldBe` midi (-1)
        (midi 3 ^-^ midi 4) `shouldBe` midi (-1)
        (midi 3 ^-^ midi 9) `shouldBe` midi (-6)
        down (midi 5) `shouldBe` midi (-5)
        down (midi 7) `shouldBe` midi (-7)
        down (midi 7) `shouldBe` (unison ^-^ midi 7)
      it "multiplication" do
        (midi 7 ^* 2) `shouldBe` midi 14
        (midi 2 ^* 4) `shouldBe` midi 8
        (midi (-3) ^* 4) `shouldBe` midi (-12)
        (midi 4 ^* (-3)) `shouldBe` midi (-12)
        (4 *^ midi 2) `shouldBe` midi 8
        (4 *^ midi (-4)) `shouldBe` midi (-16)
        (5 *^ midi (-4)) `shouldBe` midi (-20)
      it "numeric operations" do
        direction (midi 1) `shouldBe` GT
        direction (midi 0) `shouldBe` EQ
        direction (midi (-3)) `shouldBe` LT
        iabs (midi (-3)) `shouldBe` midi 3
      it "interval class conversion" do
        ic (midi 15) `shouldBe` midic 3
        emb (midi 3) `shouldBe` midi 3
      it "steps (true)" do
        isStep (midi (-2)) `shouldBe` true
        isStep (midi (-1)) `shouldBe` true
        isStep (midi 0) `shouldBe` true
        isStep (midi 1) `shouldBe` true
        isStep (midi 2) `shouldBe` true
      it "steps (false)" do
        isStep (midi 3) `shouldBe` false
        isStep (midi 11) `shouldBe` false
        isStep (midi 12) `shouldBe` false
        isStep (midi 13) `shouldBe` false
        isStep (midi (-3)) `shouldBe` false
        isStep (midi (-11)) `shouldBe` false
        isStep (midi (-12)) `shouldBe` false
        isStep (midi (-13)) `shouldBe` false

    describe "Interval Class Interface" do
      it "addition / subtraction / negation" do
        (midic 3 ^+^ midic 4) `shouldBe` midic 7
        (midic 3 ^+^ midic 11) `shouldBe` midic 14
        (midic 7 ^+^ midic 7) `shouldBe` midic 14
        (midic (-3) ^+^ midic 4) `shouldBe` midic 1
        (midic 3 ^+^ midic (-4)) `shouldBe` midic (-1)
        (midic 3 ^-^ midic 4) `shouldBe` midic (-1)
        (midic 3 ^-^ midic 9) `shouldBe` midic (-6)
        down (midic 5) `shouldBe` midic (-5)
        down (midic 7) `shouldBe` midic (-7)
        down (midic 7) `shouldBe` (unison ^-^ midic 7)
      it "multiplication" do
        (midic 7 ^* 2) `shouldBe` midic 14
        (midic 2 ^* 4) `shouldBe` midic 8
        (midic (-3) ^* 4) `shouldBe` midic (-12)
        (midic 4 ^* (-3)) `shouldBe` midic (-12)
        (4 *^ midic 2) `shouldBe` midic 8
        (4 *^ midic (-4)) `shouldBe` midic (-16)
        (5 *^ midic (-4)) `shouldBe` midic (-20)
      it "numeric operations" do
        direction (midic 1) `shouldBe` GT
        direction (midic 0) `shouldBe` EQ
        direction (midic 6) `shouldBe` EQ
        direction (midic (-3)) `shouldBe` LT
        iabs (midic (-3)) `shouldBe` midic 3
      it "interval class conversion" do
        ic (midic 15) `shouldBe` midic 3
        ic (midic (-16)) `shouldBe` midic 8
        emb (midic 3) `shouldBe` midi 3
        emb (midic 15) `shouldBe` midi 3
      it "steps (true)" do
        isStep (midic (-2)) `shouldBe` true
        isStep (midic (-1)) `shouldBe` true
        isStep (midic 0) `shouldBe` true
        isStep (midic 1) `shouldBe` true
        isStep (midic 2) `shouldBe` true
      it "steps (false)" do
        isStep (midic 3) `shouldBe` false
        isStep (midic 9) `shouldBe` false

    describe "Pitch Interface" do
      it "conversion" do
        toPitch (midi 63) `shouldBe` midip 63
        toInterval (midip 61) `shouldBe` midi 61
        pc (midip 63) `shouldBe` midipc 3
      it "arithmetics" do
        (midip 63 +^ midi 7) `shouldBe` midip 70
        (midip 63 +^ midi (-3)) `shouldBe` midip 60
        (midip 63 -^ midi 7) `shouldBe` midip 56
        (midip 67 `pfrom` midip 61) `shouldBe` midi 6
        (midip 67 `pto` midip 61) `shouldBe` midi (-6)

    describe "Pitch Class Interface" do
      it "conversion" do
        toPitch (midic 3) `shouldBe` midipc 3
        toInterval (midipc 4) `shouldBe` midic 4
        pc (midipc 3) `shouldBe` midipc 3
        (emb <$> midipc 3) `shouldBe` midip 3
      it "arithmetics" do
        (midipc 3 +^ midic 7) `shouldBe` midipc 10
        (midipc 3 +^ midic (-3)) `shouldBe` midipc 0
        (midipc 3 -^ midic 7) `shouldBe` midipc 8
        (midipc 7 `pfrom` midipc 1) `shouldBe` midic 6
        (midipc 7 `pto` midipc 1) `shouldBe` midic 6
