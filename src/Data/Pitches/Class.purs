module Data.Pitches.Class
  ( class Interval
  , octave
  , direction
  , iabs
  , unison
  , oct
  , down
  , (^+^)
  , nappend
  , (^-^)
  , (^*)
  , (*^)
  , rewop
  , class IntervalClassOf
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
  , class ParseNotation
  , parseNotation
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
  , class ShowPitch
  , showPitch
  , class ParsePitchNotation
  , parsePitchNotation
  , class ToMidiPitch
  , toMidiPitch
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Group (class Group, ginverse, power)
import Data.Maybe (Maybe)

---------------
-- Intervals --
---------------
class
  (Group i) <= Interval i where
  octave :: i
  direction :: i -> Ordering
  iabs :: i -> i

class
  (Interval i, Interval ic, HasIntervalClass i ic) <= IntervalClassOf ic i | i -> ic, ic -> i where
  emb :: ic -> i

class
  (Interval i, Interval ic) <= HasIntervalClass i ic | i -> ic where
  ic :: i -> ic

class
  (Interval i) <= Diatonic i where
  isStep :: i -> Boolean

class
  (Interval i) <= Chromatic i where
  chromaticSemitone :: i

class ToMidi i where
  toMidi :: i -> Int

class ParseNotation a where
  parseNotation :: String -> Maybe a

unison :: forall i. Interval i => i
unison = mempty

oct :: forall i ic. IntervalClassOf ic i => Int -> ic -> i
oct octs ic = emb ic <> (octave `power` octs)

down :: forall i. Interval i => i -> i
down = ginverse

infixl 6 append as ^+^

nappend :: forall g. Group g => g -> g -> g
nappend a b = a <> ginverse b

infixl 6 nappend as ^-^

infixl 7 power as ^*

rewop :: forall g. Group g => Int -> g -> g
rewop = flip power

infixr 7 rewop as *^

newtype ImperfectInterval i
  = Impf (i -> i)

minor :: forall i. Chromatic i => ImperfectInterval i -> i
minor (Impf int) = int chromaticSemitone

major :: forall i. Interval i => ImperfectInterval i -> i
major (Impf int) = int unison

dim :: forall i. Chromatic i => i -> i
dim = (_ ^-^ chromaticSemitone)

aug :: forall i. Chromatic i => i -> i
aug = (_ ^+^ chromaticSemitone)

-------------
-- Pitches --
-------------
newtype Pitch a
  = Pitch a

derive newtype instance eqPitch :: (Eq a) => Eq (Pitch a)

derive newtype instance ordPitch :: (Ord a) => Ord (Pitch a)

derive instance functorPitch :: Functor Pitch

derive instance genericPitch :: Generic (Pitch a) _

toPitch :: forall i. (Interval i) => i -> Pitch i
toPitch = Pitch

toInterval :: forall i. (Interval i) => Pitch i -> i
toInterval (Pitch i) = i

pto :: forall i. (Interval i) => Pitch i -> Pitch i -> i
pto (Pitch a) (Pitch b) = b <> down a

pfrom :: forall i. (Interval i) => Pitch i -> Pitch i -> i
pfrom (Pitch a) (Pitch b) = a <> down b

pplusi :: forall i. (Interval i) => Pitch i -> i -> Pitch i
pplusi (Pitch a) b = Pitch (a <> b)

infixl 6 pplusi as +^

iplusp :: forall i. (Interval i) => i -> Pitch i -> Pitch i
iplusp a (Pitch b) = Pitch (a <> b)

infixr 6 iplusp as ^+

pminusi :: forall i. (Interval i) => Pitch i -> i -> Pitch i
pminusi (Pitch a) b = Pitch (a <> down b)

infixl 6 pminusi as -^

pc :: forall i ic. (HasIntervalClass i ic) => Pitch i -> Pitch ic
pc = map ic

class ShowPitch i where
  showPitch :: i -> String

instance showPitchInst :: ShowPitch a => Show (Pitch a) where
  show (Pitch i) = showPitch i

class ToMidiPitch i where
  toMidiPitch :: i -> Int

instance tomidiPitch :: ToMidiPitch a => ToMidi (Pitch a) where
  toMidi (Pitch i) = toMidiPitch i

class ParsePitchNotation a where
  parsePitchNotation :: String -> Maybe (Pitch a)

instance parsenotationPitch :: ParsePitchNotation a => ParseNotation (Pitch a) where
  parseNotation = parsePitchNotation
