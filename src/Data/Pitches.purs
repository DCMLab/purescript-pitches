-- | A small library for working with musical pitches and intervals
-- | in a systematic and well-defined way.
-- |
-- | This module reexports the following submodules of this library for convenience:
-- | - [Data.Pitches.Class](./Data.Pitches.Class.html) - the generic interval and pitch interface
-- | - [Data.Pitches.Spelled](./Data.Pitches.Spelled.html) - types for spelled pitches and intervals
-- | - [Data.Pitches.Midi](./Data.Pitches.Midi.html) - types for MIDI pitches and intervals
module Data.Pitches
  ( module Data.Pitches.Class
  , module Data.Pitches.Spelled
  , module Data.Pitches.Midi
  ) where

import Data.Pitches.Class
import Data.Pitches.Spelled
import Data.Pitches.Midi
