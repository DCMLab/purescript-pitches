module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import TestMidi (testMidi)
import TestSpelled (testSpelled)

main :: Effect Unit
main = do
  log "hi"
  launchAff_
    $ runSpec [ consoleReporter ] do
        testSpelled
        testMidi
