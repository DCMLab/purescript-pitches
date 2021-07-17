{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pitches"
, dependencies =
  [ "console"
  , "effect"
  , "group"
  , "partial"
  , "quickcheck"
  , "simple-json"
  , "spec"
  , "spec-quickcheck"
  , "string-parsers"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
