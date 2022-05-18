{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pitches"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "group"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
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
