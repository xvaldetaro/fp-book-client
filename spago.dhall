{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "const"
  , "control"
  , "css"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-generic"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "js-date"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "undefined"
  , "uuid"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "../server/src/Data/Api/**/*.purs", "test/**/*.purs" ]
}
