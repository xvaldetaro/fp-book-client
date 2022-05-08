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
  , "foreign-generic"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "js-date"
  , "maybe"
  , "nonempty"
  , "now"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "strings"
  , "tailrec"
  , "transformers"
  , "undefined"
  , "uuid"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs", "../server/src/Data/Api/**/*.purs", "test/**/*.purs" ]
}
