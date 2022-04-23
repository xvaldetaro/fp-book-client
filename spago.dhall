{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "js-date"
  , "maybe"
  , "now"
  , "prelude"
  , "psci-support"
  , "refs"
  , "routing"
  , "routing-duplex"
  , "transformers"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
