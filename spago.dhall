{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "const"
  , "control"
  , "css"
  , "datetime"
  , "effect"
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
  , "tailrec"
  , "transformers"
  , "undefined"
  , "uuid"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
