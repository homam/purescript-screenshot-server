{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "console"
    , "datetime"
    , "effect"
    , "express"
    , "js-date"
    , "node-process"
    , "now"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
