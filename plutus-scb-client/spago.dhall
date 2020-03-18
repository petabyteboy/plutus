{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "plutus-playground-client"
, dependencies =
    [ "prelude"
    , "aff"
    , "console"
    , "debug"
    , "effect"
    , "halogen"
    , "argonaut-codecs"
    , "foreign-generic"
    , "psci-support"
    , "transformers"
    , "remotedata"
    , "servant-support"
    , "test-unit"
    , "uuid"
    , "newtype"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs"
    , "test/**/*.purs"
    , "generated/**/*.purs"
    , "../web-common/src/AjaxUtils.purs"
    , "../web-common/src/Bootstrap.purs"
    , "../web-common/src/Icons.purs"
    , "../web-common/src/Data/**/*.purs"
    , "../web-common/src/Language/**/*.purs"
    ]
}
