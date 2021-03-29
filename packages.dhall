let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201204/packages.dhall sha256:89f184cea1ca40630ea34fb68972589b8eedf4809275686aef85f86abaa2145f
let overrides = { metadata = upstream.metadata // { version = "v0.14.0-rc3" } }
in  upstream
    with purescript-erlps-core =
        { dependencies =
          [ "prelude" ]
        , repo =
          "https://github.com/erlscripten/erlps-core.git"
        , version =
          "main"
        }
    with purescript-erlps-stdlib =
        { dependencies =
          [ "prelude", "purescript-erlps-core" ]
        , repo =
          "https://github.com/erlscripten/erlps-stdlib.git"
        , version =
          "main"
        }
     with purescript-erlps-aeserialization =
        { dependencies =
          [ "prelude", "purescript-erlps-core", "purescript-erlps-stdlib" ]
        , repo =
          "https://github.com/erlscripten/erlps-aeserialization.git"
        , version =
          "main"
        }
    with purescript-erlps-aebytecode =
        { dependencies =
          [ "prelude", "purescript-erlps-core", "purescript-erlps-aeserialization" ]
        , repo =
          "https://github.com/erlscripten/erlps-aebytecode.git"
        , version =
          "main"
        }
    with base58 =
        { dependencies =
          [ "prelude" ]
        , repo =
          "https://github.com/throughnothing/purescript-base58.git"
        , version =
          "v0.0.3"
        }
    with math.repo = "https://github.com/minoki/purescript-math/"
    with math.version = "es6-functions"





