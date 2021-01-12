{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arraybuffer"
  , "base58"
  , "numbers"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "numbers"
  , "lists"
  , "node-buffer"
  , "psci-support"
  , "rationals"
  , "spec"
  , "purescript-erlps-core"
  , "purescript-erlps-stdlib"
  , "purescript-erlps-aeserialization"
  , "purescript-erlps-aebytecode"
  , "b64"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
