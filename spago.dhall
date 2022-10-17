{ name = "linqish"
, dependencies =
  [ "arrays", "control", "effect", "prelude", "test-unit", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
