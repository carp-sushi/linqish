{ name = "linqish"
, dependencies =
  [ "arrays", "console", "control", "effect", "prelude", "test-unit", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
