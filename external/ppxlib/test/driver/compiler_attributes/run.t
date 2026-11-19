Attributes that the compiler itself interprets (rather than a ppx) are
allowlisted in [Name.Allowlisted], so they pass the driver's checks untouched
instead of being reported as unused.

  $ cat > test.ml <<EOF
  > let x = 1 [@@foo]
  > EOF
  $ ./driver.exe -check -impl test.ml
  [%%ocaml.error "Attribute `foo' was not used"]
  let x = 1[@@foo ]

  $ cat > test.ml <<EOF
  > let[@inline] f x = x
  > EOF
  $ ./driver.exe -check -impl test.ml
  let f x = x[@@inline ]

[poll] is one such compiler-interpreted (OxCaml) attribute: [let[@poll error]]
forbids the compiler from inserting polling points in a function. Like
[@inline], it must survive ppx preprocessing rather than erroring with
"Attribute `poll' was not used".

  $ cat > test.ml <<EOF
  > let[@poll error] g x = x
  > EOF
  $ ./driver.exe -check -impl test.ml
  let g x = x[@@poll error]
