[Ast_builder.eta_reduce_if_possible] eta-reduces a function which only applies a
function to its parameters.

  $ ./test.exe
  eta reduction applies:
  f
  f
  eta reduction doesn't apply:
  function | x -> f x
  fun y -> f x y
  fun x -> f x y
  fun x -> Non_identifier x
  ((fun x y -> f x y)[@attr ])
  fun x -> ((fun y -> f x y)[@attr ])
