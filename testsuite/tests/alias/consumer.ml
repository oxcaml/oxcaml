(* Compiled WITHOUT [-alias]: this type-checks because [use_foo.cmi] carries a
   hidden [module Foo = Bar] alias, so [Use_foo.y : Foo.t] resolves to
   [Bar.t]. *)
let z : Bar.t = Use_foo.y
