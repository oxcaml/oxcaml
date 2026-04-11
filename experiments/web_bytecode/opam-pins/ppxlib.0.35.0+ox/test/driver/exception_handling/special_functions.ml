open Ppxlib

let expand e = Location.raise_errorf ~loc:e.pexp_loc "error special function"

let expand2 e =
  Location.raise_errorf ~loc:e.pexp_loc "second error special function"

let rule = Context_free.Rule.special_function "n_args" expand
let rule2 = Context_free.Rule.special_function "n_args2" expand2;;

Driver.register_transformation ~rules:[ rule; rule2 ] "special_function_demo"

let () = Driver.standalone ()
