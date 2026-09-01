type t = { foo : int; bar : int }

(* fun pattern - the outer let uses type_let_def_wrap_warnings,
   but the fun parameter should go through check_usage *)
let f ({ foo; bar } : t) = foo

(* match pattern - goes through check_usage *)
let g x = match (x : t) with { foo; bar } -> foo

(* function pattern - goes through check_usage *)
let h = function ({ foo; bar } : t) -> foo
