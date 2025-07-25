(* TEST
   expect;
*)
type u = < x : int >

type t = private < u ; .. >

let f (x : t) (y : u) = x = y

[%%expect
{|
type u = < x : int >
type t = private < x : int; .. >
Line 5, characters 28-29:
5 | let f (x : t) (y : u) = x = y
                                ^
Error: This expression has type "u" but an expression was expected of type "t"
       The second object type has an abstract row, it cannot be closed
|}]

let g (x : u) (y : t) = x = y

[%%expect
{|
Line 1, characters 28-29:
1 | let g (x : u) (y : t) = x = y
                                ^
Error: This expression has type "t" but an expression was expected of type "u"
       The first object type has an abstract row, it cannot be closed
|}]
