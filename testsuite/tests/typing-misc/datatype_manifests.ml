(* TEST
   expect;
*)

type t = int * int = { foo : string }

[%%expect
{|
Line 1, characters 0-37:
1 | type t = int * int = { foo : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "int * int"
|}]

let f (x : int * int) : t = x

[%%expect
{|
Line 1, characters 24-25:
1 | let f (x : int * int) : t = x
                            ^
Error: Unbound type constructor "t"
|}]
