(* TEST
 expect;
*)
(* CR 5.4-merge: expect tests auto-resolved in favour of upstream *)

module M = struct type t = A | B end
let rec f () = g A
and g (x : M.t) = f ()
[%%expect{|
module M : sig type t = A | B end
Line 2, characters 17-18:
2 | let rec f () = g A
                     ^
Error: Unbound constructor "A"
|}]

let rec f () = g 42
and g (x : string) = f ()
[%%expect{|
Line 1, characters 17-19:
1 | let rec f () = g 42
                     ^^
Error: The constant "42" has type "int" but an expression was expected of type
         "string"
|}]

let rec opt_error ?(opt : string) () = f ?opt ()
[%%expect{|
Line 1, characters 20-32:
1 | let rec opt_error ?(opt : string) () = f ?opt ()
                        ^^^^^^^^^^^^
Error: This pattern matches values of type "string"
       but a pattern was expected which matches values of type "'a option"
|}]

let rec opt_ok_f () = opt_ok_g ~foo:A ~bar:A ()
and opt_ok_g ?(foo : M.t option) ?(bar : M.t = M.A) () = opt_ok_f ()
[%%expect{|
Line 1, characters 36-37:
1 | let rec opt_ok_f () = opt_ok_g ~foo:A ~bar:A ()
                                        ^
Error: Unbound constructor "A"
|}]
