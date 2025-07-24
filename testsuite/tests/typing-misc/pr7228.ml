(* TEST
   expect;
*)

type t = A of { mutable x : int };;

fun (A r) -> r.x <- 42

[%%expect {|
type t = A of { mutable x : int; }
- : t -> unit = <fun>
|}]

(* Check that mutability is blocked for inline records on private types *)
type t = private A of { mutable x : int };;

fun (A r) -> r.x <- 42

[%%expect
{|
type t = private A of { mutable x : int; }
Line 3, characters 15-16:
3 | fun (A r) -> r.x <- 42
                   ^
Error: Cannot assign field "x" of the private type "t.A"
|}]
