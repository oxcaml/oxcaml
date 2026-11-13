(* TEST
 flags = "-rectypes";
 expect;
*)

(* Every cycle of recursive definitions must go through a function, a lazy or
   a mutable field. *)

type t = A | B of { t : t; mutable payload : int }
let rec t = B { t; payload = 1 };;
[%%expect{|
type t = A | B of { t : t; mutable payload : int; }
Line 2, characters 12-32:
2 | let rec t = B { t; payload = 1 };;
                ^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

type u = { u : u; mutable payload : int }
let rec u = { u; payload = 1 };;
[%%expect{|
type u = { u : u; mutable payload : int; }
Line 2, characters 12-30:
2 | let rec u = { u; payload = 1 };;
                ^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

type m = { mutable m : m option }
let rec m = { m = Some m };;
[%%expect{|
type m = { mutable m : m option; }
val m : m = {m = Some <cycle>}
|}];;

type c = C of { mutable c : c option }
let rec c = C { c = Some c };;
[%%expect{|
type c = C of { mutable c : c option; }
val c : c = C {c = Some <cycle>}
|}];;

let rec l = 1 :: l;;
[%%expect{|
Line 1, characters 12-18:
1 | let rec l = 1 :: l;;
                ^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = (1, x);;
[%%expect{|
Line 1, characters 12-18:
1 | let rec x = (1, x);;
                ^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = `A x;;
[%%expect{|
Line 1, characters 12-16:
1 | let rec x = `A x;;
                ^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = [: x :];;
[%%expect{|
Line 1, characters 12-19:
1 | let rec x = [: x :];;
                ^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = [| x |];;
[%%expect{|
val x : 'a array as 'a = [|<cycle>|]
|}];;

let rec x = ref x;;
[%%expect{|
val x : 'a ref as 'a = {contents = <cycle>}
|}];;

let rec x = Some (ref x);;
[%%expect{|
val x : 'a ref option as 'a = Some {contents = <cycle>}
|}];;

let rec x = ref (Some x);;
[%%expect{|
val x : 'a option ref as 'a = {contents = Some <cycle>}
|}];;

let rec f = Some (fun () -> f);;
[%%expect{|
val f : (unit -> 'a) option as 'a = Some <fun>
|}];;

let rec x = Some (lazy (Some x));;
[%%expect{|
val x : 'a option lazy_t option as 'a = Some <lazy>
|}];;

(* Mutual recursion: only cycles are rejected. *)

let rec x = Some y
and y = fun () -> x;;
[%%expect{|
val x : (unit -> 'a) option as 'a = Some <fun>
val y : unit -> 'a option as 'a = <fun>
|}, Principal{|
val x : (unit -> 'a) option as 'a = Some <fun>
val y : unit -> ((unit -> 'a) option as 'a) = <fun>
|}];;

let rec x = Some y
and y = Some x;;
[%%expect{|
Line 1, characters 12-18:
1 | let rec x = Some y
                ^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = Some y
and y = ref x;;
[%%expect{|
val x : 'a ref option as 'a = Some {contents = <cycle>}
val y : 'a option ref as 'a = {contents = Some <cycle>}
|}];;

let rec x = Some y
and y = Some y;;
[%%expect{|
Line 2, characters 8-14:
2 | and y = Some y;;
            ^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = Some y
and y = [ z ]
and z = (x, 1);;
[%%expect{|
Line 1, characters 12-18:
1 | let rec x = Some y
                ^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = [y]
and y = 0;;
[%%expect{|
val x : int list = [0]
val y : int = 0
|}];;

(* Local bindings and modules *)

let rec x = let y = x in Some y;;
[%%expect{|
Line 1, characters 12-31:
1 | let rec x = let y = x in Some y;;
                ^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = let y = Some x in ref y;;
[%%expect{|
val x : 'a option ref as 'a = {contents = Some <cycle>}
|}];;

module type Empty = sig end
let rec x = Some (module struct let y = x end : Empty);;
[%%expect{|
module type Empty = sig end
Line 2, characters 12-54:
2 | let rec x = Some (module struct let y = x end : Empty);;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = let module M = struct let y = x end in Some (module M : Empty);;
[%%expect{|
Line 1, characters 12-74:
1 | let rec x = let module M = struct let y = x end in Some (module M : Empty);;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

let rec x = let module M = struct let y = x end in ref (fun () -> M.y);;
[%%expect{|
val x : (unit -> ((unit -> 'a) ref as 'a)) ref = {contents = <fun>}
|}, Principal{|
val x : (unit -> 'a) ref as 'a = {contents = <fun>}
|}];;
