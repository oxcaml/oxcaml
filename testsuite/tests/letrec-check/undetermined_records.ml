(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Recursive bindings of records and variants containing [any] *)

type ('a : any) t = { v : 'a; mutable next : 'a t option }
type r = { back : r t }
[%%expect{|
type ('a : any) t = { v : 'a; mutable next : 'a t option; }
type r = { back : r t; }
|}];;

(* Per the declaration, [next] is always scannable, so it can be recursive *)
let rec x = { v = 1; next = Some x }
[%%expect{|
val x : int t = {v = 1; next = Some <cycle>}
|}];;

(* [v] might not be scannable, so we conservatively disallow it from being
   recursive to avoid order-dependence between [value_rec_check] and filling the
   sort variable *)
let rec a = { back = b }
and b = { v = a; next = None }
[%%expect{|
Line 2, characters 8-30:
2 | and b = { v = a; next = None }
            ^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

(* The same allowed case, but with a variant *)
type ('a : any) w = A of 'a * 'a w option
[%%expect{|
type ('a : any) w = A of 'a * 'a w option
|}];;

let rec y = A (1, Some y)
[%%expect{|
val y : int w = A (1, Some <cycle>)
|}];;

(* And the conservatively-disallowed case, with a variant *)
type q = { unwrap : q w }
[%%expect{|
type q = { unwrap : q w; }
|}];;

let rec c = { unwrap = d }
and d = A (c, None)
[%%expect{|
Line 2, characters 8-19:
2 | and d = A (c, None)
            ^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

type ('a : any) opt = N | S of 'a
type tree = { left : tree opt; right : tree opt; data : int }
let rec leaf = { left = S leaf; right = S leaf; data = 42 }
[%%expect{|
type ('a : any) opt = N | S of 'a
type tree = { left : tree opt; right : tree opt; data : int; }
Line 3, characters 15-59:
3 | let rec leaf = { left = S leaf; right = S leaf; data = 42 }
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;
