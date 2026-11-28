(* TEST
 expect;
*)

(* Recursive bindings of records and variants containing [any] *)

type ('a : any) t = { v : 'a; mutable next : 'a t option }
type r = { back : r t }
[%%expect{|
type ('a : any) t = { v : 'a; mutable next : 'a t option; }
type r = { back : r t; }
|}];;

let rec x = { v = 1; next = Some x }
[%%expect{|
val x : int t = {v = 1; next = Some <cycle>}
|}];;

let rec a = { back = b }
and b = { v = a; next = None }
[%%expect{|
Line 1, characters 12-24:
1 | let rec a = { back = b }
                ^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

type ('a : any) w = A of 'a * 'a w option
[%%expect{|
type ('a : any) w = A of 'a * 'a w option
|}];;

let rec y = A (1, Some y)
[%%expect{|
Line 1, characters 12-25:
1 | let rec y = A (1, Some y)
                ^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}];;

type q = { unwrap : q w }
[%%expect{|
type q = { unwrap : q w; }
|}];;

let rec c = { unwrap = d }
and d = A (c, None)
[%%expect{|
Line 1, characters 12-26:
1 | let rec c = { unwrap = d }
                ^^^^^^^^^^^^^^
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
