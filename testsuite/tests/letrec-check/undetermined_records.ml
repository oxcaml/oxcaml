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

let rec x = { v = 1; next = Some x }
[%%expect{|
val x : int t = {v = 1; next = Some <cycle>}
|}];;

let rec a = { back = b }
and b = { v = a; next = None }
[%%expect{|
val a : r = {back = {v = <cycle>; next = None}}
val b : r t = {v = {back = <cycle>}; next = None}
|}];;

type ('a : any) w = A of 'a * 'a w option
[%%expect{|
type ('a : any) w = A of 'a * 'a w option
|}];;

let rec y = A (1, Some y)
[%%expect{|
val y : int w = A (1, Some <cycle>)
|}];;

type q = { unwrap : q w }
[%%expect{|
type q = { unwrap : q w; }
|}];;

let rec c = { unwrap = d }
and d = A (c, None)
[%%expect{|
val c : q = {unwrap = A (<cycle>, None)}
val d : q w = A ({unwrap = <cycle>}, None)
|}];;

type ('a : any) opt = N | S of 'a
type tree = { left : tree opt; right : tree opt; data : int }
let rec leaf = { left = S leaf; right = S leaf; data = 42 }
[%%expect{|
type ('a : any) opt = N | S of 'a
type tree = { left : tree opt; right : tree opt; data : int; }
val leaf : tree = {left = S <cycle>; right = S <cycle>; data = 42}
|}];;
