(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Type-directed disambiguation of records whose expected type is a box of an
   implicit unboxed record type, which is the boxed record type. *)

type r = { a : int64_u; b : string }
type s = { a : int; b : string }
[%%expect{|
type r = { a : int64_u; b : string; }
type s = { a : int; b : string; }
|}]

let x : r# box = { a = #1L; b = "x" }
[%%expect{|
val x : r = {a = <abstr>; b = "x"}
|}]

let x : r# box = { a = #1L; b = "x" }
let r : r = x
[%%expect{|
val x : r = {a = <abstr>; b = "x"}
val r : r = {a = <abstr>; b = "x"}
|}]

let f (x : r# box) =
  match x with
  | { a = _; b } -> b
[%%expect{|
val f : r -> string = <fun>
|}]

let f (x : r# box) = x.b
[%%expect{|
val f : r -> string = <fun>
|}]

let f (x : r# box) = { x with b = "y" }
[%%expect{|
val f : r -> r = <fun>
|}]

type r_box = r# box
let x : r_box = { a = #1L; b = "x" }
[%%expect{|
type r_box = r
val x : r_box = {a = <abstr>; b = "x"}
|}]

(* Errors *)

let x : r# box = #{ a = #1L; b = "x" }
[%%expect{|
Line 1, characters 17-38:
1 | let x : r# box = #{ a = #1L; b = "x" }
                     ^^^^^^^^^^^^^^^^^^^^^
Error: This unboxed record expression should be boxed instead,
       the expected type is "r"
|}]

type u = #{ c : int; d : string }
let x : u box = { c = 1; d = "x" }
[%%expect{|
type u = #{ c : int; d : string; }
Line 2, characters 16-34:
2 | let x : u box = { c = 1; d = "x" }
                    ^^^^^^^^^^^^^^^^^^
Error: This expression should not be a record, the expected type is "u box"
|}]

let x : u box = #{ c = 1; d = "x" }
[%%expect{|
Line 1, characters 16-35:
1 | let x : u box = #{ c = 1; d = "x" }
                    ^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a unboxed record, the expected type is
       "u box"
|}]
