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
Line 1, characters 17-37:
1 | let x : r# box = { a = #1L; b = "x" }
                     ^^^^^^^^^^^^^^^^^^^^
Error: This boxed record expression should be unboxed instead,
       the expected type is "r"
|}]

let x : r# box = { a = #1L; b = "x" }
let r : r = x
[%%expect{|
Line 1, characters 17-37:
1 | let x : r# box = { a = #1L; b = "x" }
                     ^^^^^^^^^^^^^^^^^^^^
Error: This boxed record expression should be unboxed instead,
       the expected type is "r"
|}]

let f (x : r# box) =
  match x with
  | { a = _; b } -> b
[%%expect{|
Line 3, characters 4-16:
3 |   | { a = _; b } -> b
        ^^^^^^^^^^^^
Error: This boxed record pattern should be unboxed instead,
       the expected type is "r"
|}]

let f (x : r# box) = x.b
[%%expect{|
Line 1, characters 21-22:
1 | let f (x : r# box) = x.b
                         ^
Error: This expression has type "r",
       which is an unboxed record rather than a boxed one.
|}]

let f (x : r# box) = { x with b = "y" }
[%%expect{|
Line 1, characters 21-39:
1 | let f (x : r# box) = { x with b = "y" }
                         ^^^^^^^^^^^^^^^^^^
Error: This expression has type "r",
       which is an unboxed record rather than a boxed one.
|}]

type r_box = r# box
let x : r_box = { a = #1L; b = "x" }
[%%expect{|
type r_box = r
Line 2, characters 16-36:
2 | let x : r_box = { a = #1L; b = "x" }
                    ^^^^^^^^^^^^^^^^^^^^
Error: This boxed record expression should be unboxed instead,
       the expected type is "r_box"
|}]

(* Errors *)

let x : r# box = #{ a = #1L; b = "x" }
[%%expect{|
Line 1, characters 20-21:
1 | let x : r# box = #{ a = #1L; b = "x" }
                        ^
Error: The unboxed record field "a" belongs to the type "r#"
       but is mixed here with fields of type "r# box" = "r"
|}]

type u = #{ c : int; d : string }
let x : u box = { c = 1; d = "x" }
[%%expect{|
type u = #{ c : int; d : string; }
Line 2, characters 16-34:
2 | let x : u box = { c = 1; d = "x" }
                    ^^^^^^^^^^^^^^^^^^
Error: This boxed record expression should be unboxed instead,
       the expected type is "u box"
|}]

let x : u box = #{ c = 1; d = "x" }
[%%expect{|
Line 1, characters 19-20:
1 | let x : u box = #{ c = 1; d = "x" }
                       ^
Error: The unboxed record field "c" belongs to the type "u"
       but is mixed here with fields of type "u box"
|}]
