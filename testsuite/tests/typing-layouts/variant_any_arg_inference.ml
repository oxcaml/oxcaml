(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* Variable representation records and variants used in functions that don't
   constrain the sort variable *)

type ('a : any) t = A of 'a
[%%expect{|
type ('a : any) t = A of 'a
|}]

let g () =
  let f x =
    let t = A x in
    t
  in
  f #3.14
[%%expect{|
Line 6, characters 4-9:
6 |   f #3.14
        ^^^^^
Error: This constant has type "float#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because of the definition of f at lines 2-4, characters 8-5.
|}]

(* This errors when split into separate toplevel statements, as [f]'s sort is
   defaulted after the first *)
let f x =
  let t = A x in
  t
[%%expect{|
val f : 'a -> 'a t = <fun>
|}]

let _ = f #3.14
[%%expect{|
Line 1, characters 10-15:
1 | let _ = f #3.14
              ^^^^^
Error: This constant has type "float#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because of the definition of f at lines 1-3, characters 6-3.
|}]

let h (A x : _ t) : float# = x
[%%expect{|
Line 1, characters 29-30:
1 | let h (A x : _ t) : float# = x
                                 ^
Error: The value "x" has type "('a : value_or_null)"
       but an expression was expected of type "float#"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because it's the type of a constructor argument being projected.
|}]

type ('a : any) pair = P of 'a * int
[%%expect{|
type ('a : any) pair = P of 'a * int
|}]

let use () =
  let mk x = P (x, 1) in
  mk #3.14
[%%expect{|
Line 3, characters 5-10:
3 |   mk #3.14
         ^^^^^
Error: This constant has type "float#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because of the definition of mk at line 2, characters 9-21.
|}]

let up () =
  let f x = A x in
  f #(1, "a")
[%%expect{|
Line 3, characters 4-13:
3 |   f #(1, "a")
        ^^^^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value_or_null)"
       The layout of #('a * 'b) is
           '_representable_layout_1 & '_representable_layout_2
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a value layout
         because of the definition of f at line 2, characters 8-15.
|}]

type ('a : any) r = { v : 'a; n : int }
[%%expect{|
type ('a : any) r = { v : 'a; n : int; }
|}]

let rec_mk () =
  let mk x = { v = x; n = 0 } in
  mk #3.14
[%%expect{|
>> Fatal error: Layout is not a value
Uncaught exception: Misc.Fatal_error

|}]

let rec_get (r : _ r) : float# = r.v
[%%expect{|
val rec_get : float# r -> float# = <fun>
|}]

let rec_pat ({ v; n } : _ r) : float# =
  ignore n;
  v
[%%expect{|
val rec_pat : float# r -> float# = <fun>
|}]
