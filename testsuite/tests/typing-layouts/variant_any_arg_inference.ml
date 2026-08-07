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
val g : unit -> float# t = <fun>
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
val h : float# t -> float# = <fun>
|}]

type ('a : any) pair = P of 'a * int
[%%expect{|
type ('a : any) pair = P of 'a * int
|}]

let use () =
  let mk x = P (x, 1) in
  mk #3.14
[%%expect{|
val use : unit -> float# pair = <fun>
|}]

let up () =
  let f x = A x in
  f #(1, "a")
[%%expect{|
val up : unit -> #(int * string) t = <fun>
|}]

type ('a : any) r = { v : 'a; n : int }
[%%expect{|
type ('a : any) r = { v : 'a; n : int; }
|}]

let rec_mk () =
  let mk x = { v = x; n = 0 } in
  mk #3.14
[%%expect{|
val rec_mk : unit -> float# r = <fun>
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
