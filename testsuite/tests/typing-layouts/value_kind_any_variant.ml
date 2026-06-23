(* TEST
 flags = "-extension layouts_alpha -dlambda -dno-unique-ids";
 expect;
*)

(* This lambda test should show that value_kind for [opt] contains its
   constructors (be [Pvariant _] rather than [Pgenval]) *)

type ('a : any) t = A of 'a | B
[%%expect{|
0
type ('a : any) t = A of 'a | B
|}]

let f (b : bool) (x : int) =
  let opt = if b then B else A x in
  match opt with B -> 1 | A x -> x
[%%expect{|
(let
  (f =
     (function {nlocal = 0} b[value<int>] x[value<int>] : int
       (region
         (let (opt = (if b 0 (makelocalblock 0 (value<int>) x)))
           (if opt (field_imm 0 opt) 1)))))
  (apply (field_imm 1 (global Toploop!)) "f" f))
val f : bool -> int -> int = <fun>
|}]

external box_float : float# -> float = "%box_float"
[%%expect{|
0
external box_float : float# -> float = "%box_float"
|}]

let g (b : bool) (x : float#) =
  let opt = if b then B else A x in
  match opt with B -> 0.0 | A x -> box_float x
[%%expect{|
(let
  (g =
     (function {nlocal = 0} b[value<int>] x[float] : float
       (region
         (let (opt = (if b 0 (makelocalblock 0 (float64) x)))
           (if opt (%float_of_float# (mixedfield 0  (float64) opt)) 0.0)))))
  (apply (field_imm 1 (global Toploop!)) "g" g))
val g : bool -> float# -> float = <fun>
|}]

(* The field's sort is undetermined, so the value_kind stays conservative *)
let opaque (type a : any) (r : a t) = r
[%%expect{|
(let (opaque = (function {nlocal = 0} r r))
  (apply (field_imm 1 (global Toploop!)) "opaque" opaque))
val opaque : ('a : any). 'a t -> 'a t = <fun>
|}]
