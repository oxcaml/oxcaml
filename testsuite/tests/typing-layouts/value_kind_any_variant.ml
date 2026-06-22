(* TEST
 flags = "-extension layouts_alpha -dlambda";
 expect;
*)

(* Regression test for the value_kind of a boxed variant whose type parameter
   has kind [any].  At typedecl time such a constructor's representation is
   [Cstr_layout_variable], but once the type is instantiated [Typeopt.value_kind]
   recomputes it (via the [constructor_representation_for_value_kind] forward
   reference) so that the [opt] binding below gets a precise [Pvariant]
   value_kind rather than the conservative [Pgenval].  Without that, the
   construct-then-match in [f] would not have its allocation eliminated. *)

type ('a : any) t = A of 'a | B
[%%expect{|
0
type ('a : any) t = A of 'a | B
|}]

(* The [opt] binding's value_kind is a [Pvariant] with the instantiated field
   ([value<int>]), not a bare [value]. *)
let f (b : bool) (x : int) =
  let opt = if b then B else A x in
  match opt with B -> 1 | A x -> x
[%%expect{|
(let
  (f/292 =
     (function {nlocal = 0} b/294[value<int>] x/295[value<int>] : int
       (region
         (let
           (opt/296 =[value<(consts (0)) (non_consts ([0: value<int>]))>]
              (if b/294 0 (makelocalblock 0 (value<int>) x/295)))
           (if opt/296 (field_imm 0 opt/296) 1)))))
  (apply (field_imm 1 (global Toploop!)) "f" f/292))
val f : bool -> int -> int = <fun>
|}]

(* A [float#] (kind float64) argument: the recomputed representation is a mixed
   block, so the value_kind is [Pvariant] with a [float64] field. *)
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
  (g/299 =
     (function {nlocal = 0} b/301[value<int>] x/302[float] : float
       (region
         (let
           (opt/303 =[value<(consts (0)) (non_consts ([0: float64]))>]
              (if b/301 0 (makelocalblock 0 (float64) x/302)))
           (if opt/303 (%float_of_float# (mixedfield 0  (float64) opt/303))
             0.0)))))
  (apply (field_imm 1 (global Toploop!)) "g" g/299))
val g : bool -> float# -> float = <fun>
|}]
