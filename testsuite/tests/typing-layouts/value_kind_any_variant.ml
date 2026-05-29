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
  (f/288 =
     (function {nlocal = 0} b/290[value<int>] x/291[value<int>] : int
       (region
         (let
           (opt/292 =[value<(consts (0)) (non_consts ([0: value<int>]))>]
              (if b/290 0 (makelocalblock 0 (value<int>) x/291)))
           (if opt/292 (field_imm 0 opt/292) 1)))))
  (apply (field_imm 1 (global Toploop!)) "f" f/288))
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
  (g/295 =
     (function {nlocal = 0} b/297[value<int>] x/298[float] : float
       (region
         (let
           (opt/299 =[value<(consts (0)) (non_consts ([0: float64]))>]
              (if b/297 0 (makelocalblock 0 (float64) x/298)))
           (if opt/299 (%float_of_float# (mixedfield 0  (float64) opt/299))
             0.0)))))
  (apply (field_imm 1 (global Toploop!)) "g" g/295))
val g : bool -> float# -> float = <fun>
|}]
