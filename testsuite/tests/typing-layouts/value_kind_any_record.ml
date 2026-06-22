(* TEST
 flags = "-extension layouts_alpha -dlambda";
 expect;
*)

(* Regression test for the value_kind of a boxed record whose type parameter has
   kind [any].  At typedecl time its representation is [Record_variable], but once
   the type is instantiated [Typeopt.value_kind] recomputes it (via the
   [record_representation_for_value_kind] forward reference) so the [r] bindings
   below get a precise [Pvariant] shape rather than the conservative [Pgenval]
   (mirroring [value_kind_any_variant.ml]). *)

type ('a : any) t = { x : 'a; y : int }
[%%expect{|
0
type ('a : any) t = { x : 'a; y : int; }
|}]

(* all-value instance: [r]'s value_kind is a [Pvariant] with instantiated fields *)
let f (x : int) =
  let r = { x; y = 1 } in
  r.x + r.y
[%%expect{|
(let
  (f/294 =
     (function {nlocal = 0} x/296[value<int>] : int
       (region
         (let
           (r/297 =[value<
                     (consts ()) (non_consts ([0: value<int>, value<int>]))>]
              (makelocalblock 0 (value<int>,value<int>) x/296 1))
           (%int_add (field_int 0 r/297) (field_int 1 r/297))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/294))
val f : int -> int = <fun>
|}]

external box_float : float# -> float = "%box_float"
[%%expect{|
0
external box_float : float# -> float = "%box_float"
|}]

(* [float#] field instance: [r]'s value_kind is a mixed-block [Pvariant] *)
let g (x : float#) =
  let r = { x; y = 1 } in
  r.y
[%%expect{|
(let
  (g/306 =
     (function {nlocal = 0} x/308[float] : int
       (region
         (let
           (r/309 =[value<
                     (consts ()) (non_consts ([0: float64, value<int>]))>]
              (makelocalblock 0 (float64,value_or_null<int>) x/308 1))
           (mixedfield 1  (float64,value<int>) r/309)))))
  (apply (field_imm 1 (global Toploop!)) "g" g/306))
val g : float# -> int = <fun>
|}]
