(* TEST
 flags = "-extension layouts_alpha -dlambda -dno-unique-ids";
 expect;
*)

(* A variant whose declaration mentions an [any]-kinded parameter stores
   [Cstr_layout_variable] for the relevant constructor, since its representation
   depends on the type argument. [Typeopt.value_kind] recomputes the
   representation at each use site from the instantiated argument types, so these
   should get a precise [Pvariant] value_kind (and precise field kinds) -- just
   like the [value] baseline below -- rather than the opaque [Pgenval]. *)

type ('a : value) value_v = A of 'a | B
let f_value (x : string value_v) = x
[%%expect{|
0
type 'a value_v = A of 'a | B
(let
  (f_value =
     (function {nlocal = 0} x[value<(consts (0)) (non_consts ([0: *]))>]
       : (consts (0)) (non_consts ([0: *])) x))
  (apply (field_imm 1 (global Toploop!)) "f_value" f_value))
val f_value : string value_v -> string value_v = <fun>
|}]

type ('a : any) any_v = A of 'a | B
[%%expect{|
0
type ('a : any) any_v = A of 'a | B
|}]

(* value instantiation: same shape as [f_value] above *)
let f_string (x : string any_v) = x
[%%expect{|
(let
  (f_string =
     (function {nlocal = 0} x[value<(consts (0)) (non_consts ([0: *]))>]
       : (consts (0)) (non_consts ([0: *])) x))
  (apply (field_imm 1 (global Toploop!)) "f_string" f_string))
val f_string : string any_v -> string any_v = <fun>
|}]

(* immediate instantiation: payload kind is now [value<int>] *)
let f_int (x : int any_v) = x
[%%expect{|
(let
  (f_int =
     (function {nlocal = 0}
       x[value<(consts (0)) (non_consts ([0: value<int>]))>]
       : (consts (0)) (non_consts ([0: value<int>])) x))
  (apply (field_imm 1 (global Toploop!)) "f_int" f_int))
val f_int : int any_v -> int any_v = <fun>
|}]

(* unboxed-float instantiation: payload becomes a flat [float64] field *)
let f_float64 (x : float# any_v) = x
[%%expect{|
(let
  (f_float64 =
     (function {nlocal = 0}
       x[value<(consts (0)) (non_consts ([0: float64]))>]
       : (consts (0)) (non_consts ([0: float64])) x))
  (apply (field_imm 1 (global Toploop!)) "f_float64" f_float64))
val f_float64 : float# any_v -> float# any_v = <fun>
|}]

(* multi-argument constructor with a mix of value and unboxed arguments *)
type ('a : any, 'b : any) any2 = C of 'a * 'b | D
let f_mixed (x : (int, float#) any2) = x
[%%expect{|
0
type ('a : any, 'b : any) any2 = C of 'a * 'b | D
(let
  (f_mixed =
     (function {nlocal = 0}
       x[value<(consts (0)) (non_consts ([0: value<int>, float64]))>]
       : (consts (0)) (non_consts ([0: value<int>, float64])) x))
  (apply (field_imm 1 (global Toploop!)) "f_mixed" f_mixed))
val f_mixed : (int, float#) any2 -> (int, float#) any2 = <fun>
|}]
