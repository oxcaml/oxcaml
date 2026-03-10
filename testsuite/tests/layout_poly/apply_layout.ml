(* TEST
   flags = "-extension layout_poly_alpha";
   expect;
*)

(* A functor using a layout-poly identity function at a concrete layout *)
module F (M : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end) = struct
  let use_int (x : int) = M.id x
  let use_float (x : float#) = M.id x
end
[%%expect{|
>> Fatal error: Translcore: unexpected Texp_apply_layout
with sorts [value]
Uncaught exception: Misc.Fatal_error

|}]

(* Two layout variables instantiated independently *)
module G (M : sig
  val map : layout_ x y. ('a : x) ('b : y). ('a -> 'b) -> 'a -> 'b
end) = struct
  let apply_int_to_float (f : int -> float#) (x : int) = M.map f x
end
[%%expect{|
>> Fatal error: Translcore: unexpected Texp_apply_layout
with sorts [value, float64]
Uncaught exception: Misc.Fatal_error

|}]

(* Calling the function multiple times at different layouts *)
module H (M : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end) = struct
  let use (x : int) (y : float#) =
    let x' = M.id x in
    let y' = M.id y in
    (x', y')
end
[%%expect{|
Line 7, characters 9-11:
7 |     (x', y')
             ^^
Error: This expression has type "float#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of value
         because it's the type of a tuple element.
|}]

(* Let binding: binding a layout-poly value *)
module I (M : sig
  val f : layout_ x. ('a : x). 'a -> 'a
end) = struct
  let _ = M.f
end
[%%expect{|
>> Fatal error: Translcore: unexpected Texp_apply_layout
with sorts [value]
Uncaught exception: Misc.Fatal_error

|}]

(* Layout-poly value used in a type-constrained binding *)
module J (M : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end) = struct
  let f : int -> int = M.id
end
[%%expect{|
>> Fatal error: Translcore: unexpected Texp_apply_layout
with sorts [value]
Uncaught exception: Misc.Fatal_error

|}]

(* Inst_mutvar: mutable variable of a layout-poly type *)
let poly_ f : layout_ x. ('a : x). 'a -> 'a = fun x -> x
let _ =
  let mutable g = f in
  g
[%%expect{|
Line 1, characters 14-43:
1 | let poly_ f : layout_ x. ('a : x). 'a -> 'a = fun x -> x
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Sort polymorphism is not supported in term-level type annotations
|}]

(* Inst_binding_op: layout-poly binding operator *)
let poly_ ( let+ ) : layout_ x. ('a : x). 'a -> 'a = fun x -> x
let _ = let+ x = 42 in x
[%%expect{|
Line 1, characters 21-50:
1 | let poly_ ( let+ ) : layout_ x. ('a : x). 'a -> 'a = fun x -> x
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Sort polymorphism is not supported in term-level type annotations
|}]
