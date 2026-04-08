(* TEST
   flags = "-extension layout_poly_alpha";
   expect;
*)

module type S = sig
  val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> unit
end
[%%expect{|
module type S =
  sig val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> unit end
|}]

(* layout instantiation requires static *)
module F (M : S) = struct
  let h = M.f
end
[%%expect{|
Line 2, characters 10-13:
2 |   let h = M.f
              ^^^
Error: The value "M.f" is "dynamic"
       but is expected to be "static"
         because it is layout-polymorphic and being instantiated here.
|}]

(* layout-poly values after instantiation are still [static]. The same cannot be
said for general static evaluation. *)
module F (M : S @ static) : sig
  val h : 'a -> 'b -> unit
end @ static = struct
  let h = M.f
end
[%%expect{|
>> Fatal error: Translcore: translation of layout-polymorphic instantiation is not yet supported
(layout args: [value, value])
Uncaught exception: Misc.Fatal_error

|}]

module F (M : S @ static) : sig
  val g : int
end = struct
  let g = M.f
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let g = M.f
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : 'a -> 'b -> unit end
       is not included in
         sig val g : int end
       Values do not match:
         val g : 'a -> 'b -> unit
       is not included in
         val g : int
       The type "'a -> 'b -> unit" is not compatible with the type "int"
|}]

module F (M : S @ static) : sig
  val g : int
end = struct
  let poly_ g = M.f
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ g = M.f
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> unit end
       is not included in
         sig val g : int end
       Values do not match:
         val g : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> unit
       is not included in
         val g : int
       The type "'a -> 'b -> unit" is not compatible with the type "int"
|}]

module F (M : S @ static) = struct
  let g (x : int) (y : float#) =
    M.f x y;
    M.f y x
end
[%%expect{|
>> Fatal error: Translcore: translation of layout-polymorphic instantiation is not yet supported
(layout args: [float64, value])
Uncaught exception: Misc.Fatal_error

|}]

(* Two layout variables instantiated independently *)
module G (M : sig
  val map : layout_ x y. ('a : x) ('b : y). ('a -> 'b) -> 'a -> 'b
end @ static) = struct
  let apply_int_to_float (f : int -> float#) (x : int) = M.map f x
end
[%%expect{|
>> Fatal error: Translcore: translation of layout-polymorphic instantiation is not yet supported
(layout args: [value, float64])
Uncaught exception: Misc.Fatal_error

|}]

(* partial instantiation; the uninstantiated sort stays a variable for further unification *)
module F (M :S @ static) = struct
  let g = M.f 42
  let h (x : float#)= g x
end
[%%expect{|
>> Fatal error: Translcore: translation of layout-polymorphic instantiation is not yet supported
(layout args: [value, float64])
Uncaught exception: Misc.Fatal_error

|}]

(* partial instantiation; the uninstantiated sort defaults to [value] *)
module F (M : sig
  val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> unit
end @ static) : sig
  val g : layout_ y. ('b : y). 'b -> unit
end = struct
  let g = M.f 42
end
[%%expect{|
Lines 5-7, characters 6-3:
5 | ......struct
6 |   let g = M.f 42
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : '_weak1 -> unit end
       is not included in
         sig val g : layout_ l. ('b : l). 'b -> unit end
       Values do not match:
         val g : '_weak1 -> unit
       is not included in
         val g : layout_ l. ('b : l). 'b -> unit
       The type "'_weak1 -> unit" is not compatible with the type "'a -> unit"
       Type "'_weak1" is not compatible with type "'a"
|}]

(* Re-generalization *)
module F (M : sig
  val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> unit
end @ static) : sig
  val g : layout_ y. ('b : y). 'b -> unit
end = struct
  let poly_ g x = M.f 42 x
end
[%%expect{|
>> Fatal error: Translcore: translation of layout-polymorphic instantiation is not yet supported
(layout args: [value, <genvar>])
Uncaught exception: Misc.Fatal_error

|}]

(* don't work without eta-expansion *)
module F (M : sig
  val f : layout_ x y. ('a : x) ('b : y). 'a -> 'b -> unit
end @ static) : sig
  val g : layout_ y. ('b : y). 'b -> unit
end = struct
  let poly_ g = M.f 42
end
[%%expect{|
Line 6, characters 2-22:
6 |   let poly_ g = M.f 42
      ^^^^^^^^^^^^^^^^^^^^
Error: The right-hand side of a "let poly_" binding must be a syntactic value.
|}]


(* Calling the function multiple times at different layouts *)
module H (M : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end @ static) = struct
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
       But the layout of float# must be a value layout
         because it's the type of a tuple element.
|}]

(* Let binding: binding a layout-poly value *)
module I (M : sig
  val f : layout_ x. ('a : x). 'a -> 'a
end @ static) = struct
  let _ = M.f
end
[%%expect{|
>> Fatal error: Translcore: translation of layout-polymorphic instantiation is not yet supported
(layout args: [value])
Uncaught exception: Misc.Fatal_error

|}]

(* Layout-poly value used in a type-constrained binding *)
module J (M : sig
  val id : layout_ x. ('a : x). 'a -> 'a
end @ static) = struct
  let f : int -> int = M.id
end
[%%expect{|
>> Fatal error: Translcore: translation of layout-polymorphic instantiation is not yet supported
(layout args: [value])
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
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* Inst_binding_op: layout-poly binding operator *)
let poly_ ( let+ ) : layout_ x. ('a : x). 'a -> 'a = fun x -> x
let _ = let+ x = 42 in x
[%%expect{|
Line 1, characters 21-50:
1 | let poly_ ( let+ ) : layout_ x. ('a : x). 'a -> 'a = fun x -> x
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* Binding_op from a module signature raises the unsupported error *)
module F (M : sig
  val ( let+ ) : layout_ x. ('a : x). 'a -> ('a -> 'b) -> 'b
end @ static) = struct
  open M
  let g = let+ x = 42 in x
end
[%%expect{|
Line 5, characters 10-14:
5 |   let g = let+ x = 42 in x
              ^^^^
Error: Instantiation of layout-polymorphic values is not yet supported for binding operators.
|}]
