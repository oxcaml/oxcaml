(* TEST
 flags = "-extension layouts_alpha";
 { expect; }
 { flags += " -O3"; expect.opt; }
*)

(* Tests for arrays whose element type is not known to be representable
   (e.g. because it has layout [any] or an abstract kind). Values of such
   arrays can be passed around: [Typeopt.value_kind] only uses the element's
   array kind as a refinement, and falls back to [Pgenval] when the element
   has no representable layout. But anything that needs the element's array
   kind, such as specializing an array primitive, must reject them. See
   [Typeopt.type_representable_layout]. *)

(*******************************************************)
(* Test: externals over [any separable] type variables *)

(* CR layouts-scannable: This signature seems needlessly restrictive
   (but this probably doesn't matter too much). *)
external weird_id : ('a : any mod separable). 'a array -> int = "%identity"
let weird_id' arr = weird_id arr
[%%expect{|
external weird_id : ('a : any separable). 'a array -> int = "%identity"
val weird_id' : ('a : value_maybe_null). 'a array -> int = <fun>
|}]

(* This is not sound whenever 'a is not a value, but this is the kind of
   unsoundness that is generally permitted with [external] declarations *)
external weird_flarr_len : ('a : any mod separable). 'a array -> int
  = "%floatarray_length"
let weird_flarr_len' x = weird_flarr_len x
[%%expect{|
external weird_flarr_len : ('a : any separable). 'a array -> int
  = "%floatarray_length"
val weird_flarr_len' : ('a : value_maybe_null). 'a array -> int = <fun>
|}]

external product_edge_case : ('a : any mod separable). #('a * 'a) array -> int
  = "%identity"
let product_edge_case x = product_edge_case x
[%%expect{|
external product_edge_case : ('a : any separable). #('a * 'a) array -> int
  = "%identity"
val product_edge_case : ('a : value_maybe_null). #('a * 'a) array -> int =
  <fun>
|}]

(*******************************************************************)
(* Test: arrays of non-representable elements can be passed around *)

kind_ k
type t : k separable
type t_any : any separable
[%%expect{|
kind_ k
type t : k separable
type t_any : any separable
|}]

let f (a : t array) = a
[%%expect{|
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

let f (a : t iarray) = a
[%%expect{|
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

let f (a : t_any array) = a
[%%expect{|
Line 1, characters 26-27:
1 | let f (a : t_any array) = a
                              ^
Error: A representable layout is required here.
       The layout of t_any is any separable
         because of the definition of t_any at line 3, characters 0-26.
       But the layout of t_any must be representable
         because it's the type of an array element.
|}]

let f (type a : any separable) (a : a array) = a
[%%expect{|
Line 1, characters 47-48:
1 | let f (type a : any separable) (a : a array) = a
                                                   ^
Error: A representable layout is required here.
       The layout of a is any separable
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of an array element.
|}]

type r = { a : t array }
let h (x : r) = x.a
[%%expect{|
type r = { a : t array; }
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

module F (X : sig
  kind_ k
  type t : k separable
  val arr : unit -> t array
  val len : t array -> int
end) = struct
  let g () = ignore (X.arr ())
  let len () = X.len (X.arr ())
end
[%%expect{|
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

(* The functor body doesn't depend on the element's representation. *)
module Float = F (struct
  kind_ k = value
  type t = float
  let arr () = [| 1.0; 2.0; 3.0 |]
  let len = Array.length
end)
let n = Float.len ()
[%%expect{|
Line 1, characters 15-16:
1 | module Float = F (struct
                   ^
Error: Unbound module "F"
|}]

external[@layout_poly] length : ('a : any separable). 'a array -> int
  = "%array_length"
module Float_u = F (struct
  kind_ k = float64
  type t = float#
  let arr () = [| #1.0; #2.0 |]
  let len a = length a
end)
let n = Float_u.len ()
[%%expect{|
external length : ('a : any separable). 'a array -> int = "%array_length"
  [@@layout_poly]
Line 3, characters 17-18:
3 | module Float_u = F (struct
                     ^
Error: Unbound module "F"
|}]

(************************************************************************)
(* Test: operations that need the element's representation are rejected *)

let mk () : t array = [| assert false |]
[%%expect{|
Line 1, characters 22-40:
1 | let mk () : t array = [| assert false |]
                          ^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a array"
       but an expression was expected of type "t array"
       The kind of t is k separable
         because of the definition of t at line 2, characters 0-20.
       But the kind of t must be representable
         because it's the type of an array element.
|}]

let m (a : t array) = match a with [| _ |] -> 1 | _ -> 0
[%%expect{|
Line 1, characters 35-42:
1 | let m (a : t array) = match a with [| _ |] -> 1 | _ -> 0
                                       ^^^^^^^
Error: This pattern matches values of type "'a array"
       but a pattern was expected which matches values of type "t array"
       The kind of t is k separable
         because of the definition of t at line 2, characters 0-20.
       But the kind of t must be representable
         because it's the type of an array element.
|}]

let len (a : t array) = length a
[%%expect{|
Line 1, characters 31-32:
1 | let len (a : t array) = length a
                                   ^
Error: The value "a" has type "t array" but an expression was expected of type
         "'a array"
       The kind of t is k separable
         because of the definition of t at line 2, characters 0-20.
       But the kind of t must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(* Externals can only mention a non-representable type under [array]... *)
external get : t array -> int -> t = "%array_safe_get"
[%%expect{|
Line 1, characters 33-34:
1 | external get : t array -> int -> t = "%array_safe_get"
                                     ^
Error: Types in an external must have a representable layout.
       The kind of t is k separable
         because of the definition of t at line 2, characters 0-20.
       But the kind of t must be representable
         because it's the type of the result of an external declaration.
|}]

external make : int -> t -> t array = "%makearray_dynamic"
[%%expect{|
Line 1, characters 23-24:
1 | external make : int -> t -> t array = "%makearray_dynamic"
                           ^
Error: Types in an external must have a representable layout.
       The kind of t is k separable
         because of the definition of t at line 2, characters 0-20.
       But the kind of t must be representable
         because it's the type of an argument in an external declaration.
|}]

(* ...and the error appears when the primitive is specialized. *)
external len : t array -> int = "%array_length"
let g a = len a
[%%expect{|
external len : t array -> int = "%array_length"
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

external len : t iarray -> int = "%array_length"
let g a = len a
[%%expect{|
external len : t iarray -> int = "%array_length"
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

external len : t_any array -> int = "%array_length"
let g a = len a
[%%expect{|
external len : t_any array -> int = "%array_length"
Line 2, characters 14-15:
2 | let g a = len a
                  ^
Error: A representable layout is required here.
       The layout of t_any is any separable
         because of the definition of t_any at line 3, characters 0-26.
       But the layout of t_any must be representable
         because it's the type of an array element.
|}]

external blit : t array -> int -> t array -> int -> int -> unit = "%arrayblit"
let g a = blit a 0 a 0 0
[%%expect{|
external blit : t array -> int -> t array -> int -> int -> unit
  = "%arrayblit"
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

external size : t array -> int = "%array_element_size_in_bytes"
let g a = size a
[%%expect{|
external size : t array -> int = "%array_element_size_in_bytes"
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

external make_uninit : int -> t array = "%makearray_dynamic_uninit"
let g () = make_uninit 3
[%%expect{|
external make_uninit : int -> t array = "%makearray_dynamic_uninit"
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

external idx : int -> (t array, t) idx_mut = "%unsafe_array_idx"
let g () = idx 0
[%%expect{|
external idx : int -> (t array, t) idx_mut = "%unsafe_array_idx"
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]

(**********************************************************)
(* Test: errors report the element type as it was written *)

type t_alias = t
external len : t_alias array -> int = "%array_length"
let g a = len a
[%%expect{|
type t_alias = t
external len : t_alias array -> int = "%array_length"
>> Fatal error: Typeopt.type_representable_layout: unexpected missing layout (1)
Uncaught exception: Misc.Fatal_error

|}]
