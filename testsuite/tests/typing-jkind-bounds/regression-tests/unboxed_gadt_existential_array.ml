(* TEST
 expect;
*)

(* Regression test for COMPILERS-7227.

   An [@@unboxed] GADT constructor that opens an existential storage type
   whose kind is constrained by [immutable_data] used to produce a [t]
   whose kind "depends on the existential". When the layout of [_ t array]
   was requested -- via a layout-poly array primitive ([%array_length],
   [%array_unsafe_get]) or via [Translcore.layout_exp] / [Typeopt.value_kind]
   on a function parameter or higher-order argument -- the type was scraped
   to [Tof_kind], which fell into the [assert false] arm of
   [Typeopt.classify], producing a fatal ICE. *)

type ('a, 'b : immutable_data) unpacked : immutable_data with 'b =
  { node : 'b }
[@@unboxed]

type +'a t = Node : ('a, _) unpacked -> 'a t
[@@unboxed] [@@warning "-unused-constructor"]

[%%expect{|
type ('a, 'b : immutable_data) unpacked = { node : 'b; } [@@unboxed]
type +'a t = Node : 'a ('b : immutable_data). ('a, 'b) unpacked -> 'a t [@@unboxed]
|}]

(* (1) %array_length -> Translprim.specialize_primitive (Parraylength). *)
let _01_array_length (children : 'a t array) = Array.length children
[%%expect{|
Uncaught exception: File "typing/typeopt.ml", line 265, characters 6-12: Assertion failed

|}]

(* (2) %array_unsafe_get -> Translprim.specialize_primitive (Parrayrefu). *)
let _02_array_get (children : 'a t array) = children.(0)
[%%expect{|
Uncaught exception: File "typing/typeopt.ml", line 265, characters 6-12: Assertion failed

|}]

(* (3) Just naming the array as a parameter -> Translcore.transl_curried_function
       computing the parameter's layout via Typeopt.value_kind ->
       array_kind_of_elt -> classify. *)
let _03_param_layout (children : 'a t array) = ignore children
[%%expect{|
Uncaught exception: File "typing/typeopt.ml", line 265, characters 6-12: Assertion failed

|}]

(* (4) Passing the array to a higher-order function -> Translcore.layout_exp
       on the array argument, same value_kind -> array_kind_of_elt ->
       classify path as (3). *)
let _04_array_arg_layout (children : 'a t array) = Array.iter ignore children
[%%expect{|
Uncaught exception: File "typing/typeopt.ml", line 265, characters 6-12: Assertion failed

|}]
