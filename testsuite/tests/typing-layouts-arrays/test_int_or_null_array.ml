(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 readonly_files =
   "gen_u_array.ml test_gen_u_array.ml gen_product_array_helpers.ml";
 modules = "${readonly_files}";
 flambda2;
 stack-allocation;
 {
   bytecode;
 }
 {
   native;
 }
*)

open Gen_product_array_helpers
open Stdlib_stable
open Stdlib_upstream_compatible

(* If copying this test for a new shape, you should only have to
   change the bit between here and the next comment. See README.md in this
   test directory. *)

(* CR or-null: separability isn't inferred without the signature, investigate. *)
module Elt : sig
  type boxed_t
  type unboxed_t : value_or_null mod non_float
  val elem : boxed_t elem
  val words_wide : int
  val zero : unit -> unboxed_t
  val to_boxed : unboxed_t -> boxed_t
  val of_boxed : boxed_t -> unboxed_t
end = struct
  type boxed_t = int option

  type unboxed_t = int or_null

  let elem : boxed_t elem = Option int_elem

  let words_wide : int = 2
  let zero () : unboxed_t = Or_null.Null

  let to_boxed a = Or_null.to_option a
  let of_boxed a = Or_null.of_option a
end

open Elt

(* Below here is copy pasted due to the absence of layout polymorphism. Don't
   change it.  See README.md in this test directory. *)
module Element_ops = (val Gen_product_array_helpers.make_element_ops elem)

module UTuple_array0 :
  Gen_u_array.S0 with type element_t = unboxed_t
                  and type ('a : any mod separable) array_t = 'a array = struct
  type element_t = unboxed_t

  type ('a : any mod separable) array_t = 'a array

  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_array_length
  external length : element_t array -> int = "%array_length"
  external get: element_t array -> int -> element_t = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: element_t array -> int -> element_t -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: element_t array -> int -> element_t = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: element_t array -> int -> element_t -> unit =
    "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external makearray_dynamic : int -> element_t -> element_t array =
    "%makearray_dynamic"

  let unsafe_create : int -> element_t array =
    (* We don't actually have an uninitialized creation function for these, yet,
       so we just use [makearray_dynamic] (which is what we want to test anyway)
       with the zero element. *)
    fun i -> makearray_dynamic i (zero ())

  external unsafe_blit :
    element_t array -> int -> element_t array -> int -> int -> unit =
    "%arrayblit"

  let empty () : unboxed_t array = [||]
  let to_boxed = to_boxed

  let compare_element x y =
    Element_ops.compare (to_boxed (x ())) (to_boxed (y ()))
end

module UTuple_array = Gen_u_array.Make (UTuple_array0)

module UTuple_array_boxed = Test_gen_u_array.Make_boxed (struct
    module M = UTuple_array
    module I = Element_ops
    module E = struct
      let to_boxed x = to_boxed (x ())
      let of_boxed x () = of_boxed x
    end
  end)
module _ = Test_gen_u_array.Test (UTuple_array_boxed)
