(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 readonly_files =
   "gen_u_iarray.ml test_gen_u_iarray.ml gen_product_iarray_helpers.ml";
 modules = "${readonly_files}";
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   jsir;
 }{
   flags = "-O3";
   native;
 }{
   flags = "-Oclassic";
   native;
 }
*)

open Gen_product_iarray_helpers
open Stdlib_stable
open Stdlib_upstream_compatible

(* This test is auto-generated from the corresponding mutable array test in
   typing-layouts-arrays/ using gen_iarray_test.sh. Do not edit directly.
   See README.md in this test directory. *)
type unboxed_t = Even of int64_u | Odd of int64_u * int64_u [@@unboxed]

let elem = int64_elem
let zero () = Even #0L

let of_boxed number =
  let half = Int64_u.of_int64 (Int64.shift_right number 1) in
  if Int64.logand number 1L = 0L then Even half
  else Odd (half, Int64_u.lognot half)

let to_boxed = function
  | Even half -> Int64.shift_left (Int64_u.to_int64 half) 1
  | Odd (half, complement) ->
    assert (Int64_u.equal complement (Int64_u.lognot half));
    Int64.logor (Int64.shift_left (Int64_u.to_int64 half) 1) 1L
(* Below here is copy pasted due to the absence of layout polymorphism. Don't
   change it.  See README.md in this test directory. *)
module Element_ops = (val Gen_product_iarray_helpers.make_element_ops elem)

module UTuple_array0 :
  Gen_u_iarray.S0 with type element_t = unboxed_t
                  and type ('a : any mod separable) array_t = 'a iarray
                  and type mutable_t = unboxed_t array = struct
  type element_t = unboxed_t

  type ('a : any mod separable) array_t = 'a iarray
  type mutable_t = unboxed_t array

  type element_arg = unit -> element_t
  type t = element_t iarray
  let max_length = Sys.max_array_length
  external length : element_t iarray -> int = "%array_length"
  external get: element_t iarray -> int -> element_t = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external unsafe_get: element_t iarray -> int -> element_t = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a

  external makearray_dynamic : int -> element_t -> element_t array =
    "%makearray_dynamic"

  let unsafe_create_mutable : int -> mutable_t =
    (* We don't actually have an uninitialized creation function for these, yet,
       so we just use [makearray_dynamic] (which is what we want to test anyway)
       with the zero element. *)
    fun i ->
      if i < 0 || i > max_length then invalid_arg "unsafe_create_mutable";
      makearray_dynamic i (zero ())

  external unsafe_set_mutable: element_t array -> int -> element_t -> unit =
    "%array_unsafe_set"

  let unsafe_set_mutable t i e = unsafe_set_mutable t i (e ())

  external unsafe_get_mutable: element_t array -> int -> element_t =
    "%array_unsafe_get"

  let unsafe_get_mutable t i = let a = unsafe_get_mutable t i in fun () -> a

  let blit_to_mutable src src_off dst dst_off len =
    for i = 0 to len - 1 do
      unsafe_set_mutable dst (dst_off + i) (unsafe_get src (src_off + i))
    done

  external freeze : element_t array -> element_t iarray = "%array_to_iarray"

  let empty () : unboxed_t iarray =
    let m = unsafe_create_mutable 0 in
    freeze m

  let to_boxed = to_boxed

  let compare_element x y =
    Element_ops.compare (to_boxed (x ())) (to_boxed (y ()))
end

module UTuple_array = Gen_u_iarray.Make (UTuple_array0)

module UTuple_array_boxed = Test_gen_u_iarray.Make_boxed (struct
    module M = UTuple_array
    module I = Element_ops
    module E = struct
      let to_boxed x = to_boxed (x ())
      let of_boxed x () = of_boxed x
    end
  end)
module _ = Test_gen_u_iarray.Test (UTuple_array_boxed)
