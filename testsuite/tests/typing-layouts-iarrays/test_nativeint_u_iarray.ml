(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 readonly_files = "gen_u_iarray.ml test_gen_u_iarray.ml";
 modules = "${readonly_files}";
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   flags = "-O3";
   native;
 }{
   flags = "-Oclassic";
   native;
 }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

module Nativeint_u_array0 :
  Gen_u_iarray.S0 with type element_t = nativeint#
                   and type ('a : any) array_t = 'a iarray 
                   and type mutable_t = nativeint# array = struct
  type element_t = nativeint#

  type ('a : any) array_t = 'a iarray
  type mutable_t = nativeint# array

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
    fun i ->
      if i < 0 || i > max_length then invalid_arg "unsafe_create_mutable";
      makearray_dynamic i #0n

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

  let empty () : nativeint# iarray = 
    let m = unsafe_create_mutable 0 in
    freeze m

  let compare_element x y =
    Nativeint.compare (Nativeint_u.to_nativeint (x ())) (Nativeint_u.to_nativeint (y ()))
end

module Nativeint_u_array = Gen_u_iarray.Make (Nativeint_u_array0)

module Nativeint_u_array_boxed = Test_gen_u_iarray.Make_boxed (struct
    module M = Nativeint_u_array
    module I = struct
      include Nativeint
      let max_val = max_int
      let min_val = min_int
      let rand = Random.nativeint
      let print f = Printf.printf "%nd" f
    end
    module E = struct
      let to_boxed x = Nativeint_u.to_nativeint (x ())
      let of_boxed x () = Nativeint_u.of_nativeint x
    end
  end)
module _ = Test_gen_u_iarray.Test (Nativeint_u_array_boxed)
