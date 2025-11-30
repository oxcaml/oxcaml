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

module Float32_u_array0 :
  Gen_u_iarray.S0 with type element_t = float32#
                   and type ('a : any) array_t = 'a iarray 
                   and type mutable_t = float32# array = struct
  type element_t = float32#

  type ('a : any) array_t = 'a iarray
  type mutable_t = float32# array

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
      makearray_dynamic i (Float32_u.of_float32 Float32.zero)

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

  let empty () : float32# iarray = 
    let m = unsafe_create_mutable 0 in
    freeze m

  let compare_element x y =
    Float32.compare (Float32_u.to_float32 (x ())) (Float32_u.to_float32 (y ()))
end

module Float32_u_array = Gen_u_iarray.Make (Float32_u_array0)

module Float32_u_array_boxed = Test_gen_u_iarray.Make_boxed (struct
    module M = Float32_u_array
    module I = struct
      include Float32
      let max_val = max_float
      let min_val = min_float
      let rand x = of_float (Random.float (to_float x))
      let print f = Printf.printf "%f" (to_float f)
    end
    module E = struct
      let to_boxed x = Float32_u.to_float32 (x ())
      let of_boxed x () = Float32_u.of_float32 x
    end
  end)
module _ = Test_gen_u_iarray.Test (Float32_u_array_boxed)
