(* TEST *)

(* A store to a bigarray of complex numbers is two stores. Its index must be
   evaluated once: here it is read from memory that the first store changes. *)

open Bigarray

type complexes = (Complex.t, complex64_elt, c_layout) Array1.t

type ints = (int64, int64_elt, c_layout) Array1.t

let[@inline never] set (c : complexes) (idx : ints) (v : Complex.t) =
  Array1.unsafe_set c (Int64.to_int (Array1.unsafe_get idx 0)) v

let () =
  let c : complexes = Array1.create complex64 c_layout 4 in
  Array1.fill c Complex.zero;
  (* A second view of the same memory with another element kind, as two
     [Unix.map_file] mappings of one file would give. *)
  let idx : ints = Obj.magic c in
  set c idx { Complex.re = Int64.float_of_bits 1L; im = 42. };
  Printf.printf "c.{0}.im = %g, c.{1}.im = %g\n" c.{0}.im c.{1}.im
