(* Compiled with -fno-asan by asan_fno_asan.ml. *)

type buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let[@inline never] write_past_end (buffer : buffer) =
  Bigarray.Array1.unsafe_set buffer (Bigarray.Array1.dim buffer) 'x'
