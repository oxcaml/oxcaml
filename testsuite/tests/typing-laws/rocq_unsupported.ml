(* TEST
 flags = "-extension laws -laws-rocq rocq_unsupported.v";
 setup-ocamlc.byte-build-env;
 module = "rocq_unsupported.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A law that refers to a definition outside the supported fragment cannot
   be translated to Rocq. *)

let scale x = x *. 2.
let double n = int_of_float (scale (float_of_int n))

law? double_add (n : int) : double n = n + n
