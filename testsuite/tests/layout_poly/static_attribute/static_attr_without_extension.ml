(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* This unit has no [.mli] and is compiled without
   [-extension layout_poly_alpha]. *)

[@@@static]
