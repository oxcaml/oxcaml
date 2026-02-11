(* TEST
<<<<<<< oxcaml
 flags = "-extension labeled_tuples";
||||||| upstream-base
=======
>>>>>>> upstream-incoming
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
let foo (just_x : (x : int)) = just_x
