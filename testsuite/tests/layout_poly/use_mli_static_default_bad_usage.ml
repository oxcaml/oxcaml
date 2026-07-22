(* TEST
 readonly_files = "mli_with_static_default.mli use_mli_static_default_bad_usage.reference";
 setup-ocamlc.byte-build-env;
 module = "mli_with_static_default.mli";
 ocamlc.byte;
 module = "use_mli_static_default_bad_usage.ml";
 compiler_output = "use_mli_static_default_bad_usage.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "use_mli_static_default_bad_usage.reference";
 check-ocamlc.byte-output;
*)

let use_static (_ @ static) = ()

(* [Mli_with_static_default.bar] is overridden to [@@ dynamic], so using it
   as static is an error. *)
let () = use_static Mli_with_static_default.bar
