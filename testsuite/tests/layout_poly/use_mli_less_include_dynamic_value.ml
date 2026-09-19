(* TEST
 flags = "-extension layout_poly -nocwd -Ix .";
 readonly_files = "dynamic_lib.mli dynamic_lib.ml mli_less_include_dynamic.ml \
                   use_mli_less_include_dynamic_value.reference";
 setup-ocamlc.byte-build-env;
 module = "dynamic_lib.mli";
 ocamlc.byte;
 module = "dynamic_lib.ml";
 ocamlc.byte;
 module = "mli_less_include_dynamic.ml";
 ocamlc.byte;
 module = "use_mli_less_include_dynamic_value.ml";
 compiler_output = "use_mli_less_include_dynamic_value.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "use_mli_less_include_dynamic_value.reference";
 check-ocamlc.byte-output;
*)

let use_static (_ @ static) = ()

(* [g] is bound to a value from a dynamic unit, so even though the mli-less unit
   itself is static, [g] is [@@ dynamic]. *)
let () = use_static Mli_less_include_dynamic.g
