(* TEST
 flags = "-extension layout_poly -nocwd -Ix .";
 readonly_files = "dynamic_lib.mli dynamic_lib.ml mli_less_include_dynamic.ml \
                   use_mli_less_include_dynamic.reference";
 setup-ocamlc.byte-build-env;
 module = "dynamic_lib.mli";
 ocamlc.byte;
 module = "dynamic_lib.ml";
 ocamlc.byte;
 module = "mli_less_include_dynamic.ml";
 ocamlc.byte;
 module = "use_mli_less_include_dynamic.ml";
 compiler_output = "use_mli_less_include_dynamic.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "use_mli_less_include_dynamic.reference";
 check-ocamlc.byte-output;
*)

let use_static (_ @ static) = ()

(* [own] is defined in the mli-less unit itself, so it is static. *)
let () = use_static Mli_less_include_dynamic.own
let _ = Mli_less_include_dynamic.own 1

(* [f] was [include]d from a dynamic unit, so it is [@@ dynamic] and cannot be
   instantiated. Before this was tracked, its static half was silently missing
   and the compiler crashed in slambda eval when instantiating it. *)
let _ = Mli_less_include_dynamic.f 1
