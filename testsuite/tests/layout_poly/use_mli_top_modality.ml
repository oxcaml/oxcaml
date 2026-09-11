(* TEST
 flags = "-extension layout_poly_alpha";
 readonly_files = "mli_with_dynamic_default.mli mli_with_static_default.mli use_mli_top_modality.reference";
 setup-ocamlopt.byte-build-env;
 module = "mli_with_dynamic_default.mli";
 ocamlopt.byte;
 module = "mli_with_static_default.mli";
 ocamlopt.byte;
 (* Load the dependencies via -Ix so that a cmx is guaranteed to be available
    and they keep the [Static] staticity recorded in their cmis, allowing them
    to be rebound at [@ static] below. *)
 flags += " -nocwd -Ix . ";
 module = "use_mli_top_modality.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 compiler_reference = "use_mli_top_modality.reference";
 check-ocamlopt.byte-output;
*)

let use_static (_ @ static) = ()

(* Both files start with a top-level [@@ ...] modality, so the whole module
   becomes static and can be rebound at [@ static]. *)
module (Md @ static) = Mli_with_dynamic_default
module (Ms @ static) = Mli_with_static_default

(* In [Mli_with_dynamic_default], [bar] is overridden to [@@ static], so it is
   accessed as static. *)
let () = use_static Mli_with_dynamic_default.bar

(* In [Mli_with_static_default], [foo] defaults to [@@ static], so it is
   accessed as static. *)
let () = use_static Mli_with_static_default.foo
