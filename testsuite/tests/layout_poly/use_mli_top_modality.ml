(* TEST
 readonly_files = "mli_with_dynamic_default.mli mli_with_static_default.mli";
 setup-ocamlc.byte-build-env;
 module = "mli_with_dynamic_default.mli";
 ocamlc.byte;
 module = "mli_with_static_default.mli";
 ocamlc.byte;
 module = "use_mli_top_modality.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
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
