(* TEST
 flags = "-extension layout_poly_alpha";
 readonly_files = "mli_with_dynamic_default.mli mli_with_static_default.mli";
 setup-ocamlopt.byte-build-env;
 module = "mli_with_dynamic_default.mli";
 ocamlopt.byte;
 module = "mli_with_static_default.mli";
 ocamlopt.byte;
 (* [-Ix] promises static data is available, which keeps the [Static] staticity
    recorded in their cmis, allowing them to be rebound at [@ static] below.
    There isn't actually an implementation so stop after typing before the
    compiler actually tries to read that data. *)
 flags += " -nocwd -Ix . -stop-after typing ";
 module = "use_mli_top_modality.ml";
 ocamlopt.byte;
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
