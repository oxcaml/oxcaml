(* TEST
 readonly_files = "teq.mli cws.mli b__.ml b.mli exchange.mli";
 compile_only = "true";
 setup-ocamlc.byte-build-env;
 module = "teq.mli";
 ocamlc.byte;
 module = "cws.mli";
 ocamlc.byte;
 module = "exchange.mli";
 ocamlc.byte;
 split [
 | flags = "-no-alias-deps -w -49";
 | flags = "-no-alias-deps -w +49";
 ]
 module = "b__.ml";
 ocamlc.byte;
 flags = "";
 module = "b.mli";
 ocamlc.byte;
 split [
 | flags = "";
 | flags = "-principal";
 ]
 module = "r.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
 (* Transitive dependencies must be found through attached paths. *)
 script = "mkdir -p public";
 script;
 script = "cp b.cmi exchange.cmi teq.cmi public";
 script;
 flags += " -I public -nocwd";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Saving b.cmi may normalize B__.Wire_size.t to Cws.t. Inspecting the
   return type of B.encode for application warnings must not make it depend
   on the local GADT equation. Test both open and closed wrapper cmis.
   Exchange must be a compilation unit, rather than a functor parameter. *)

let encode_size (x : Exchange.si) =
  let Teq.Refl = Teq.same_exn Exchange.type_id B.type_id in
  B.encode x
