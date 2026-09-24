(* TEST
 readonly_files = "modality_alias_lib.mli modality_alias_lib.ml \
   modality_alias_crash.mli modality_alias_np_lib.mli \
   modality_alias_np_lib.ml modality_alias_reject.mli \
   modality_alias_reject.ml";
 flags += "-extension mode_alpha";
 setup-ocamlc.byte-build-env;
 module = "modality_alias_lib.mli";
 ocamlc.byte;
 module = "modality_alias_lib.ml";
 ocamlc.byte;
 module = "modality_alias_crash.mli";
 ocamlc.byte;
 module = "modality_alias_crash.ml";
 ocamlc.byte;
 module = "modality_alias_np_lib.mli";
 ocamlc.byte;
 module = "modality_alias_np_lib.ml";
 ocamlc.byte;
 module = "modality_alias_reject.mli";
 ocamlc.byte;
 module = "modality_alias_reject.ml";
 ocamlc.byte;
*)

(* Regression test: checking this unit against its interface used to crash
   with [Invalid_argument "submode_exn"] raised from
   [Includecore.child_modes_with_modalities]. Ingredients (all required):
   - the interface has default modality [@@ portable];
   - a module ([Outer]) whose implementation contains an alias to a module
     from another (nonportable) compilation unit ([Inner =
     Modality_alias_lib]);
   - a module alias to [Outer] ([Outer_alias]) declared in both the
     implementation and the interface.

   A module alias is not a real member of the enclosing structure and carries
   no modality: the default [@@ portable] doesn't apply to [Outer_alias], so
   both this unit and [modality_alias_reject.ml] (whose library contains a
   function) are accepted. *)

module Outer = struct
  module Inner = Modality_alias_lib
end

module Outer_alias = Outer
