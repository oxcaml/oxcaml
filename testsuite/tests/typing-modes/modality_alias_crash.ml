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
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Regression test: checking this unit against its interface used to crash
   with [Invalid_argument "submode_exn"] raised from
   [Includecore.child_modes_with_modalities]. Ingredients (all required):
   - the interface has default modality [@@ portable];
   - a module ([Outer]) whose implementation contains an alias to a module
     from another (nonportable) compilation unit ([Inner =
     Modality_alias_lib]);
   - a module alias to [Outer] ([Outer_alias]) declared in both the
     implementation and the interface, so the interface keeps the portable
     modality on the alias declaration.

   The portability claim is now checked structurally: this unit is accepted
   because [Modality_alias_lib] only contains an [int], which crosses
   portability, while [modality_alias_reject.ml] (whose library contains a
   function) is rejected with a proper error. *)

module Outer = struct
  module Inner = Modality_alias_lib
end

module Outer_alias = Outer
