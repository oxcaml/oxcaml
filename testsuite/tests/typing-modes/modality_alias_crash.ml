(* TEST
 readonly_files = "modality_alias_lib.mli modality_alias_lib.ml \
   modality_alias_crash.mli";
 flags += "-extension mode_alpha";
 ocamlrunparam = "b=0";
 setup-ocamlc.byte-build-env;
 module = "modality_alias_lib.mli";
 ocamlc.byte;
 module = "modality_alias_lib.ml";
 ocamlc.byte;
 module = "modality_alias_crash.mli";
 ocamlc.byte;
 module = "modality_alias_crash.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Known compiler crash: [Invalid_argument "submode_exn"] raised from
   [Includecore.child_modes_with_modalities] while checking this unit against
   its interface. Ingredients (all required):
   - the interface has default modality [@@ portable];
   - a module ([Outer]) whose implementation contains an alias to a module
     from another (nonportable) compilation unit ([Inner =
     Modality_alias_lib]; a local module, or a unit whose interface is
     portable, does not trigger it);
   - a module alias to [Outer] ([Outer_alias]) declared in both the
     implementation and the interface. *)

module Outer = struct
  module Inner = Modality_alias_lib
end

module Outer_alias = Outer
