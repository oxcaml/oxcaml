(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-g -gno-upstream-dwarf -save-ir-after linearization";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "touch empty.ml";
 script;
 flags = "-g -gno-upstream-dwarf start_from_emit_dwarf.cmir-linear";
 module = "empty.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The Linear IR is saved once the unit has been emitted, so it contains the
   labels that emission inserted for DWARF.  Emitting it again must not create
   labels that clash with them. *)

let[@inline never] f x = if x > 3 then x + 1 else x - 1

let[@inline never] g x = f x + 2
