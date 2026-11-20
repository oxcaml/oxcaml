(* TEST
 modules = "unregister_stubs.c mixed_lib.ml";
 flags = "-g -function-sections -frametable-sections";
 native-compiler;
 frametable_sections;
 unset OCAMLPARAM;
 native;
*)

(* Unregister and reregister, one at a time, the frametables of this unit and
   [Mixed_lib]. The stub checks that each table takes its own unit's pieces out
   of the runtime's table and leaves the other units'. Afterwards a major GC
   scans a deep stack. *)

external unregister_and_reregister : unit -> int = "frametable_unregister_test"

let () =
  Printf.printf "failed check: %d\n" (unregister_and_reregister ());
  Printf.printf "deep %d\n" (Mixed_lib.deep_alloc 500 [])
