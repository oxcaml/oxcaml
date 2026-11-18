(* TEST
 modules = "unregister_stubs.c mixed_lib.ml";
 flags = "-g -function-sections -frametable-sections";
 native-compiler;
 frametable_sections;
 unset OCAMLPARAM;
 native;
*)

(* Unregister and reregister the frametables of this unit and [Mixed_lib],
   which share one pieces section. The stub checks that the pieces stay in the
   runtime's table for as long as any table that refers to them is registered,
   and leave with the last one. Afterwards a major GC scans a deep stack. *)

external unregister_and_reregister : unit -> int = "frametable_unregister_test"

let () =
  Printf.printf "failed check: %d\n" (unregister_and_reregister ());
  Printf.printf "deep %d\n" (Mixed_lib.deep_alloc 500 [])
