(* [used] is called from the main module. [unused_export] is exposed (there is
   no .mli) but not used anywhere in the program, so the whole-program Reaper
   should delete it, along with the string constant it mentions. The marker
   strings let the test detect their presence in compilation artifacts. *)

let[@inline never] used x = "MARKER_OF_USED_EXPORT_" ^ x

let[@inline never] unused_export x = "MARKER_OF_DEAD_EXPORT_" ^ x
