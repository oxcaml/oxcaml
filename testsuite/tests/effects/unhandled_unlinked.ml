(* TEST
   runtime5;
   exit_status = "2";
   { bytecode; }
   { native; }
   {
    reference = "${test_source_directory}/unhandled_unlinked.js.reference";
    javascript;
   }
*)

open Effect
type _ t += E : unit t
let _ = perform E
