(* TEST
   runtime5;
   exit_status = "2";
   { bytecode; }
   { native; }
   { javascript;
    reference = "unhandled_unlinked.javascript.reference";
   }
*)

open Effect
type _ t += E : unit t
let _ = perform E
