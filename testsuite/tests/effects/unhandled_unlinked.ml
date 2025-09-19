(* TEST
   runtime5;
   exit_status = "2";
   { bytecode; }
   { native; }
   { javascript; }
*)

open Effect
type _ t += E : unit t
let _ = perform E
