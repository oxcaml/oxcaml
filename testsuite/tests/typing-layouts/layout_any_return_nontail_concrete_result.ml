(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

let[@inline never] nontail_concrete (f : unit -> int) () =
  f () [@nontail]

let use_int () = nontail_concrete (fun () -> 42) ()

let () = assert (use_int () = 42)
