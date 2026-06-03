(* TEST
   include systhreads;
   hassysthreads;
   multidomain;
   { bytecode; }
   { native; }
*)

let () = Thread.use_domains ()

let () =
  Thread.join (Thread.create (fun () ->
      Domain.join (Domain.Safe.spawn (fun () -> ()))) ())
