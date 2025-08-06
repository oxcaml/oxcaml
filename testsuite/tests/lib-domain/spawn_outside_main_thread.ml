(* TEST
   include systhreads;
   hassysthreads;
   runtime5;
   { bytecode; }
   { native; }
*)

[@@@warning "-fragile-literal-pattern"]

let () =
  Thread.join (Thread.create (fun () ->
      match
        Domain.join
          ((Domain.Safe.spawn [@alert "-do_not_spawn_domains"]) (fun () -> ()))
      with
      | exception
          Failure("Domain.spawn: first use must be from the main thread.") ->
        ()
      | _ -> failwith "should have failed") ())
