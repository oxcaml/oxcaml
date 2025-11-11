(* TEST
   include systhreads;
   hassysthreads;
   multidomain;
   runtime5;
   { bytecode; }
   { native; }
*)

[@@@warning "-fragile-literal-pattern"]

let () =
  Thread.join (Thread.create (fun () ->
      match
        Domain.join
          ((Domain.Safe.spawn [@alert "-do_not_spawn_domains"]) (fun () -> ref 42))
      with
      | n -> assert (!n = 42)) ())
