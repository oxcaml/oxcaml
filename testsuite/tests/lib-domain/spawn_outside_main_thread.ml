(* TEST
   include systhreads;
   hassysthreads;
   multidomain;
   { bytecode; }
   { native; }
*)

[@@@warning "-fragile-literal-pattern"]

let () =
  Thread.join (Thread.create (fun () ->
      match Domain.join (Domain.Safe.spawn (fun () -> ref 42)) with
      | n -> assert (!n = 42)) ())
