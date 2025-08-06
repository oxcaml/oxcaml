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
    match Thread.use_domains () with
    | exception
        Failure("Thread.use_domains: first use must be from the main thread.") ->
      ()
    | _ -> failwith "should have failed") ())
