(* TEST
   include systhreads;
   hassysthreads;
   multicore;
   { bytecode; }
   { native; }
*)

let () =
  (* create tick thread on initial domain *)
  Thread.create (fun () -> ()) () |> Thread.join;
  (* rely on ticks in another domain *)
  Domain.spawn (fun () ->
   let flag = Atomic.make false in
   let thread = Thread.create (fun () ->
      Atomic.set flag true;
      while Atomic.get flag do
         Domain.cpu_relax ()
      done) ()
   in
   while not (Atomic.get flag) do
     Domain.cpu_relax ();
   done;
   Atomic.set flag false;
   Thread.join thread)
  |> Domain.join
