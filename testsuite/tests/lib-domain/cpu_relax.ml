(* TEST
   include systhreads;
   hassysthreads;
   { bytecode; }
   { native; }
*)

let () =
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
   Thread.join thread