(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   multicore;
   flags += "-w -21";
   { native; }
*)

open Effect
open Effect.Deep

type _ Effect.t += Bump : int -> int Effect.t

let () =
  let workers = Array.init 2 (fun i ->
    Domain.spawn (fun () ->
      Gc.set { (Gc.get ()) with minor_heap_size = 1024 };
      Domain.Tick.with_ ~interval_usec:100 (fun _ ->
        Preemptible.try_with
          ~on_tick:(fun () -> Preempt)
          (fun () ->
            try_with (fun () ->
              let start = Unix.gettimeofday () in
              while Unix.gettimeofday () -. start < 20. do
                let n = perform (Bump i) in
                ignore (Sys.opaque_identity n)
              done)
            () { effc = (fun (type a) (e : a t) -> match e with
              | Bump n -> Some (fun (k : (a,_) continuation) ->
                let _ = Sys.opaque_identity (ref (n+1)) in
                continue k (n + 1))
              | _ -> None) })
          ()
          { effc = (fun (type a) (e : a t) -> match e with
            | Preemption -> Some (fun (k : (a,_) continuation) ->
              Gc.full_major ();
              continue k ())
            | _ -> None) }))) in
  Unix.sleepf 10.0;
  Array.iter Domain.join workers
