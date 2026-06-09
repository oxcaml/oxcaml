(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   multidomain;
   flags += "-alert -unsafe_multidomain -alert -do_not_spawn_domains -w -21";
   { native; }
*)

open Effect
open Effect.Deep
open Preemption_util

type _ Effect.t += Bump : int -> int Effect.t

let () =
  with_preemption_setup ~interval:0.0001 ~repeating:true (fun () ->
    let workers = Array.init 2 (fun i ->
      Domain.spawn (fun () ->
        Gc.set { (Gc.get ()) with minor_heap_size = 1024 };
        try_with (fun () ->
          try_with (fun () ->
            let start = Unix.gettimeofday () in
            while Unix.gettimeofday () -. start < 20.  do
              preempt_self ();
              let n = perform (Bump i) in
              ignore (Sys.opaque_identity n)
            done)
          () { effc = (fun (type a) (e : a t) -> match e with
            | Bump n -> Some (fun (k : (a,_) continuation) ->
              let _ = Sys.opaque_identity (ref (n+1)) in
              continue k (n + 1))
            | _ -> None) })
        () { effc = (fun (type a) (e : a t) -> match e with
          | Preemption -> Some (fun (k : (a,_) continuation) ->
            Gc.full_major ();
            continue k ())
          | _ -> None) }
      )
    ) in
    Unix.sleepf 10.0;
    Array.iter Domain.join workers)
