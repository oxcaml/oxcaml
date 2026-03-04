(* TEST
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

external preempt_self : unit -> unit = "caml_domain_preempt_self" [@@noalloc]

type _ Effect.t += Bump : int -> int Effect.t

exception Timeout

let () =
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.0001; it_value = 0.0001 } in
  let _ = Sys.set_signal Sys.sigalrm
    (Signal_handle (fun _ -> preempt_self ())) in
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
  Array.iter Domain.join workers;
  let _ = Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } in
  Printf.printf "PASSED\n%!"
