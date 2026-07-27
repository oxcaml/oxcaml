(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* An asynchronous exception unwinding across a TLS-owning (preemptible)
   fiber must restore the enclosing thread's TLS state: the owner fiber is
   freed during the unwind and the cached state is recomputed. *)

open Effect
open Effect.Deep

let () = Sys.catch_break true

let k = Domain.TLS.new_key (fun () -> "init")

let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () ->
      finished := true;
      raise Sys.Break)
    b;
  ref (Some b)

let () =
  Domain.TLS.set k "outer";
  let finished = ref false in
  let r = allocate_bytes finished in
  (try
    Sys.with_async_exns (fun () ->
      r := None;
      Preemptible.match_with (fun () ->
          Domain.TLS.set k "inner";
          assert (Domain.TLS.get k = "inner");
          (* Allocate until the finaliser raises [Sys.Break] asynchronously,
             unwinding out of this owner fiber. *)
          while true do
            let _ @ global = Sys.opaque_identity (42, Random.int 42) in
            ()
          done)
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (_ : a Effect.t) -> None);
          tickc = (fun () -> Continue) })
  with
  | Sys.Break -> assert !finished
  | _ -> assert false);
  (* The owner fiber is gone; the thread's state must be visible again. *)
  Printf.printf "after async unwind: %s\n" (Domain.TLS.get k);
  print_endline "OK"
