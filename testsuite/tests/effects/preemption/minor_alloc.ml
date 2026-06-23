(* TEST
   runtime5;
   poll_insertion;
   flags += "-w -21";
   { native; }
*)

open Effect
open Effect.Deep

let alloc () =
  let r @ global = ref "hello" in
  Sys.opaque_identity r

(* This test tries to set up conditions such that:
   1. a preemption interrupts a minor allocation.
   2. when we resume the continuation for that preemption, we redo that minor
      allocation, but...
   3. redoing that minor allocation puts us over the young limit, which requires
      GC'ing again

   This test consistently segfaults if we don't GC after redoing preempted
   allocations.
*)

let () =
  Gc.set { (Gc.get ()) with minor_heap_size = 1024 };
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
    let bang = Atomic.make false in
    let weird = ref [] in
    let f () =
      for i = 1 to 1_000_000_000 do
        let r = alloc () in
        if Atomic.get bang then begin
          weird := r :: !weird;
          List.iter (fun r -> assert (!r = "hello")) !weird;
          Atomic.set bang false;
        end
      done
    in
    let effc (type a) : a t -> _ = function
      | Preemption -> Some (fun (k : (a,_) continuation) ->
        for _i = 1 to Random.int 1000 do ignore (alloc ()) done;
        Atomic.set bang true;
        continue k ())
      | _ -> None
    in
    Preemptible.try_with
      ~on_tick:(fun () -> Preempt)
      f () { effc })
