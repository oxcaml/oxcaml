(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects -w -21";
   include unix;
   hasunix;
   poll_insertion;
   { native; }
*)

(* Stress TLS array growth interleaved with preemptions: the owner's array
   is repeatedly reallocated (from inside the preemptible fiber itself and
   from nested non-preemptible fibers sharing its state) while preemptions
   capture and resume the fiber at poll points. Reads after each growth and
   preemption must see every previously written value. *)

open Effect
open Effect.Deep

let num_keys = 300
let keys = Array.init num_keys (fun _ -> Domain.TLS.new_key (fun () -> -1))

let () =
  let preemptions = ref 0 in
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
      Preemptible.match_with (fun () ->
          for i = 0 to num_keys - 1 do
            (* Even indices are written from a nested non-preemptible fiber,
               which shares (and grows) this fiber's state. *)
            if i mod 2 = 0 then
              match_with (fun () -> Domain.TLS.set keys.(i) (i * i)) ()
                { retc = Fun.id; exnc = raise;
                  effc = (fun (type a) (_ : a Effect.t) -> None) }
            else Domain.TLS.set keys.(i) (i * i);
            for j = 0 to i do
              assert (Domain.TLS.get keys.(j) = j * j)
            done
          done;
          (* Make sure at least one preemption actually interleaved. *)
          let start_at = Sys.time () in
          while !preemptions = 0 do
            if Sys.time () -. start_at > 5. then failwith "Timed out after 5s"
          done;
          for j = 0 to num_keys - 1 do
            assert (Domain.TLS.get keys.(j) = j * j)
          done)
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (e : a Effect.t) ->
            match e with
            | Preemption -> Some (fun (k : (a, _) continuation) ->
                incr preemptions;
                continue k ())
            | _ -> None);
          tickc = (fun () -> Preempt) });
  (* The thread's own keys were never set. *)
  assert (Domain.TLS.get keys.(0) = -1);
  print_endline "OK"
