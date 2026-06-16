(* TEST
   runtime5;
   poll_insertion;
   flags += "-w -21";
   { native; }
*)

open Effect
open Effect.Deep

let () =
  let n = 5 in
  let conts : (unit, unit) continuation list ref = ref [] in
  let values : int ref list ref = ref [] in
  let rec collect remaining =
    if remaining = 0 then begin
      Gc.full_major ();
      Gc.compact ();
      let _ = Array.init 10000 (fun i -> ref i) in
      Gc.full_major ();
      Gc.compact ();
      List.iter (fun k -> continue k ()) (List.rev !conts)
    end else begin
      let done_ = ref false in
      let v = ref 0 in
      values := v :: !values;
      Preemptible.match_with
        (fun () ->
          let start_at = Sys.time () in
          while not !done_ do
            if Sys.time () -. start_at > 5. then failwith "timeout";
            incr v
          done;
          assert (!v > 0))
        ()
        { retc = (fun () -> ());
          exnc = raise;
          tickc = (fun () -> Preempt);
          effc = fun (type a) (e : a t) ->
            match e with
            | Preemption -> Some (fun (k : (a, _) continuation) ->
              conts := k :: !conts;
              done_ := true;
              collect (remaining - 1))
            | _ -> None }
    end
  in
  Domain.Tick.with_ ~interval_usec:10_000 (fun _ ->
    collect n);
  List.iter (fun v -> assert (!v > 0)) !values;
