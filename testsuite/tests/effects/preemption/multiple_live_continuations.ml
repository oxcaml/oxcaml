(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep
open Preemption_util

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
      match_with
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
          effc = fun (type a) (e : a t) ->
            match e with
            | Preemption -> Some (fun (k : (a, _) continuation) ->
              conts := k :: !conts;
              done_ := true;
              collect (remaining - 1))
            | _ -> None }
    end
  in
  with_preemption_setup ~interval:0.01 ~repeating:true (fun () ->
    collect n);
  List.iter (fun v -> assert (!v > 0)) !values;
