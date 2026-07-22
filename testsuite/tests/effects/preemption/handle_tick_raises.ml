(* TEST
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   exit_status = "2";
   { native; }
*)

open Effect
open Effect.Deep

(* Test that raising from on_tick correctly causes an async exception *)

let () =
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
    Preemptible.try_with
      ~on_tick:(fun () -> failwith "Raise from on_tick")
      (fun () ->
         let start_at = Sys.time () in
         while true do
           if Sys.time () -. start_at > 5.
           then failwith "Didn't get preempted after 5s!"
         done)
      ()
      { effc = (fun (type a) (eff : a Effect.t) ->
          failwith "Should not get effect")
      })
;;
