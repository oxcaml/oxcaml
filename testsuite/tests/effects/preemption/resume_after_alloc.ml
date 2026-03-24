(* TEST
   modules = "preemption_util.ml";
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Preemption_util

let () =
  let preempted = ref false in
  let result = ref None in
  ignore (run_with_tick_handler
    ~on_preemption:(fun _resume ->
      preempted := true;
      let _ = Sys.opaque_identity (ref 1) in
      let _ = Sys.opaque_identity (ref 2) in
      let _ = Sys.opaque_identity (ref 3) in
      Resume)
    (fun () ->
       let a = ref 1 in
       let b = ref 2 in
       let c = ref 3 in
       let d = ref 4 in
       let e = ref 5 in
       let f = ref 6 in
       let g = ref 7 in
       let h = ref 8 in

       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         a := !a + !b; b := !b + !c; c := !c + !d; d := !d + !e;
         e := !e + !f; f := !f + !g; g := !g + !h; h := !h + 1;
         if !h > 1000 then begin
           a := 1; b := 2; c := 3; d := 4; e := 5; f := 6; g := 7; h := 8
         end
       done;

       result := Some (!a, !b, !c, !d, !e, !f, !g, !h);
       "completed"));

  match !result with
  | Some _ -> ()
  | None -> failwith "Delayed resume: FAILED - no result"
