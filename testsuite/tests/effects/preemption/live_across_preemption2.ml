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

  ignore (run_with_tick_handler
    ~on_preemption:(fun _resume -> preempted := true; Resume)
    (fun () ->
       let r0 = ref 100 in let r1 = ref 101 in let r2 = ref 102 in let r3 = ref 103 in
       let r4 = ref 104 in let r5 = ref 105 in let r6 = ref 106 in let r7 = ref 107 in
       let r8 = ref 108 in let r9 = ref 109 in let r10 = ref 110 in let r11 = ref 111 in
       let r12 = ref 112 in let r13 = ref 113 in let r14 = ref 114 in let r15 = ref 115 in

       let start_at = Sys.time () in
       while not !preempted do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!";
         r0 := !r0 + 1; r1 := !r1 + 1; r2 := !r2 + 1; r3 := !r3 + 1;
         r4 := !r4 + 1; r5 := !r5 + 1; r6 := !r6 + 1; r7 := !r7 + 1;
         r8 := !r8 + 1; r9 := !r9 + 1; r10 := !r10 + 1; r11 := !r11 + 1;
         r12 := !r12 + 1; r13 := !r13 + 1; r14 := !r14 + 1; r15 := !r15 + 1
       done;

       let expected = [100; 101; 102; 103; 104; 105; 106; 107;
                       108; 109; 110; 111; 112; 113; 114; 115] in
       let actual = [!r0; !r1; !r2; !r3; !r4; !r5; !r6; !r7;
                     !r8; !r9; !r10; !r11; !r12; !r13; !r14; !r15] in
       List.iter2 (fun e a ->
         if a < e then failwith "Register value lost"
       ) expected actual;
       "ok"));

