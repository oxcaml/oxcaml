(* TEST
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep

type _ Effect.t +=
  | Suspend : unit Effect.t

let busy_wait_for flag =
  let start_at = Sys.time () in
  while not !flag do
    if Sys.time () -. start_at > 5.
    then failwith "Timed out after 5s!"
  done

(***********)

let perform_normal_effect () =
  print_endline "# Perform normal effect";
  let open struct
    type _ Effect.t +=
      | Go : unit Effect.t
  end
  in
  let happened = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> failwith "Should not tick")
    (fun () -> perform Go)
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Go -> Some (fun (k : (a, _) continuation) ->
          happened := true;
          continue k ())
        | _ -> None)
    };
  assert !happened;
  print_endline "OK"
;;

let preempt_on_tick () =
  print_endline "# Preempt on tick";
  let preempted = ref false in
  Preemptible.match_with
    (fun () ->
       let start_at = Sys.time () in
       while not (!preempted) do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!"
       done)
    ()
    { retc = Fun.id
    ; exnc = raise
    ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          continue k ())
        | _ -> None)
    ; tickc = (fun () -> Preempt)
    };
  assert !preempted;
  print_endline "OK"
;;

let preempt_after_two_ticks () =
  print_endline "# Preempt after two ticks";
  let preempted = ref false in
  let ticks_received = ref 0 in
  Preemptible.match_with
    (fun () ->
       let start_at = Sys.time () in
       while not (!preempted) do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!"
       done)
    ()
    { retc = Fun.id
    ; exnc = raise
    ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          continue k ())
        | _ -> None)
    ; tickc = (fun () ->
        incr ticks_received;
        if !ticks_received = 2
        then Preempt
        else Continue)
    };
  assert !preempted;
  print_endline "OK"
;;


let two_preemptible_inner_handles_tick () =
  print_endline "# Two preemptible fibers, inner handles tick";
  let outer_ticked = ref false in
  let inner_preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () ->
      outer_ticked := true;
      Continue)
    (fun () ->
       (* Non-preemptible fiber between the two preemptible ones, so the tick
          walk must traverse a non-preemptible stack to reach the inner one. *)
       Effect.Deep.try_with (fun () ->
         Preemptible.try_with
           ~on_tick:(fun () -> Preempt)
           (fun () -> busy_wait_for inner_preempted)
           ()
           { effc = (fun (type a) (eff : a Effect.t) ->
               match eff with
               | Preemption ->
                 Some (fun (k : (a, _) continuation) ->
                   inner_preempted := true;
                   continue k ())
               | _ -> None)
           }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       })
    ()
    { effc = (fun (type a) (_eff : a Effect.t) -> None)
    };
  assert !outer_ticked;
  assert !inner_preempted;
  print_endline "OK"
;;

let two_preemptible_outer_handles_tick () =
  print_endline "# Two preemptible fibers, outer handles tick";
  let inner_ticked = ref false in
  let outer_preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Preempt)
    (fun () ->
       (* Non-preemptible fiber between preemptible fibers *)
       Effect.Deep.try_with (fun () ->
         Preemptible.try_with
           ~on_tick:(fun () ->
             inner_ticked := true;
             Preempt)
           (fun () -> busy_wait_for outer_preempted)
           ()
           { effc =
               (fun (type a) (_eff : a Effect.t) -> None)
           }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       })
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            outer_preempted := true;
            continue k ())
        | _ -> None)
    };
  assert (not !inner_ticked);
  assert !outer_preempted;
  print_endline "OK"
;;

let resume_preempted_within_preemptible () =
  print_endline "# Resume preempted within preemptible";
  let saved_k
    : (unit, unit) continuation option ref = ref None
  in
  let preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Preempt)
    (fun () -> busy_wait_for preempted)
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            preempted := true;
            saved_k := Some k)
        | _ -> None)
    };
  assert !preempted;
  let k =
    match !saved_k with
    | Some k -> k
    | None -> failwith "No continuation saved"
  in
  let resumed_ok = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Continue)
    (fun () ->
       continue k ();
       resumed_ok := true)
    ()
    { effc =
        (fun (type a) (_eff : a Effect.t) -> None)
    };
  assert !resumed_ok;
  print_endline "OK"
;;

let resume_cont_with_nonpreemptible () =
  print_endline
    "# Resume cont with non-preemptible within preemptible";
  let saved_k
    : (unit, unit) continuation option ref = ref None
  in
  (* Create a continuation containing a non-preemptible fiber.
     The Suspend effect bubbles through the try_with to the outer
     preemptible handler, so k includes the try_with fiber. *)
  Preemptible.try_with
    ~on_tick:(fun () -> Continue)
    (fun () ->
       Effect.Deep.try_with
         (fun () -> Effect.perform Suspend)
         ()
         { effc =
             (fun (type a) (_eff : a Effect.t) -> None)
         })
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Suspend ->
          Some (fun (k : (a, _) continuation) ->
            saved_k := Some k)
        | _ -> None)
    };
  let k =
    match !saved_k with
    | Some k -> k
    | None -> failwith "No continuation saved"
  in
  (* Resume k inside a preemptible fiber, then tick.
     The resumed non-preemptible fiber sits between the outer
     preemptible fiber and the computation, so the tick must
     reach through it. *)
  let preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Preempt)
    (fun () ->
       continue k ();
       busy_wait_for preempted)
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            preempted := true;
            continue k ())
        | _ -> None)
    };
  assert !preempted;
  print_endline "OK"
;;

let resume_preempted_in_fiber_then_tick () =
  print_endline "# Resume preempted in fiber, then tick";
  let saved_k
    : (unit, unit) continuation option ref = ref None
  in
  let preempted_1 = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Preempt)
    (fun () -> busy_wait_for preempted_1)
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            preempted_1 := true;
            saved_k := Some k)
        | _ -> None)
    };
  assert !preempted_1;
  let k =
    match !saved_k with
    | Some k -> k
    | None -> failwith "No continuation saved"
  in
  let outer_ticked = ref false in
  let preempted_2 = ref false in
  Preemptible.try_with
    ~on_tick:(fun () ->
      outer_ticked := true;
      Preempt)
    (fun () ->
       Effect.Deep.try_with
         (fun () ->
            continue k ();
            busy_wait_for preempted_2)
         ()
         { effc =
             (fun (type a) (_eff : a Effect.t) -> None)
         })
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            preempted_2 := true;
            continue k ())
        | _ -> None)
    };
  assert !outer_ticked;
  assert !preempted_2;
  print_endline "OK"
;;

let resume_preempted_with_inner_preemptible () =
  print_endline
    "# Resume preempted with inner preemptible, inner ticks";
  let saved_k
    : (unit, unit) continuation option ref = ref None
  in
  let phase = ref 1 in
  let preempted_phase_1 = ref false in
  let preempted_by_c = ref false in
  (* A: outermost preemptible, catches Preemption in phase 1 *)
  Preemptible.try_with
    ~on_tick:(fun () -> Continue)
    (fun () ->
       (* Non-preemptible fiber between A and B *)
       Effect.Deep.try_with (fun () ->
         (* B: preemptible, preempts in phase 1, continues in phase 2 *)
         Preemptible.try_with
           ~on_tick:(fun () ->
             if !phase = 1 then Preempt else Continue)
           (fun () ->
              (* Non-preemptible fiber between B and C *)
              Effect.Deep.try_with (fun () ->
                (* C: preemptible, preempts always *)
                Preemptible.try_with
                  ~on_tick:(fun () -> Preempt)
                  (fun () ->
                     busy_wait_for preempted_phase_1;
                     busy_wait_for preempted_by_c)
                  ()
                  { effc = (fun (type a) (eff : a Effect.t) ->
                      match eff with
                      | Preemption ->
                        if !phase = 2 then
                          Some (fun (k : (a, _) continuation) ->
                            preempted_by_c := true;
                            continue k ())
                        else None
                      | _ -> None)
                  }
              ) ()
              { effc =
                  (fun (type a) (_eff : a Effect.t) -> None)
              })
           ()
           { effc =
               (fun (type a) (_eff : a Effect.t) -> None)
           }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       })
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            preempted_phase_1 := true;
            saved_k := Some k;
            phase := 2)
        | _ -> None)
    };
  assert !preempted_phase_1;
  let k =
    match !saved_k with
    | Some k -> k
    | None -> failwith "No continuation saved"
  in
  continue k ();
  assert !preempted_by_c;
  print_endline "OK"
;;

let multiple_nonpreemptible_layers () =
  print_endline "# Multiple non-preemptible layers between preemptible";
  let inner_preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Continue)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         Effect.Deep.try_with (fun () ->
           Effect.Deep.try_with (fun () ->
             Preemptible.try_with
               ~on_tick:(fun () -> Preempt)
               (fun () -> busy_wait_for inner_preempted)
               ()
               { effc = (fun (type a) (eff : a Effect.t) ->
                   match eff with
                   | Preemption ->
                     Some (fun (k : (a, _) continuation) ->
                       inner_preempted := true;
                       continue k ())
                   | _ -> None)
               }
           ) ()
           { effc =
               (fun (type a) (_eff : a Effect.t) -> None)
           }
         ) ()
         { effc =
             (fun (type a) (_eff : a Effect.t) -> None)
         }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       })
    ()
    { effc =
        (fun (type a) (_eff : a Effect.t) -> None)
    };
  assert !inner_preempted;
  print_endline "OK"
;;

let inner_preemptible_finishes_then_outer_ticks () =
  print_endline
    "# Inner preemptible finishes, outer still gets ticks";
  let inner_done = ref false in
  let outer_preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Preempt)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         Preemptible.try_with
           ~on_tick:(fun () -> Continue)
           (fun () -> inner_done := true)
           ()
           { effc =
               (fun (type a) (_eff : a Effect.t) -> None)
           }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       };
       assert !inner_done;
       busy_wait_for outer_preempted)
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            outer_preempted := true;
            continue k ())
        | _ -> None)
    };
  assert !outer_preempted;
  print_endline "OK"
;;

let inner_preemptible_finishes_sibling_ticks () =
  print_endline
    "# Inner preemptible finishes, sibling preemptible ticks";
  let first_done = ref false in
  let second_preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Continue)
    (fun () ->
       (* First inner preemptible fiber: finishes immediately *)
       Effect.Deep.try_with (fun () ->
         Preemptible.try_with
           ~on_tick:(fun () -> Continue)
           (fun () -> first_done := true)
           ()
           { effc =
               (fun (type a) (_eff : a Effect.t) -> None)
           }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       };
       assert !first_done;
       (* Second inner preemptible fiber: should get ticks *)
       Effect.Deep.try_with (fun () ->
         Preemptible.try_with
           ~on_tick:(fun () -> Preempt)
           (fun () -> busy_wait_for second_preempted)
           ()
           { effc = (fun (type a) (eff : a Effect.t) ->
               match eff with
               | Preemption ->
                 Some (fun (k : (a, _) continuation) ->
                   second_preempted := true;
                   continue k ())
               | _ -> None)
           }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       })
    ()
    { effc =
        (fun (type a) (_eff : a Effect.t) -> None)
    };
  assert !second_preempted;
  print_endline "OK"
;;

let three_preemptible_middle_finishes () =
  print_endline
    "# Three preemptible fibers, middle finishes, inner ticks";
  let middle_done = ref false in
  let inner_preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Continue)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         (* Middle preemptible fiber: finishes immediately *)
         Preemptible.try_with
           ~on_tick:(fun () -> Continue)
           (fun () ->
              middle_done := true;
              (* Inner preemptible fiber: should get ticks
                 after middle finishes *)
              Effect.Deep.try_with (fun () ->
                Preemptible.try_with
                  ~on_tick:(fun () -> Preempt)
                  (fun () -> busy_wait_for inner_preempted)
                  ()
                  { effc = (fun (type a) (eff : a Effect.t) ->
                      match eff with
                      | Preemption ->
                        Some (fun (k : (a, _) continuation) ->
                          inner_preempted := true;
                          continue k ())
                      | _ -> None)
                  }
              ) ()
              { effc =
                  (fun (type a) (_eff : a Effect.t) -> None)
              })
           ()
           { effc =
               (fun (type a) (_eff : a Effect.t) -> None)
           }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       })
    ()
    { effc =
        (fun (type a) (_eff : a Effect.t) -> None)
    };
  assert !middle_done;
  assert !inner_preempted;
  print_endline "OK"
;;

let multiple_layers_outer_preempts () =
  print_endline
    "# Multiple non-preemptible layers, outer preempts";
  let outer_preempted = ref false in
  Preemptible.try_with
    ~on_tick:(fun () -> Preempt)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         Effect.Deep.try_with (fun () ->
           Preemptible.try_with
             ~on_tick:(fun () -> Continue)
             (fun () -> busy_wait_for outer_preempted)
             ()
             { effc =
                 (fun (type a) (_eff : a Effect.t) -> None)
             }
         ) ()
         { effc =
             (fun (type a) (_eff : a Effect.t) -> None)
         }
       ) ()
       { effc =
           (fun (type a) (_eff : a Effect.t) -> None)
       })
    ()
    { effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption ->
          Some (fun (k : (a, _) continuation) ->
            outer_preempted := true;
            continue k ())
        | _ -> None)
    };
  assert !outer_preempted;
  print_endline "OK"
;;

let () =
  Domain.Tick.with_ ~interval_usec:1_000 (fun _ ->
    perform_normal_effect ();
    preempt_on_tick ();
    preempt_after_two_ticks ();
    two_preemptible_inner_handles_tick ();
    two_preemptible_outer_handles_tick ();
    resume_preempted_within_preemptible ();
    resume_cont_with_nonpreemptible ();
    resume_preempted_in_fiber_then_tick ();
    resume_preempted_with_inner_preemptible ();
    multiple_nonpreemptible_layers ();
    inner_preemptible_finishes_then_outer_ticks ();
    inner_preemptible_finishes_sibling_ticks ();
    three_preemptible_middle_finishes ();
    multiple_layers_outer_preempts ())
;;
