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

type (-'a, 'x, +'b) cont : value mod non_float

type last_fiber [@@immediate]

external cont_set_last_fiber :
  _ cont -> last_fiber -> unit = "%setfield1"

external reperform :
  'a t -> ('a, _, 'b) cont -> last_fiber -> 'b = "%reperform"

external resume : ('a, _, 'b) cont -> ('c -> 'a) -> 'c -> 'b = "%resume"

type ('a,'x,'b) effc = 'a t -> ('a, 'x, 'b) cont -> last_fiber -> 'b

type tick_outcome =
  | Preempt
  | Continue

external with_stack_preemptible :
  ('x -> 'b) ->
  (exn -> 'b) ->
  ('a . ('a,'x,'b) effc) ->
  (unit -> tick_outcome) ->
  ('d -> 'x) ->
  'd ->
  'b = "%with_stack_preemptible"

type ('a,'b) continuation =
  | Cont : ('a,'x,'b) cont -> ('a, 'b) continuation [@@unboxed]

let continue (Cont k) v = resume k (fun x-> x) v

type ('a,'b) preemptible_handler =
  { retc: 'a -> 'b;
    exnc: exn -> 'b;
    effc: 'c.'c t -> (('c,'b) continuation -> 'b) option;
    tickc: unit -> tick_outcome }

let match_with_preemptible comp arg handler =
  let effc eff k last_fiber =
    match handler.effc eff with
    | Some f ->
      cont_set_last_fiber k last_fiber;
      f (Cont k)
    | None -> reperform eff k last_fiber
  in
  with_stack_preemptible handler.retc handler.exnc effc handler.tickc comp arg

type 'a effect_handler =
  { effc: 'b. 'b t -> (('b, 'a) continuation -> 'a) option }

let try_with_preemptible
  :  on_tick:(unit -> tick_outcome)
  -> ('b -> 'a)
  -> 'b
  -> 'a effect_handler
  -> 'a
  =
  fun ~on_tick comp arg handler ->
  match_with_preemptible comp arg
    { retc = Fun.id
    ; exnc = raise
    ; effc = handler.effc
    ; tickc = on_tick
    };
;;


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
  try_with_preemptible
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
  match_with_preemptible
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
  match_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () ->
      outer_ticked := true;
      Continue)
    (fun () ->
       (* Non-preemptible fiber between the two preemptible ones.
          This means the inner preemptible fiber's creation must walk
          up to find the outer preemptible fiber to set
          preemptible_child. *)
       Effect.Deep.try_with (fun () ->
         try_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () -> Preempt)
    (fun () ->
       (* Non-preemptible fiber between preemptible fibers *)
       Effect.Deep.try_with (fun () ->
         try_with_preemptible
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
  try_with_preemptible
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
  try_with_preemptible
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
  try_with_preemptible
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
  try_with_preemptible
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
  try_with_preemptible
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
  try_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () -> Continue)
    (fun () ->
       (* Non-preemptible fiber between A and B *)
       Effect.Deep.try_with (fun () ->
         (* B: preemptible, preempts in phase 1, continues in phase 2 *)
         try_with_preemptible
           ~on_tick:(fun () ->
             if !phase = 1 then Preempt else Continue)
           (fun () ->
              (* Non-preemptible fiber between B and C *)
              Effect.Deep.try_with (fun () ->
                (* C: preemptible, preempts always *)
                try_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () -> Continue)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         Effect.Deep.try_with (fun () ->
           Effect.Deep.try_with (fun () ->
             try_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () -> Preempt)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         try_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () -> Continue)
    (fun () ->
       (* First inner preemptible fiber: finishes immediately *)
       Effect.Deep.try_with (fun () ->
         try_with_preemptible
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
         try_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () -> Continue)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         (* Middle preemptible fiber: finishes immediately *)
         try_with_preemptible
           ~on_tick:(fun () -> Continue)
           (fun () ->
              middle_done := true;
              (* Inner preemptible fiber: should get ticks
                 after middle finishes *)
              Effect.Deep.try_with (fun () ->
                try_with_preemptible
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
  try_with_preemptible
    ~on_tick:(fun () -> Preempt)
    (fun () ->
       Effect.Deep.try_with (fun () ->
         Effect.Deep.try_with (fun () ->
           try_with_preemptible
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
  let tick = Domain.Tick.acquire ~interval_usec:1000 in
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
  multiple_layers_outer_preempts ();
  Domain.Tick.release tick
;;
