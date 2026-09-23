open Effect
open Effect.Deep

type 'a preemption_action =
  | Resume
  | Handled of 'a

let run_with_tick_handler ?(interval_usec = 100_000) ?(repeating = false)
    ~on_preemption computation =
  (* The handler returns an aliased value; [with_] requires a unique result. *)
  let get_result = Domain.Tick.with_ ~interval_usec (fun () ->
      let preempted_once = ref false in
      let result = Preemptible.try_with
        ~on_tick:(fun () ->
          if !preempted_once && not repeating
          then Continue
          else (preempted_once := true; Preempt))
        computation
        ()
        { effc = (fun (type a) (e : a t) ->
            match e with
            | Preemption -> Some (fun (k : (a, _) continuation) ->
              let resume () = continue k () in
              match on_preemption resume with
              | Resume -> resume ()
              | Handled result -> result)
            | _ -> None) }
      in
      fun () -> result)
  in
  get_result () [@nontail]
