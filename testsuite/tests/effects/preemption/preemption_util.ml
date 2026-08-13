open Effect
open Effect.Deep

type 'a preemption_action =
  | Resume
  | Handled of 'a

let run_with_tick_handler ?(interval_usec = 100_000) ?(repeating = false)
    ~on_preemption computation =
  Domain.Tick.with_ ~interval_usec (fun _ ->
      let preempted_once = ref false in
      Preemptible.try_with
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
            | _ -> None) })
