open Effect
open Effect.Deep

external preempt_self : unit -> unit = "caml_domain_preempt_self"
[@@noalloc]

let with_preemption_setup ?(interval = 0.1) ?(repeating = false) f =
  let it_interval = if repeating then interval else 0. in
  Unix.setitimer ITIMER_REAL { it_interval; it_value = interval } |> ignore;
  Sys.set_signal Sys.sigalrm (Signal_handle (fun _ -> preempt_self ()))
  |> ignore;
  Fun.protect f
    ~finally:(fun () ->
      Unix.setitimer ITIMER_REAL { it_interval = 0.; it_value = 0. } |> ignore)

type 'a preemption_action =
  | Resume
  | Handled of 'a

let run_with_tick_handler ?(interval = 0.1) ?(repeating = false)
    ~on_preemption computation =
  with_preemption_setup ~interval ~repeating (fun () ->
    try_with
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
  )
