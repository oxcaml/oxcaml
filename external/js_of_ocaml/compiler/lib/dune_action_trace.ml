open! Stdlib

let now () = Time_now.nanoseconds_since_unix_epoch ()

let sys_time_to_nanoseconds time = Base.Int63.to_int_trunc time

let[@inline always] add_trace_event_if_enabled
    ~event_tracing_context
    ~(category : string)
    ~(name : string)
    f =
  match Build_action_trace_kernel.enabled () with
  | false -> f ()
  | true ->
      let start = now () in
      let result = f () in
      let finish = now () in
      let event =
        Build_action_trace_kernel.Event.span
          ~category
          ~name
          ~start_in_nanoseconds:(sys_time_to_nanoseconds start)
          ~finish_in_nanoseconds:(sys_time_to_nanoseconds finish)
          ()
      in
      Build_action_trace_kernel.Context.emit event_tracing_context event;
      result
