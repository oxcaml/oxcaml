open! Stdlib

external now_in_nanoseconds : unit -> int = "oxcaml_dune_action_trace_now_in_nanoseconds" [@@noalloc]

let[@inline always] add_trace_event_if_enabled
    ~event_tracing_context
    ~(category : string)
    ~(name : string)
    f =
  match Build_action_trace_kernel.enabled () with
  | false -> f ()
  | true ->
      let start_in_nanoseconds = now_in_nanoseconds () in
      let result = f () in
      let finish_in_nanoseconds = now_in_nanoseconds () in
      let event =
        Build_action_trace_kernel.Event.span
          ~category
          ~name
          ~start_in_nanoseconds
          ~finish_in_nanoseconds
          ()
      in
      Build_action_trace_kernel.Context.emit event_tracing_context event;
      result
