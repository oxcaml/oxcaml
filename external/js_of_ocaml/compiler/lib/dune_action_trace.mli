open! Stdlib

(* NOTE: [add_trace_event_if_enabled] lets us time the time
   JSOO spends on individual files using the [build_action_trace_kernel_library].  *)
val add_trace_event_if_enabled :
     event_tracing_context:Build_action_trace_kernel.Context.t
  -> category:string
  -> name:string
  -> (unit -> 'a)
  -> 'a
