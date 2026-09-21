(* TEST
 unset DUNE_ACTION_TRACE_DIR;
 include ocamlcommon;
 bytecode;
*)

let () =
  assert (not (Action_trace.enabled ()));
  let gettimeofday () = failwith "Clock called with tracing disabled" in
  assert (Profile.record_action ~gettimeofday ~name:"disabled"
    (fun () -> 42) = 42);
  Action_trace.with_fresh_context ~name:"disabled" ~f:(fun context ->
    Action_trace.Context.emit context
      (Action_trace.Event.span ~category:"test" ~name:"disabled"
         ~start_in_nanoseconds:0 ~finish_in_nanoseconds:1_000 ()))
