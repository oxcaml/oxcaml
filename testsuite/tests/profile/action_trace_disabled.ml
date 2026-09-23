(* TEST
 unset DUNE_ACTION_TRACE_DIR;
 include ocamlcommon;
 bytecode;
*)

let () =
  assert (not (Action_trace.enabled ()));
  (* The clock is still installed for timing passes, but no span is recorded
     and the profile is never serialised. *)
  let clock_calls = ref 0 in
  let gettimeofday () = incr clock_calls; 0. in
  assert (Profile.record_action ~gettimeofday ~name:"disabled"
    (fun () -> 42) = 42);
  assert (!clock_calls = 1);
  Action_trace.with_fresh_context ~name:"disabled" ~f:(fun context ->
    Action_trace.Context.emit context
      (Action_trace.Event.span ~category:"test" ~name:"disabled"
         ~start_in_nanoseconds:0 ~finish_in_nanoseconds:1_000 ()))

let () =
  let counter_calls = ref 0 in
  let counter_f count =
    incr counter_calls;
    Profile.Counters.(set "nodes" count (create ()))
  in
  let record columns =
    Clflags.profile_columns := columns;
    assert (Profile.record_call_with_counters ~counter_f "pass"
      (fun () -> 42) = 42)
  in
  record [];
  assert (!counter_calls = 0);
  record [`Time];
  assert (!counter_calls = 0);
  record [`Counters];
  assert (!counter_calls = 1)
