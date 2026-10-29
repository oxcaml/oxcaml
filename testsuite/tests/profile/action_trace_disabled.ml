(* TEST
 unset DUNE_ACTION_TRACE_DIR;
 include ocamlcommon;
 bytecode;
*)

let () =
  let gettimeofday () = failwith "Clock called with tracing disabled" in
  assert (Profile.with_action_trace ~gettimeofday ~name:"disabled"
    (fun () -> 42) = 42)

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
