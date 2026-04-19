(* TEST
 {
   include runtime_events;
   set OCAML_RUNTIME_EVENTS_PERF_COUNTERS = "r3c";
   runtime5;
   native;
 }
*)

(* Tests the PMC-enabled end-to-end path: when
   OCAML_RUNTIME_EVENTS_PERF_COUNTERS is set and PMC setup succeeds, every
   runtime and user-span callback receives a perf_sample array of length 1
   (one counter configured). Exercises parse_perf_config_string,
   perf_events_setup, the header perf-counter bitfield, write_to_ring, and
   extract_perf_data. If PMC is unavailable at runtime (non-Linux, non-x86,
   sandboxed CI, RDPMC-unavailable kernel, etc.) the test exits cleanly as a
   no-op success. *)

open Runtime_events

external[@layout_poly] array_length :
  ('a : any mod separable). local_ 'a array -> int =
  "%array_length"

let done_line () = print_endline "perf_samples enabled test passed"

type User.tag += PerfSpan

let user_span_event = User.register "perf.span" PerfSpan Type.span

let () =
  start ();
  if not (perf_counters_active ()) then begin
    done_line ();
    exit 0
  end;

  let begin_seen = ref 0 in
  let end_seen = ref 0 in
  let user_begin_seen = ref 0 in
  let user_end_seen = ref 0 in
  let bad_length = ref 0 in

  let check_length (local_ samples : perf_sample array) =
    if array_length samples <> 1 then incr bad_length
  in

  let runtime_begin _domain_id _ts _phase
      (local_ samples : perf_sample array) =
    incr begin_seen;
    check_length samples
  in

  let runtime_end _domain_id _ts _phase
      (local_ samples : perf_sample array) =
    incr end_seen;
    check_length samples
  in

  let user_span_handler _domain_id _ts _ev v
      (local_ samples : perf_sample array) =
    (match v with
     | Type.Begin -> incr user_begin_seen
     | Type.End -> incr user_end_seen);
    check_length samples
  in

  let cursor = create_cursor None in
  let callbacks =
    Callbacks.create ~runtime_begin ~runtime_end ()
    |> Callbacks.add_user_event Type.span user_span_handler
  in

  User.write user_span_event Begin;
  for _ = 1 to 10 do
    ignore (Sys.opaque_identity (Array.make 1000 0));
    Gc.minor ()
  done;
  User.write user_span_event End;

  for _ = 0 to 100 do
    ignore (read_poll cursor callbacks None)
  done;

  assert (!begin_seen > 0);
  assert (!end_seen > 0);
  assert (!user_begin_seen > 0);
  assert (!user_end_seen > 0);
  assert (!bad_length = 0);
  done_line ()
