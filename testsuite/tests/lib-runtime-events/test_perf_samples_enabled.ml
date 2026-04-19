(* TEST
 {
   include runtime_events;
   set OCAML_RUNTIME_EVENTS_PERF_COUNTERS = "r3c";
   runtime5;
   { bytecode; }
   { native; }
 }
*)

(* Tests the PMC-enabled end-to-end path: when
   OCAML_RUNTIME_EVENTS_PERF_COUNTERS is set and PMC setup succeeds, runtime
   and user-span callbacks are invoked and at least one callback observes a
   non-empty perf_sample array. Exercises parse_perf_config_string,
   perf_events_setup, the header perf-counter bitfield, write_to_ring, and
   extract_perf_data. If PMC is unavailable at runtime (non-Linux, non-x86,
   sandboxed CI, RDPMC-unavailable kernel, etc.) the test exits cleanly as a
   no-op success.

   Note: we intentionally do not assert on the exact value of array_length
   here. Native and bytecode unbox unboxed-product arrays differently, so
   `array_length` on a `local_ perf_sample array` returns the element count
   in native and the wosize (= element_count * number_of_components) in
   bytecode. We only check "non-empty" (length > 0), which is consistent
   across both modes. *)

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
  let any_nonempty = ref false in

  let mark_nonempty (local_ samples : perf_sample array) =
    if array_length samples > 0 then any_nonempty := true
  in

  let runtime_begin _domain_id _ts _phase
      (local_ samples : perf_sample array) =
    incr begin_seen;
    mark_nonempty samples
  in

  let runtime_end _domain_id _ts _phase
      (local_ samples : perf_sample array) =
    incr end_seen;
    mark_nonempty samples
  in

  let user_span_handler _domain_id _ts _ev v
      (local_ samples : perf_sample array) =
    (match v with
     | Type.Begin -> incr user_begin_seen
     | Type.End -> incr user_end_seen);
    mark_nonempty samples
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
  assert !any_nonempty;
  done_line ()
