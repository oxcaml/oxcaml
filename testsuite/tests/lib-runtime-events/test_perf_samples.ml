(* TEST
 {
   runtime4;
   skip;
 }{
   include runtime_events;
   runtime5;
   { bytecode; }
   { native; }
 }
*)

(* Tests that:
 * - the perf_sample array is passed to runtime_begin and runtime_end callbacks
 * - the perf_sample array is passed to user span callbacks
 * - when no OCAML_RUNTIME_EVENTS_PERF_COUNTERS env var is set, the array is empty
 * - the callback signatures match the expected API
 *)

open Runtime_events

(* External for getting length of unboxed product arrays *)
external[@layout_poly] array_length :
  ('a : any mod separable). local_ 'a array -> int =
  "%array_length"

let samples_seen_begin = ref 0
let samples_seen_end = ref 0
let empty_arrays_begin = ref 0
let empty_arrays_end = ref 0

let runtime_begin _domain_id _ts _phase
    (local_ perf_samples : perf_sample array) =
    incr samples_seen_begin;
    if array_length perf_samples = 0 then
      incr empty_arrays_begin

let runtime_end _domain_id _ts _phase
    (local_ perf_samples : perf_sample array) =
    incr samples_seen_end;
    if array_length perf_samples = 0 then
      incr empty_arrays_end

(* User span event testing *)
type User.tag += PerfSpan

let user_span_event = User.register "perf.span" PerfSpan Type.span

let user_span_begin_seen = ref 0
let user_span_end_seen = ref 0
let user_span_empty_arrays = ref 0

let user_span_handler _domain_id _ts _ev v
    (local_ perf_samples : perf_sample array) =
  (match v with
   | Type.Begin -> incr user_span_begin_seen
   | Type.End -> incr user_span_end_seen);
  if array_length perf_samples = 0 then
    incr user_span_empty_arrays

let () =
    start ();
    let cursor = create_cursor None in
    let callbacks =
      Callbacks.create ~runtime_begin ~runtime_end ()
      |> Callbacks.add_user_event Type.span user_span_handler
    in
    (* Emit user span events *)
    User.write user_span_event Begin;
    (* Trigger some GC activity inside the span *)
    for _ = 1 to 10 do
      ignore (Sys.opaque_identity (Array.make 1000 0));
      Gc.minor ()
    done;
    User.write user_span_event End;
    (* Poll for events *)
    for _ = 0 to 100 do
      ignore (read_poll cursor callbacks None)
    done;
    (* Verify runtime callbacks *)
    assert (!samples_seen_begin > 0);
    assert (!samples_seen_end > 0);
    assert (!empty_arrays_begin = !samples_seen_begin);
    assert (!empty_arrays_end = !samples_seen_end);
    (* Verify user span callbacks *)
    assert (!user_span_begin_seen > 0);
    assert (!user_span_end_seen > 0);
    assert (!user_span_empty_arrays =
            !user_span_begin_seen + !user_span_end_seen);
    print_endline "perf_samples callback test passed"
