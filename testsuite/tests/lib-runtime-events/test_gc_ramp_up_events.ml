(* TEST
 include runtime_events;
 { bytecode; }
 { native; }
*)

(* Tests the runtime events for GC ramp-up (#15001):
   - a call to Gc.ramp_up is covered by one EV_EXPLICIT_GC_RAMP_UP span;
   - a major slice inside the ramp-up phase reports suspended words;
   - a major slice after Gc.ramp_down reports resumed words;
   - large allocations are reported as direct major allocations.
   Slice counters are only logged by slices that do work. *)

open Runtime_events

let in_ramp_up = ref false
let ramp_up_begins = ref 0
let ramp_up_ends = ref 0
let suspended_in_ramp_up = ref false
let resumed = ref false
let direct = ref false

let runtime_begin _dom _ts = function
  | EV_EXPLICIT_GC_RAMP_UP -> incr ramp_up_begins; in_ramp_up := true
  | _ -> ()

let runtime_end _dom _ts = function
  | EV_EXPLICIT_GC_RAMP_UP -> incr ramp_up_ends; in_ramp_up := false
  | _ -> ()

let runtime_counter _dom _ts counter v =
  match counter with
  | EV_C_MAJOR_SUSPENDED_ALLOCATED_WORDS ->
    if v > 0 && !in_ramp_up then suspended_in_ramp_up := true
  | EV_C_MAJOR_RESUMED_ALLOCATED_WORDS -> if v > 0 then resumed := true
  | EV_C_MAJOR_DIRECT_ALLOCATED_WORDS -> if v > 0 then direct := true
  | _ -> ()

let callbacks = Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter ()

(* Arrays large enough to be allocated directly in the major heap. Each is
   followed by a slice with an explicit budget: slices which find no work log
   no events, and during ramp-up a slice computing its own budget may have
   none. *)
let alloc_major n =
  let l = ref [] in
  for _ = 1 to n do
    l := Array.make 10_000 0 :: !l;
    ignore (Gc.major_slice 10_000)
  done;
  Sys.opaque_identity !l

let () =
  start ();
  let cursor = create_cursor None in
  ignore (Sys.opaque_identity (alloc_major 10));
  let live, suspended = Gc.ramp_up (fun () -> alloc_major 100) in
  ignore (read_poll cursor callbacks None);
  Gc.ramp_down suspended;
  ignore (Sys.opaque_identity (alloc_major 10));
  ignore (Sys.opaque_identity live);
  ignore (read_poll cursor callbacks None);
  Printf.printf "ramp-up spans: %d begin, %d end\n" !ramp_up_begins !ramp_up_ends;
  Printf.printf "suspended words in ramp-up: %b\n" !suspended_in_ramp_up;
  Printf.printf "resumed words: %b\n" !resumed;
  Printf.printf "direct words: %b\n" !direct
