(* TEST
 modules = "tick_stack_realloc_.c";
 {
   bytecode;
 }{
   stack-checks;
   native;
 }
*)

(* Regression test for a use-after-free in [caml_tick_fiber_res].

   The tick walk recurses over the fiber chain holding raw [stack_info]
   pointers and runs each preemptible fiber's tick handler as a callback,
   outermost first. Those callbacks run on the innermost fiber's stack: if
   one of them grows that stack, [caml_try_realloc_stack] frees the
   innermost [stack_info], and the walk's innermost frame then reads the
   tick handler from freed memory.

   Structure: a preemptible parent fiber (tick handler [parent_tickc])
   runs a preemptible child fiber (tick handler [child_tickc]). The child
   triggers a tick walk synchronously via a C stub. On the first walk,
   [parent_tickc] grows the child's stack, then runs a throwaway
   preemptible fiber whose stack allocations sweep the stack cache,
   reusing the child's freed [stack_info] and overwriting its stale
   tick-handler slot with [wrong_tickc]. The buggy walk then reads that
   freed slot: it skips [child_tickc] and calls [wrong_tickc] instead. *)

open Effect
open Effect.Deep

external request_and_process_tick : unit -> unit
  = "test_request_and_process_tick"

let parent_ticks = ref 0
let child_ticks = ref 0
let wrong_ticks = ref 0

(* Deep enough to force stack reallocation through every pooled size
   class, whatever size the fiber's stack starts at. *)
let depth = 10_000

let rec grow n = if n = 0 then 0 else 1 + grow (Sys.opaque_identity (n - 1))

let wrong_tickc () =
  incr wrong_ticks;
  Continue

(* Regrow through the same stack-cache size classes the child's stack
   just vacated, so its freed stack_info is reallocated and the stale
   tick-handler slot is overwritten with [wrong_tickc]. *)
let reuse_freed_stacks () =
  Preemptible.match_with (fun () -> ignore (grow depth)) ()
    { retc = (fun () -> ());
      exnc = raise;
      tickc = wrong_tickc;
      effc = (fun (type a) (_ : a t) -> None) }

let parent_tickc () =
  incr parent_ticks;
  if !parent_ticks = 1 then begin
    (* Runs on the child's stack: grow it, freeing the stack_info that
       the in-progress tick walk still holds a pointer to. *)
    ignore (grow depth);
    reuse_freed_stacks ()
  end;
  Continue

let child_tickc () =
  incr child_ticks;
  Continue

let report walk =
  Printf.printf "walk %d: parent=%d child=%d wrong=%d\n"
    walk !parent_ticks !child_ticks !wrong_ticks

let child () =
  (* First walk: [parent_tickc] reallocates this fiber's stack mid-walk. *)
  request_and_process_tick ();
  report 1;
  (* Second walk: no reallocation; both handlers must run. *)
  request_and_process_tick ();
  report 2

let parent () =
  Preemptible.match_with child ()
    { retc = (fun () -> ());
      exnc = raise;
      tickc = child_tickc;
      effc = (fun (type a) (_ : a t) -> None) }

let () =
  Preemptible.match_with parent ()
    { retc = (fun () -> ());
      exnc = raise;
      tickc = parent_tickc;
      effc = (fun (type a) (_ : a t) -> None) }
