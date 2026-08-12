(* TEST
  { bytecode; }
  { native; }
*)

(* Regression test: young handler closures installed on a suspended
   continuation must survive a minor GC.

   [caml_continuation_update_handler_noexc] (the primitive behind
   [Shallow.continue_with] and [continue_with_handler]) detaches the stack
   of a suspended continuation, overwrites its retc/exnc/effc/tick slots
   with plain stores, and re-attaches it. The handler slots live in
   malloc'd stack memory, so no write barrier records them. If the
   continuation has been promoted to the major heap, nothing scans its
   suspended stack during a minor collection: the young handler closures
   are collected out from under it. The stdlib only avoids this because no
   poll point happens to fall between the update and the resume, but the
   middle end does not guarantee that, and
   [caml_continuation_update_tick_handler_noexc] is used on continuations
   that stay suspended indefinitely.

   This test performs the update and resume separately, with the minor GC
   that the no-poll window is supposed to exclude in between. To observe
   the corruption deterministically (rather than crashing), the new value
   handler captures a young ref that is also reachable from a global, so
   the minor GC promotes the ref but (with the bug) not the closure: the
   stale closure environment still points at the old minor-heap copy,
   whose first field now holds a forwarding pointer. Nothing allocates
   between the GC and the handler running, so the stale closure body still
   executes and reads the clobbered field. *)

open Effect

(* Mirror of [Effect.Prim.update_cont_handler_noexc]. *)
external update_handlers :
  ('a, 'b) Shallow.continuation
  -> ('b -> 'r)                       (* value handler *)
  -> (exn -> 'r)                      (* exception handler *)
  -> ('c t -> 'd)                     (* effect handler; not invoked here *)
  -> (unit -> tick_outcome) or_null   (* tick handler *)
  -> ('a, 'b) Shallow.continuation
  = "caml_continuation_update_handler_noexc" [@@noalloc]

(* Mirror of [Effect.Prim.continue]. *)
external resume : ('a, 'b) Shallow.continuation -> 'a -> 'r = "%continue"

(* Keeps the captured ref reachable so the minor GC promotes it away from
   under the (unscanned) closure environment. *)
let keep : int ref option ref = ref None

(* Not inlined, so that the young closures are dead in every caller frame
   (including on the bytecode stack) once it returns: after this, the
   suspended stack holds the only references to them. *)
let[@inline never] install k c =
  let retc n = n + !c in
  let effc _ = failwith "effc must not run" in
  update_handlers k retc raise effc Null

let () =
  let k = Shallow.fiber (fun n -> Sys.opaque_identity (n + 1)) in
  (* Promote the suspended continuation (and, transitively, its current
     stack contents) to the major heap. *)
  Gc.minor ();
  let c = Sys.opaque_identity (ref 1) in
  keep := Some c;
  let k = install k c in
  (* The minor GC between update and resume. The stack now holds the only
     references to the new handler closures; if it isn't scanned, [retc]
     is lost and its environment slot goes stale. *)
  Gc.minor ();
  c := 41;
  let result : int = resume k 100 in
  print_endline (if result = 142 then "ok" else "CORRUPT")
