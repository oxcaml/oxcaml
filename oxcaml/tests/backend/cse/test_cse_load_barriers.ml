(* Regression test for CSE across memory fences. CSE must not merge a mutable
   load across [lfence] or [mfence] — the merge would effectively hoist the load
   above the fence.

   These externals are rewritten to fence instructions at direct call sites (see
   [backend/amd64/cfg_selection.ml]); this file is only compiled, never linked
   or executed, so the absence of C implementations does not matter. *)

external load_fence : unit -> unit = "caml_load_fence" [@@noalloc] [@@builtin]

external store_fence : unit -> unit = "caml_store_fence" [@@noalloc] [@@builtin]

external memory_fence : unit -> unit = "caml_memory_fence"
[@@noalloc] [@@builtin]

(* Baseline: without a barrier, the second load is CSE'd. *)
let[@inline never] no_barrier (r : int ref) = !r + !r

(* The load after [load_fence] must not be merged with the one before. *)
let[@inline never] load_across_load_fence (r : int ref) =
  let a = !r in
  load_fence ();
  a + !r

(* Same for [memory_fence]. *)
let[@inline never] load_across_memory_fence (r : int ref) =
  let a = !r in
  memory_fence ();
  a + !r

(* [store_fence] only orders stores, which CSE neither eliminates nor moves, so
   merging the two loads is allowed. *)
let[@inline never] load_across_store_fence (r : int ref) =
  let a = !r in
  store_fence ();
  a + !r
