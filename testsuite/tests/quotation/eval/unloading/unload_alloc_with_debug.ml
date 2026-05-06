(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming -I ${ocamlsrcdir}/utils";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Reproduces the bug in [backend/emitaux.ml] [emit_frame] where the
   check [if flags = 3] should be [if flags land 3 = 3]. When an
   unloadable function (compiled by [Eval.eval], so
   [fd_unloadable = true] and bit 2 of flags is set) contains
   allocations ([Dbg_alloc] frame descriptor) AND [Clflags.debug] is
   true with non-empty alloc debuginfo, [get_flags] returns 3 and the
   per-alloc debuginfo labels SHOULD be emitted. After the
   [flags lor 4] for UNLOADABLE, [flags] is 7 — and the buggy check
   [if flags = 3] is false, so the labels are NOT emitted.

   The runtime parser ([next_frame_descr] in
   [runtime/frame_descriptors.c]) sees bit 0 (DEBUG) and bit 1 (ALLOC)
   both set, and tries to skip [4 * num_allocs] bytes for the absent
   labels — silently misaligning every subsequent descriptor in the
   frame table. The next [next_frame_descr] read lands in padding /
   the start of the next descriptor and produces a garbage retaddr,
   which either trips [CAMLassert(Retaddr_frame(d) >= 4096)] in debug
   runtimes or inserts wrong entries into the frame-descriptor
   hashtable, causing wrong stack scans on the next major GC.

   With the bug fixed ([flags land 3 = 3]), this test runs to
   completion. With the bug present, expect either an assertion
   failure during [Eval.eval] (table construction) or a crash /
   wrong scan during the [Gc.compact] inside the eval'd function. *)

let () =
  (* Switch on debug-info emission for [Eval.eval]'s JIT compilation,
     so [Dbg_alloc] safe-points get [flags = 3] (DEBUG | ALLOC). *)
  Clflags.debug := true;
  let f =
    Eval.eval
      <[
        fun n ->
          (* Many allocations inside one function, multiple call sites,
             so the unit's frame table has multiple [Dbg_alloc] frames
             for the parser to walk past in sequence. *)
          let pair = (n, n + 1) in
          let triple = (n, n + 1, n + 2) in
          let lst = [n; n + 1; n + 2; n + 3] in
          (* Force a major GC mid-function. The stack scan reads the
             frame descriptor for this call site; if the descriptor was
             misaligned at registration time, the lookup either finds a
             garbage descriptor or fails outright. *)
          Gc.compact ();
          let (a, b) = pair in
          let (c, _, _) = triple in
          a + b + c + List.length lst
      ]>
  in
  let r = f 41 in
  Printf.printf "result = %d\n" r;
  (* A second compaction with the closure dropped should unload the
     unit cleanly if the frame table is well-formed. *)
  Gc.compact ();
  Printf.printf "ok\n"
