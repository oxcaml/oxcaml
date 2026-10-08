(* The operations on pseudo-instrumentation counters that the compiler applies
   as it transforms code: inlining, specialization, and attaching the entry
   counters of inlined calls to the edges into the code they were inlined
   into. *)
module F = Fdo_counter

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.eprintf "FAILED: %s\n%!" name)

let fn unmangled_name = F.function_id ~unmangled_name ~discriminator:0

let pos function_id function_body_hash ast_pos edge =
  F.position ~function_id ~function_body_hash ~ast_pos ~edge

let counter position inlining_stack : F.t = { position; inlining_stack }

let leaf_a = fn "A.f"

let body_hash n = F.Function_body_hash.of_int32 (Int32.of_int n)

let body_a = body_hash 0xa

let body_b = body_hash 0xb

let body_d = body_hash 0xd

let body_edited = body_hash 0xed

let leaf_c = fn "C.f"

let ctx_b = pos (fn "B.g") body_b 3 F.Callsite

let ctx_d = pos (fn "D.g") body_d 4 F.Callsite

let edge_a edge = pos leaf_a body_a 7 edge

let specialize function_id =
  F.specialized ~unspecialized:function_id ~specialization_site:ctx_d

(* Entry counters do not depend on the body: a function keeps its identity
   across edits, while its interior counters (which carry the body hash) do not.
   Instantiation (a module initializer copied by specialization) preserves
   this. *)
let () =
  let entry = counter (F.function_entry leaf_a) [] in
  check "interior counters depend on the body"
    (not
       (F.equal
          (counter (pos leaf_a body_a 7 Then) [])
          (counter (pos leaf_a body_edited 7 Then) [])));
  let inner =
    counter
      (F.instantiation_site
         (F.function_id ~unmangled_name:"Example.Inner" ~discriminator:0))
      []
  in
  check "instantiated entry has no body hash"
    (F.equal
       (F.specialize entry ~at:inner)
       (counter
          (F.function_entry
             (F.specialized ~unspecialized:leaf_a
                ~specialization_site:inner.position))
          []))

(* Inlining appends the call site, innermost first; specialization renames the
   outermost function identity. *)
let () =
  let then_ = counter (edge_a Then) [] in
  let once = F.inline then_ ~at:(counter ctx_b []) in
  let twice = F.inline once ~at:(counter ctx_d []) in
  check "inlining context" (F.equal once (counter (edge_a Then) [ctx_b]));
  check "nested inlining context"
    (F.equal twice (counter (edge_a Then) [ctx_b; ctx_d]));
  check "call site itself has typed inlining context"
    (F.equal
       (F.inline (counter ctx_b []) ~at:(counter ctx_d []))
       (counter ctx_b [ctx_d]));
  check "specialization changes only outermost function identity"
    (F.equal
       (F.specialize once ~at:(counter ctx_d []))
       (counter (edge_a Then) [pos (specialize (fn "B.g")) body_b 3 Callsite]));
  check "specialization of a position without context"
    (F.equal
       (F.specialize then_ ~at:(counter ctx_d []))
       (counter (pos (specialize leaf_a) body_a 7 Then) []));
  let entry = counter (F.function_entry leaf_a) [] in
  check "entry inlining preserves function identity"
    (F.equal
       (F.inline entry ~at:(counter ctx_b []))
       (counter (F.function_entry leaf_a) [ctx_b]));
  check "entry specialization changes function identity structurally"
    (F.equal
       (F.specialize entry ~at:(counter ctx_d []))
       (counter (F.function_entry (specialize leaf_a)) []))

(* The entry counters of inlined calls, added to an edge's counters. *)
let () =
  let inlined_calls =
    [ counter (F.function_entry leaf_c) [ctx_b];
      counter (F.function_entry leaf_c) [ctx_d] ]
  in
  let edge = [counter (edge_a Then) []] in
  let counters = F.add_all edge inlined_calls in
  check "inlined calls follow the edge's counters"
    (List.equal F.equal counters (counter (edge_a Then) [] :: inlined_calls));
  check "inlined calls are deduplicated"
    (List.equal F.equal (F.add_all counters [List.hd inlined_calls]) counters)

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
