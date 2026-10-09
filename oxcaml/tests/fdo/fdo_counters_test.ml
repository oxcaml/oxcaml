(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

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

let fn demangled_name =
  F.Definition_id.create_original ~demangled_name ~discriminator:0

let pos function_id function_body_hash ast_pos edge =
  F.Position.create_interior ~function_id ~function_body_hash ~ast_pos ~edge

let counter position inlining_stack : F.t = { position; inlining_stack }

let leaf_a = fn "A.f"

let body_hash n = F.Function_body_hash.of_int32 (Int32.of_int n)

let body_a = body_hash 0xa

let body_b = body_hash 0xb

let body_d = body_hash 0xd

let body_edited = body_hash 0xed

let leaf_c = fn "C.f"

let ctx_b = pos (fn "B.g") body_b 3 F.Edge.Callsite

let ctx_d = pos (fn "D.g") body_d 4 F.Edge.Callsite

let edge_a edge = pos leaf_a body_a 7 edge

let specialize function_id =
  F.Definition_id.create_specialized ~unspecialized:function_id
    ~specialization_site:ctx_d

(* Entry counters do not depend on the body: a function keeps its identity
   across edits, while its interior counters (which carry the body hash) do not.
   Specialization at a module binding preserves this. *)
let () =
  let entry = counter (F.Position.create_function_entry leaf_a) [] in
  check "interior counters depend on the body"
    (not
       (F.equal
          (counter (pos leaf_a body_a 7 Then) [])
          (counter (pos leaf_a body_edited 7 Then) [])));
  let inner =
    counter
      (F.Position.create_instantiation_site
         (F.Definition_id.create_original ~demangled_name:"Example.Inner"
            ~discriminator:0))
      []
  in
  check "instantiated entry has no body hash"
    (F.equal
       (F.specialize entry ~at:inner)
       (counter
          (F.Position.create_function_entry
             (F.Definition_id.create_specialized ~unspecialized:leaf_a
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
  let entry = counter (F.Position.create_function_entry leaf_a) [] in
  check "entry inlining preserves function identity"
    (F.equal
       (F.inline entry ~at:(counter ctx_b []))
       (counter (F.Position.create_function_entry leaf_a) [ctx_b]));
  check "entry specialization changes function identity structurally"
    (F.equal
       (F.specialize entry ~at:(counter ctx_d []))
       (counter (F.Position.create_function_entry (specialize leaf_a)) []))

(* The entry counters of inlined calls, added to an edge's counters. *)
let () =
  let inlined_calls =
    [ counter (F.Position.create_function_entry leaf_c) [ctx_b];
      counter (F.Position.create_function_entry leaf_c) [ctx_d] ]
  in
  let edge = [counter (edge_a Then) []] in
  let counters = F.add_all edge inlined_calls in
  check "inlined calls follow the edge's counters"
    (List.equal F.equal counters (counter (edge_a Then) [] :: inlined_calls));
  check "inlined calls are deduplicated"
    (List.equal F.equal (F.add_all counters [List.hd inlined_calls]) counters)

(* Hashes are part of the profile format, not just an equality shortcut. *)
let () =
  let entry = F.Position.create_function_entry leaf_a in
  let interior = edge_a Then in
  let module_site = F.Position.create_instantiation_site (fn "Example.Inner") in
  let specialized = specialize leaf_a in
  check "golden function hash"
    (F.Hash.equal (F.Definition_id.hash leaf_a) (F.Hash.of_int32 0x819a56d5l));
  check "golden position hash"
    (F.Hash.equal (F.Position.hash interior) (F.Hash.of_int32 0xf29d67eel));
  check "golden specialized function hash"
    (F.Hash.equal
       (F.Definition_id.hash specialized)
       (F.Hash.of_int32 0x92925811l));
  check "entry hash agrees with function hash"
    (F.Hash.equal (F.Position.hash entry) (F.Definition_id.hash leaf_a));
  check "only entries carry the entry bit"
    (F.Hash.is_function_entry (F.Position.hash entry)
    && (not (F.Hash.is_function_entry (F.Position.hash interior)))
    && not (F.Hash.is_function_entry (F.Position.hash module_site)));
  check "hash conversion preserves all bits"
    (Int32.equal (F.Hash.to_int32 (F.Hash.of_int32 0xffffffffl)) 0xffffffffl);
  check "body-hash conversion preserves all bits"
    (Int32.equal
       (F.Function_body_hash.to_int32
          (F.Function_body_hash.of_int32 0xffffffffl))
       0xffffffffl);
  check "hash ordering is unsigned"
    (F.Hash.compare (F.Hash.of_int32 0x80000000l) (F.Hash.of_int32 0x7fffffffl)
    > 0);
  check "specialization recomputes cached hashes"
    (List.equal F.Hash.equal
       (F.hash (F.specialize (counter interior []) ~at:(counter ctx_d [])))
       (F.hash (counter (pos specialized body_a 7 Then) [])))

(* Nested components must not collapse to the same polynomial. *)
let () =
  List.iter
    (fun (case, edge) ->
      check "switch case and later AST position have distinct hashes"
        (not
           (F.Hash.equal
              (F.Position.hash (pos leaf_a body_a 7 (Switch_case case)))
              (F.Position.hash (pos leaf_a body_a 10 edge)))))
    [0, F.Edge.Then; 1, F.Edge.Else; 2, F.Edge.Callsite];
  let a = counter (edge_a Then) [] in
  let b = counter (edge_a Else) [] in
  check "incoming counters are deduplicated in first-occurrence order"
    (List.equal F.equal (F.add_all [a] [b; b; a]) [a; b])

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
