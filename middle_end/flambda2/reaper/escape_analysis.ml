(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* An allocation [a] (see [allocated_in]) may be stack-allocated in a region
   spanning its region scope (the body of the enclosing function, or the handler
   of the enclosing recursive continuation) if the allocated value can never be
   reached once control leaves that scope, and is never stored in a block that
   lives on the heap. We follow the allocated value forwards through the [alias]
   relation computed by the points-to analysis (which already accounts for flow
   through function arguments, results and known block fields), and declare that
   it escapes if it reaches:

   - a node with [any_usage] (unknown function, external call, mutation, ...);

   - a field of a block that is not itself a candidate (a heap or static block),
   or a field of a candidate that escapes ([contained_in]); besides, a value
   stored in a candidate block is live wherever that block is (see [owner]
   below), since it can be retrieved from it (and is passed along with the block
   if the latter is unboxed);

   - a parameter of a continuation defined outside the scope (see [scope_exit]):
   for a function, its return or exception values; for a recursive handler, this
   includes the parameters of the recursive continuations themselves, so
   loop-carried values escape.

   - an argument (or the callee) of a tail call, since the region is closed
   before the call to keep it a tail call (see [Traverse.traverse_apply]).

   Besides, nothing is done to close regions on exceptional paths: as for the
   regions introduced by the type checker, the handler catching the exception is
   expected to restore the local stack pointer (handlers of try-with have a
   try-region of their own for that purpose, which they close on entry: this
   frees everything allocated in the body of the [try], so values passed to such
   a handler are treated as leaving the scope, see [Traverse]), or failing that,
   the next [End_region] of an enclosing region does. This is however not enough
   for the region of a loop iteration raising to a handler outside of the loop,
   since an enclosing loop could then accumulate unclosed regions; such scopes
   are marked [unsafe_scope] and get no local allocations (see [Traverse]).

   Finally, since an allocation always goes to the innermost region open at
   runtime, no allocation is a candidate while a region pre-existing in the term
   (as introduced by the type checker for [local_] allocations, and closed
   before the enclosing function returns) is open: see
   [Traverse_env.enter_foreign_region].

   Values that flow into a callee and come back through its result are tracked
   by the aliases created by the points-to analysis for calls, so no special
   treatment is needed for calls.

   A value reaching the return of the function containing its allocation does
   not necessarily escape: if all call sites of that function are known direct
   calls (see [cannot_change_calling_convention]), the function may be made to
   allocate its result in its caller's region, as [exclave_] does. The value
   then has to obey the rules of the scope of each call site, which is expressed
   by the [owner] relation. Functions for which this happens are recorded in
   [local_return]: their calling convention is changed so that they return at
   mode local (see [Unboxing_analysis.compute_code_changes] for the code
   metadata, and [Rebuild] for the function bodies and call sites).

   A partial application of a local closure (through an unknown-arity call)
   yields a closure pointing at it, allocated on the heap by the curry functions
   (as for the closures of the type checker); the nodes standing for such
   partial applications in the call witness chains are considered to contain the
   closure, so that they must not outlive its region either. *)

[@@@ocaml.warning "-not-principal"]

open Global_flow_graph.Relations
open Points_to_analysis.Relations
open! Datalog_helpers.Syntax
open Datalog_helpers

let enabled = Oxcaml_args.Extra_options.bool __LOC__ "reaper-escape"

(* [flow a x]: the value allocated at [a] may be the value of [x]. This is the
   restriction to candidates of the inverse of [sources] (from the points-to
   analysis), except that it is not cut off at nodes with [any_source]. The
   points-to analysis does not record the sources of such nodes, nor propagate
   them any further, since it cannot change their representation anyway; but a
   value joined with unknown values (for instance in a parameter of a function
   that is also called with the result of an exported function) may still never
   leave the scope of its allocation. Reusing [sources], and treating as
   escaping any value reaching a node with [any_source], was measured to lose
   about 40% of the stack allocations in the stdlib. *)
let flow = rel2 "escape_flow" Cols.[n; n]

let is_candidate = rel1 "escape_candidate" Cols.[n]

let escapes = rel1 "escapes" Cols.[n]

(* [escapes_anywhere a]: the value escapes for a reason independent of the
   region scopes (it may be used by unknown code, or stored in a heap block). *)
let escapes_anywhere = rel1 "escapes_anywhere" Cols.[n]

(* [owner a s]: the value allocated at [a] may be live in region scope [s]: [s]
   is the scope of the allocation, the scope of a call site receiving the value
   as the result of a function returning at mode local, or a scope owning a
   candidate block containing the value. *)
let owner = rel2 "escape_owner" Cols.[n; n]

(* [contained_in a b]: the value allocated at [a] may be stored in a field of
   the candidate block allocated at [b]. *)
let contained_in = rel2 "escape_contained_in" Cols.[n; n]

(* [returned a r]: the value allocated at [a], or a candidate block containing
   it, may be the value of the function result [r]. *)
let returned = rel2 "escape_returned" Cols.[n; n]

(* Indexes of input tables in the order needed by the rules below. *)
let is_function_result = rel1 "escape_is_function_result" Cols.[n]

let partial_application_of_code =
  rel2 "escape_partial_application_of_code" Cols.[n; n]

(* [is_closure_of a c]: the candidate [a] is a closure whose code is [c]. *)
let is_closure_of = rel2 "escape_closure_of" Cols.[n; n]

let local_allocation_tbl =
  Datalog.create_relation ~name:"local_allocation" Cols.[n]

let local_allocation x = local_allocation_tbl % [x]

let scope_with_local_allocation_tbl =
  Datalog.create_relation ~name:"scope_with_local_allocation" Cols.[n]

let scope_with_local_allocation x = scope_with_local_allocation_tbl % [x]

let local_return_tbl = Datalog.create_relation ~name:"local_return" Cols.[n]

let local_return x = local_return_tbl % [x]

let local_closure_code_tbl =
  Datalog.create_relation ~name:"local_closure_code" Cols.[n]

let local_closure_code x = local_closure_code_tbl % [x]

let scope_with_local_call_result_tbl =
  Datalog.create_relation ~name:"scope_with_local_call_result" Cols.[n]

let scope_with_local_call_result x = scope_with_local_call_result_tbl % [x]

let candidate_rules =
  saturate_in_order
    [ (let$ [s; r] = ["s"; "r"] in
       [return_of ~scope:s ~result:r] ==> is_function_result r);
      (let$ [q; c] = ["q"; "c"] in
       [partial_application_node ~node:q ~code_id:c]
       ==> partial_application_of_code c q);
      (let$ [a; s] = ["a"; "s"] in
       [allocated_in ~alloc:a ~scope:s] ==> and_ [is_candidate a; flow a a]);
      (let$ [a; wit; c] = ["a"; "wit"; "c"] in
       [ is_candidate a;
         constructor ~base:a !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       ==> is_closure_of a c) ]

(* When calling conventions may not be changed, no function may be made to
   return at mode local. *)
let no_calling_convention_change_rules =
  saturate_in_order
    [ (let$ [s; r] = ["s"; "r"] in
       [return_of ~scope:s ~result:r] ==> cannot_change_calling_convention s) ]

(* First stratum: where the allocated values flow, and the reasons for escaping
   that do not depend on region scopes. *)
let flow_rules =
  Schedule.fixpoint
    [ Schedule.saturate
        [ (let$ [a; x; y] = ["a"; "x"; "y"] in
           [flow a x; rev_alias ~from:x ~to_:y] ==> flow a y);
          (let$ [a; c; q] = ["a"; "c"; "q"] in
           [is_closure_of a c; partial_application_of_code c q] ==> flow a q);
          (let$ [a; x; relation; base] = ["a"; "x"; "relation"; "base"] in
           [ flow a x;
             rev_constructor ~from:x relation ~base;
             when1 Field.is_real_field relation;
             is_candidate base ]
           ==> contained_in a base);
          (let$ [a; x] = ["a"; "x"] in
           [flow a x; any_usage x] ==> escapes_anywhere a);
          (let$ [a; x; relation; base] = ["a"; "x"; "relation"; "base"] in
           [ flow a x;
             rev_constructor ~from:x relation ~base;
             when1 Field.is_real_field relation;
             ~~(is_candidate base) ]
           ==> escapes_anywhere a);
          (let$ [a; b] = ["a"; "b"] in
           [contained_in a b; escapes_anywhere b] ==> escapes_anywhere a) ] ]

(* Second stratum: the constraints of the region scopes, for the values that may
   still be made local. Values that escape anywhere need no [owner] or
   [returned] facts, which keeps these joins small. *)
let scope_rules =
  Schedule.fixpoint
    [ Schedule.saturate
        [ (let$ [a] = ["a"] in
           [escapes_anywhere a] ==> escapes a);
          (let$ [a; s] = ["a"; "s"] in
           [allocated_in ~alloc:a ~scope:s; ~~(escapes_anywhere a)]
           ==> owner a s);
          (let$ [a; b; s] = ["a"; "b"; "s"] in
           [contained_in a b; owner b s; ~~(escapes_anywhere a)] ==> owner a s);
          (let$ [a; r] = ["a"; "r"] in
           [flow a r; is_function_result r; ~~(escapes_anywhere a)]
           ==> returned a r);
          (let$ [a; b; r] = ["a"; "b"; "r"] in
           [contained_in a b; returned b r; ~~(escapes_anywhere a)]
           ==> returned a r);
          (let$ [a; b] = ["a"; "b"] in
           [contained_in a b; escapes b] ==> escapes a);
          (let$ [a; s; x] = ["a"; "s"; "x"] in
           [owner a s; flow a x; scope_exit ~scope:s ~param:x] ==> escapes a);
          (let$ [a; s] = ["a"; "s"] in
           [owner a s; unsafe_scope s] ==> escapes a);
          (* Returned from a function whose calling convention cannot be
             changed. *)
          (let$ [a; s; r] = ["a"; "s"; "r"] in
           [ owner a s;
             flow a r;
             return_of ~scope:s ~result:r;
             cannot_change_calling_convention s ]
           ==> escapes a);
          (* Returned to a call site that cannot host a local result. *)
          (let$ [a; s; r; q] = ["a"; "s"; "r"; "q"] in
           [ owner a s;
             flow a r;
             return_of ~scope:s ~result:r;
             flow a q;
             indirect_call_result q ]
           ==> escapes a);
          (let$ [a; s; r; q] = ["a"; "s"; "r"; "q"] in
           [ owner a s;
             flow a r;
             return_of ~scope:s ~result:r;
             flow a q;
             unscoped_call_result q ]
           ==> escapes a);
          (* Returned to a call site in scope [s']: the value is then also owned
             by [s']. *)
          (let$ [a; s; r; q; s'; w; wit] =
             ["a"; "s"; "r"; "q"; "s'"; "w"; "wit"]
           in
           [ owner a s;
             flow a r;
             return_of ~scope:s ~result:r;
             ~~(cannot_change_calling_convention s);
             flow a q;
             call_result ~result:q ~scope:s' ~call_widget:w;
             sources w wit;
             constructor ~base:wit !!Field.code_id_of_call_witness ~from:s ]
           ==> owner a s') ] ]

let decision_rules =
  saturate_in_order
    [ (let$ [a; s] = ["a"; "s"] in
       [allocated_in ~alloc:a ~scope:s; ~~(escapes a)]
       ==> and_ [local_allocation a; scope_with_local_allocation s]);
      (* Unboxed allocations no longer exist, so need no local return (but the
         local allocations they contain may). *)
      (let$ [a; s; r] = ["a"; "s"; "r"] in
       [ owner a s;
         local_allocation a;
         ~~(to_unbox a);
         returned a r;
         return_of ~scope:s ~result:r ]
       ==> local_return s);
      (let$ [x; wit; c] = ["x"; "wit"; "c"] in
       [ local_allocation x;
         constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       ==> local_closure_code c);
      (let$ [q; s'; w; wit; c] = ["q"; "s'"; "w"; "wit"; "c"] in
       [ call_result ~result:q ~scope:s' ~call_widget:w;
         sources w wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c;
         local_return c ]
       ==> scope_with_local_call_result s') ]

let scopes_of_allocation =
  let scopes =
    query
      (let^$ [a], [s] = ["a"], ["s"] in
       [allocated_in ~alloc:a ~scope:s] =>? [s])
  in
  fun db a ->
    Cursor.fold_with_parameters scopes [a] db ~init:[] ~f:(fun [s] acc ->
        s :: acc)

let returns_local db code_id =
  Code_id_or_name.Map.mem
    (Code_id_or_name.code_id code_id)
    (Datalog.get_table local_return_tbl db)

let is_local_closure_code db code_id =
  Code_id_or_name.Map.mem
    (Code_id_or_name.code_id code_id)
    (Datalog.get_table local_closure_code_tbl db)

let perform_analysis db ~stats =
  Profile.record_call ~accumulate:true "escape_analysis" (fun () ->
      let run db rules =
        List.fold_left (fun db rule -> Schedule.run ~stats rule db) db rules
      in
      let db = run db candidate_rules in
      let db =
        if Flambda_features.reaper_change_calling_conventions ()
        then db
        else run db no_calling_convention_change_rules
      in
      let db = Schedule.run ~stats flow_rules db in
      let db = Schedule.run ~stats scope_rules db in
      run db decision_rules)
