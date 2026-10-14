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
   or a field of a candidate that escapes;

   - a parameter of a continuation defined outside the scope (see [scope_exit]):
   for a function, its return or exception values; for a recursive handler, this
   includes the parameters of the recursive continuations themselves, so
   loop-carried values escape.

   - an argument (or the callee) of a tail call, since the region is closed
   before the call to keep it a tail call (see [Traverse.traverse_apply]).

   Besides, nothing is done to close regions on exceptional paths: as for the
   regions introduced by the type checker, the handler catching the exception is
   expected to restore the local stack pointer (handlers of try-with have a
   region of their own for that purpose), or failing that, the next [End_region]
   of an enclosing region does. This is however not enough for the region of a
   loop iteration raising to a handler outside of the loop (whose try-region may
   have been removed by Simplify as unused), since an enclosing loop could then
   accumulate unclosed regions; such scopes are marked [unsafe_scope] and get no
   local allocations (see [Traverse]).

   Values that flow into a callee and come back through its result are tracked
   by the aliases created by the points-to analysis for calls, so no special
   treatment is needed for calls. *)

[@@@ocaml.warning "-not-principal"]

open Global_flow_graph.Relations
open Points_to_analysis.Relations
open! Datalog_helpers.Syntax
open Datalog_helpers

let enabled = Oxcaml_args.Extra_options.bool __LOC__ "reaper-escape"

(* [flow a x]: the value allocated at [a] may be the value of [x]. *)
let flow = rel2 "escape_flow" Cols.[n; n]

let is_candidate = rel1 "escape_candidate" Cols.[n]

let escapes = rel1 "escapes" Cols.[n]

let local_allocation_tbl =
  Datalog.create_relation ~name:"local_allocation" Cols.[n]

let local_allocation x = local_allocation_tbl % [x]

let scope_with_local_allocation_tbl =
  Datalog.create_relation ~name:"scope_with_local_allocation" Cols.[n]

let scope_with_local_allocation x = scope_with_local_allocation_tbl % [x]

let candidate_rules =
  saturate_in_order
    [ (let$ [a; s] = ["a"; "s"] in
       [allocated_in ~alloc:a ~scope:s] ==> and_ [is_candidate a; flow a a]) ]

let escape_rules =
  Schedule.fixpoint
    [ Schedule.saturate
        [ (let$ [a; x; y] = ["a"; "x"; "y"] in
           [flow a x; rev_alias ~from:x ~to_:y] ==> flow a y);
          (let$ [a; x] = ["a"; "x"] in
           [flow a x; any_usage x] ==> escapes a);
          (let$ [a; x; relation; base] = ["a"; "x"; "relation"; "base"] in
           [ flow a x;
             rev_constructor ~from:x relation ~base;
             ~~(is_candidate base) ]
           ==> escapes a);
          (let$ [a; x; relation; base] = ["a"; "x"; "relation"; "base"] in
           [flow a x; rev_constructor ~from:x relation ~base; escapes base]
           ==> escapes a);
          (let$ [a; s; x] = ["a"; "s"; "x"] in
           [ allocated_in ~alloc:a ~scope:s;
             flow a x;
             scope_exit ~scope:s ~param:x ]
           ==> escapes a);
          (let$ [a; s] = ["a"; "s"] in
           [allocated_in ~alloc:a ~scope:s; unsafe_scope s] ==> escapes a) ] ]

let decision_rules =
  saturate_in_order
    [ (let$ [a; s] = ["a"; "s"] in
       [allocated_in ~alloc:a ~scope:s; ~~(escapes a)]
       ==> and_ [local_allocation a; scope_with_local_allocation s]) ]

let scopes_of_allocation =
  let scopes =
    query
      (let^$ [a], [s] = ["a"], ["s"] in
       [allocated_in ~alloc:a ~scope:s] =>? [s])
  in
  fun db a ->
    Cursor.fold_with_parameters scopes [a] db ~init:[] ~f:(fun [s] acc ->
        s :: acc)

let perform_analysis db ~stats =
  Profile.record_call ~accumulate:true "escape_analysis" (fun () ->
      let run db rules =
        List.fold_left (fun db rule -> Schedule.run ~stats rule db) db rules
      in
      let db = run db candidate_rules in
      let db = Schedule.run ~stats escape_rules db in
      run db decision_rules)
