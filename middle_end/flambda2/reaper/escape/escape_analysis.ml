(******************************************************************************
 *                                  OxCaml                                    *
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

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

(* An allocation [a] (see [allocated_in]) may be stack-allocated in a region
   spanning its region scope (the body of the enclosing function, or the handler
   of the enclosing recursive continuation) if the allocated value can never be
   reached once control leaves that scope, and is never stored in a block that
   lives on the heap. We follow the allocated value forwards through the [alias]
   relation computed by the points-to analysis (which already accounts for flow
   through function arguments, results and known block fields), see
   [candidate_source], and declare that it escapes if it reaches:

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
   before the call to keep it a tail call (see [Escape_traverse.record_apply]).

   Besides, nothing is done to close regions on exceptional paths: as for the
   regions introduced by the type checker, the handler catching the exception is
   expected to restore the local stack pointer. Handlers of try-with have a
   try-region of their own for that purpose, which they close on entry (and
   which Simplify never removes): this frees everything allocated since the
   [try] started, so values passed to such a handler are treated as leaving the
   scope (see [Escape_traverse]). This also frees the region of a scope left by
   the exception, provided the [try] started outside of that scope. We do not
   track where it started, so a scope from which an exception may reach a
   handler defined outside of it (other than the exception continuation of the
   function, whose caller is responsible for the handling) is conservatively
   marked [unsafe_scope] and gets no local allocations (see [Escape_traverse]):
   had the [try] started inside the scope (as when the handler has been lifted
   out of it), the region would be left open on that path, and an enclosing loop
   could accumulate such regions.

   Finally, since an allocation always goes to the innermost region open at
   runtime, no allocation is a candidate while a region pre-existing in the term
   (as introduced by the type checker for [local_] allocations, and closed
   before the enclosing function returns) is open: see
   [Escape_env.enter_foreign_region].

   Values that flow into a callee and come back through its result are tracked
   by the aliases created by the points-to analysis for calls, so no special
   treatment is needed for calls.

   A value reaching the return of the function containing its allocation does
   not necessarily escape: if all call sites of that function are known direct
   calls (see [cannot_change_calling_convention]), the function may be made to
   allocate its result in its caller's region, as [exclave_] does. The value
   then has to obey the rules of the scope of each call site, which is expressed
   by the [owner] relation. Such a function cannot have a region spanning its
   body. It gets one for its other local allocations when a place can be found
   to close it before anything returned is allocated, as [exclave_] does (see
   [Escape_exclave]); otherwise all its local allocations live until the region
   of its caller is closed. The latter must not happen for a function that may
   tail call itself, directly or through other functions: every iteration would
   leave its allocations behind, in a loop expected to run in constant space.
   Hence the functions on a cycle of calls in tail position are never made to
   return at mode local (see [cannot_return_local]). Functions for which this
   happens are recorded in [local_return]: their calling convention is changed
   so that they return at mode local (see [Escape_solution.adjust_code_metadata]
   for the code metadata, and [Escape_rebuild] for the function bodies and call
   sites).

   A dynamic partial application (an unknown-arity call with fewer arguments
   than the arity of the callee) allocates, in the curry functions, a closure
   pointing at the callee: on the heap, or in the region that is current at the
   call site, according to the code metadata of the callee (see
   [Lambda.curried_function_kind]). A heap block must never point at the local
   stack, so the code of a local closure is given the metadata the type checker
   gives its own local closures ([nlocal] equal to the arity, see
   [Escape_solution.adjust_code_metadata]), which makes every partial
   application of that code a local allocation, in the region current at the
   call site. Such a partial application is therefore treated as a candidate
   allocation in the region scope of the call site (see [allocation]),
   containing the closure. Allocated in the region current while the closure is
   alive, it cannot outlive the closure unless it leaves that region, which the
   rules for allocations prevent; and if it escapes, so does the closure, whose
   code then keeps allocating its partial applications on the heap. The call
   witnesses of the points-to analysis being per code, this concerns the partial
   applications of every closure of the code; one made outside of any region
   scope makes the closure escape.

   A partial application also holds the arguments it was given (as does the type
   checker, for which a partial application closes over its arguments): a value
   flowing into a parameter of the code that precedes the partial application is
   contained in it, and escapes if the partial application is allocated on the
   heap (see [partial_argument_holder]).

   This requires every partial application of every closure of the code to be
   known. Several closures may share one code (for instance after the function
   allocating them has been inlined); if any of them may be used by unknown code
   ([any_usage]), which could partially apply it and treat the result as a heap
   value, no closure of that code is made local (see
   [code_with_unknown_closure]).

   Finally, an application of several arguments to a closure expecting more goes
   through the generic application functions ([caml_applyN]), which, for a call
   returning at mode global, wrap the successive applications in a region of
   their own. A partial application allocated locally would not survive it, so
   the applications whose result is such a partial application are given a local
   return mode, as the type checker does (see [Escape_rebuild]). *)

[@@@ocaml.warning "-not-principal"]

open Global_flow_graph.Relations
open Points_to_analysis.Relations
open Escape_graph.Relations
open! Datalog_helpers.Syntax
open Datalog_helpers

(* Relations computed by [Unboxing_analysis]. *)
let cannot_change_calling_convention =
  Unboxing_analysis.cannot_change_calling_convention_relation

let to_unbox = Unboxing_analysis.to_unbox

(* [partial_application_node q c]: [q] stands for the closure resulting from a
   partial application of a function with code [c]; such a closure points at the
   closure being applied. These are the nodes created by
   [Traverse_acc.create_unknown_arity_call_witnesses] for the functions taking
   their parameters in several groups: the result of each call witness but the
   last, which itself has a call witness for the next group of parameters. They
   are the only nodes that are both constructed with a call witness and the
   result of a call witness, since the results of the last call witness are the
   return variables of the code, which are not constructed. *)
let partial_application_node = rel2 "escape_partial_application_node" Cols.[n; n]

(* [call_result_of_code q s' c]: [q] receives a return value of a known-arity
   application taking place in region scope [s'], whose callee may have code
   [c]. The application is modelled by a call widget from which [q] is read (see
   [Traverse_acc.make_known_arity_apply_widget]), whose sources are the call
   witnesses of the functions that may be called. *)
let call_result_of_code = rel3 "escape_call_result_of_code" Cols.[n; n; n]

(* [partial_holds q p]: the partial application [q] (see
   [partial_application_node]) holds the arguments given for the parameter [p]
   of its code: those of the group of parameters applied to obtain [q], and of
   the preceding ones, since [q] points at the partial application it was
   obtained from. *)
let partial_holds = rel2 "escape_partial_holds" Cols.[n; n]

(* [partial_argument_holder p r]: a value flowing into the parameter [p] may be
   held by the partial application resulting from the unknown-arity application
   whose result is [r]. *)
let partial_argument_holder = rel2 "escape_partial_argument_holder" Cols.[n; n]

(* [code_with_unknown_closure c]: some closure with code [c] may be used by
   unknown code. *)
let code_with_unknown_closure = rel1 "escape_code_with_unknown_closure" Cols.[n]

(* [code_has_heap_closure c]: some closure with code [c] is allocated on the
   heap. The curry functions allocate the partial applications of a closure
   according to the metadata of its code, which makes them local only if every
   closure of the code is local: otherwise, all the closures of the code and all
   their partial applications stay on the heap. A partial application treated as
   local while it is allocated on the heap could point at the local stack
   through the arguments it holds; one treated as heap-allocated while it is
   allocated locally could outlive its region. *)
let code_has_heap_closure = rel1 "escape_code_has_heap_closure" Cols.[n]

(* [tail_over_applied_code c]: a candidate closure with code [c] may be the
   callee of an unknown-arity application in tail position with more arguments
   than the function has parameters, the function having several parameters.
   Such an application goes through the generic application functions
   ([caml_applyN]), which first apply the closure to its first argument, in a
   region of their own: were the resulting partial application allocated
   locally, they could no longer tail call the last function applied (see
   [Cmm_helpers.apply_function_body]). The closures of such a code are thus kept
   on the heap. Computed outside of Datalog, from the numbers of arguments and
   parameters (see [add_tail_over_applications]). *)
let tail_over_applied_code_tbl =
  Schemas.N.create ~name:"escape_tail_over_applied_code"

let tail_over_applied_code c = Datalog.atom tail_over_applied_code_tbl [c]

let tail_callee_tbl = Schemas.N.create ~name:"escape_tail_callee"

let tail_callee v = Datalog.atom tail_callee_tbl [v]

(* [tail_edge f c]: the function [f] contains a known-arity application in tail
   position (an application whose result is the result of [f]; this includes the
   ones marked [@nontail]) whose callee may have code [c]. Only the functions
   whose calling convention may be changed are considered: the others never
   return at mode local anyway. *)
let tail_edge = rel2 "escape_tail_edge" Cols.[n; n]

(* [on_tail_cycle f]: the function [f] is on a cycle of [tail_edge]s. This is
   computed outside of Datalog (see [add_tail_cycles]), in time linear in the
   number of edges: a transitive closure would be quadratic on long chains of
   tail calls. *)
let on_tail_cycle_tbl = Schemas.N.create ~name:"escape_on_tail_cycle"

let on_tail_cycle f = Datalog.atom on_tail_cycle_tbl [f]

(* [cannot_return_local s]: the function [s] cannot be made to return at mode
   local: either its calling convention cannot be changed, or it is on a cycle
   of calls in tail position (see the preamble).

   A cycle of tail calls going through an application we do not see as an edge
   (of unknown arity, or with an unknown callee) needs no special treatment: a
   value returned at mode local by a function of the cycle would be returned by
   all of them, hence through that application, which makes it escape. *)
let cannot_return_local = rel1 "escape_cannot_return_local" Cols.[n]

let is_normal_return_of_call field =
  match Field.view field with
  | Return_of_call (Normal _) -> true
  | Return_of_call Exn
  | Block _ | Value_slot _ | Function_slot _ | Call_witness _ | Is_int | Get_tag
  | Boxed_number _ | Code_id_of_call_witness ->
    false

(* [candidate_source x a]: the value of [x] may be the value allocated at [a],
   where [a] is a candidate not already known to escape (see [candidate_rules]).
   This is the restriction to such candidates of the [sources] relation of the
   points-to analysis, except that it is not cut off at nodes with [any_source].
   The points-to analysis does not record the sources of such nodes, nor
   propagate them any further, since it cannot change their representation
   anyway; but a value joined with unknown values (for instance in a parameter
   of a function that is also called with the result of an exported function)
   may still never leave the scope of its allocation. Reusing [sources], and
   treating as escaping any value reaching a node with [any_source], was
   measured to lose about 40% of the stack allocations in the stdlib.

   The reached node comes first so that the rules below can be joined on it: the
   variable order of a rule (the order in which its variables are listed) is the
   nesting order of its loops, and a relation is only iterated on a variable
   once its preceding columns are bound. *)
let candidate_source = rel2 "escape_candidate_source" Cols.[n; n]

let candidate_tbl = Datalog.create_relation ~name:"escape_candidate" Cols.[n]

let is_candidate x = candidate_tbl % [x]

let escapes = rel1 "escapes" Cols.[n]

(* [escapes_anywhere a]: the value escapes for a reason independent of the
   region scopes (it may be used by unknown code, or stored in a heap block). *)
let escapes_anywhere = rel1 "escapes_anywhere" Cols.[n]

(* [owner a s]: the value allocated at [a] may be live in region scope [s]: [s]
   is the scope of the allocation, the scope of a call site receiving the value
   as the result of a function returning at mode local, or a scope owning a
   candidate block containing the value. *)
let owner = rel2 "escape_owner" Cols.[n; n]

(* [in_region a s]: the value allocated at [a] may be allocated in the region of
   the region scope [s]: [s] is the scope of the allocation, or the scope of a
   call site that receives the value, or a block containing it, as the result of
   a function returning at mode local ([delivered a s]). Unlike [owner], this
   does not hold for a value that is merely stored in a block allocated in [s]:
   such a value was allocated before the block, possibly in the region of an
   enclosing scope or of a caller, and may then leave [s]. *)
let in_region = rel2 "escape_in_region" Cols.[n; n]

let delivered = rel2 "escape_delivered" Cols.[n; n]

(* [contained_in a b]: the value allocated at [a] may be stored in a field of
   the candidate block allocated at [b]. *)
let contained_in = rel2 "escape_contained_in" Cols.[n; n]

(* [result_of a s]: the value allocated at [a] may be a result of the function
   [s]; [returned a s]: the same, or a candidate block containing the value may
   be. *)
let result_of = rel2 "escape_result_of" Cols.[n; n]

let returned = rel2 "escape_returned" Cols.[n; n]

(* [reaches_indirect_call_result a] (resp. [reaches_unscoped_call_result a]):
   the value allocated at [a] may be the result of an unknown-arity application
   (resp. of an application outside of any region scope). *)
let reaches_indirect_call_result =
  rel1 "escape_reaches_indirect_call_result" Cols.[n]

let reaches_unscoped_call_result =
  rel1 "escape_reaches_unscoped_call_result" Cols.[n]

(* [partial_result_of_code c r]: [r] is the result of an unknown-arity
   application with fewer arguments than the arity of its callee, which may have
   code [c]: a partial application of that code, allocated by the curry
   functions (see the preamble). *)
let partial_result_of_code = rel2 "escape_partial_result_of_code" Cols.[n; n]

(* [candidate_code c]: [c] is the code of a candidate closure. *)
let candidate_code = rel1 "escape_candidate_code" Cols.[n]

(* [allocation a s]: the candidate [a] is allocated in region scope [s]: an
   allocation recorded by the traversal ([allocated_in]), or a partial
   application of a candidate code at a call site in [s]. *)
let allocation = rel2 "escape_allocation" Cols.[n; n]

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
    [ (let$ [q; next_wit; wit; c] = ["q"; "next_wit"; "wit"; "c"] in
       [ constructor ~base:q !!Field.unknown_arity_call_witness ~from:next_wit;
         rev_constructor ~from:q !!(Field.normal_return_of_call 0) ~base:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       ==> partial_application_node q c);
      (let$ [q; s'; relation; w; wit; c] =
         ["q"; "s'"; "relation"; "w"; "wit"; "c"]
       in
       [ call_result ~result:q ~scope:s';
         accessor ~to_:q relation ~base:w;
         when1 is_normal_return_of_call relation;
         sources w wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       ==> call_result_of_code q s' c);
      (let$ [q; c; wit; cofield; p] = ["q"; "c"; "wit"; "cofield"; "p"] in
       [ partial_application_node q c;
         rev_constructor ~from:q !!(Field.normal_return_of_call 0) ~base:wit;
         parameter ~base:wit cofield ~to_:p ]
       ==> partial_holds q p);
      (let$ [q'; wit; q; c; p] = ["q'"; "wit"; "q"; "c"; "p"] in
       [ partial_holds q' p;
         constructor ~base:q' !!Field.unknown_arity_call_witness ~from:wit;
         constructor ~base:wit !!(Field.normal_return_of_call 0) ~from:q;
         partial_application_node q c ]
       ==> partial_holds q p);
      (let$ [q; r; p] = ["q"; "r"; "p"] in
       [partial_holds q p; rev_alias ~from:q ~to_:r; indirect_call_result r]
       ==> partial_argument_holder p r);
      (let$ [y; wit; c] = ["y"; "wit"; "c"] in
       [ any_usage y;
         constructor ~base:y !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       ==> code_with_unknown_closure c);
      (let$ [s] = ["s"] in
       [cannot_change_calling_convention s] ==> cannot_return_local s);
      (let$ [f; q; s'; c] = ["f"; "q"; "s'"; "c"] in
       [ return_of ~scope:f ~result:q;
         ~~(cannot_change_calling_convention f);
         call_result_of_code q s' c;
         ~~(cannot_change_calling_convention c) ]
       ==> tail_edge f c);
      (* The result of an unknown-arity application is aliased to a partial
         application node of the code of its callee exactly when fewer arguments
         than the arity are supplied. *)
      (let$ [q; c; r] = ["q"; "c"; "r"] in
       [ partial_application_node q c;
         rev_alias ~from:q ~to_:r;
         indirect_call_result r ]
       ==> partial_result_of_code c r);
      (let$ [x; s; wit; c] = ["x"; "s"; "wit"; "c"] in
       [ allocated_in ~alloc:x ~scope:s;
         constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       ==> candidate_code c);
      (let$ [a; s] = ["a"; "s"] in
       [allocated_in ~alloc:a ~scope:s] ==> allocation a s);
      (let$ [c; r; s] = ["c"; "r"; "s"] in
       [ candidate_code c;
         partial_result_of_code c r;
         scoped_indirect_call_result ~result:r ~scope:s ]
       ==> allocation r s);
      (let$ [a; s] = ["a"; "s"] in
       [allocation a s] ==> is_candidate a);
      (* [any_usage] is already propagated backwards along aliases by the
         points-to analysis. Rule out these candidates before following their
         flows, which can be quadratic even when all the allocations escape. *)
      (let$ [a] = ["a"] in
       [is_candidate a; any_usage a] ==> escapes_anywhere a);
      (* Partially applied outside of any region scope: the partial application
         would be allocated in an unknown region (see the preamble). *)
      (let$ [x; wit; c; r] = ["x"; "wit"; "c"; "r"] in
       [ is_candidate x;
         constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c;
         partial_result_of_code c r;
         unscoped_indirect_call_result r ]
       ==> escapes_anywhere x);
      (* Another closure of the same code may be partially applied by unknown
         code (see the preamble). *)
      (let$ [x; wit; c] = ["x"; "wit"; "c"] in
       [ is_candidate x;
         constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c;
         code_with_unknown_closure c ]
       ==> escapes_anywhere x);
      (* Keep [is_candidate] for escaping allocations: values stored in them
         must still escape via [contained_in]. Only prune the flow seeds, before
         [flow_rules] extends [escapes_anywhere] with further reasons. *)
      (let$ [a] = ["a"] in
       [is_candidate a; ~~(escapes_anywhere a)] ==> candidate_source a a);
      (* A partial application points at the closure applied. *)
      (let$ [x; wit; c; r] = ["x"; "wit"; "c"; "r"] in
       [ is_candidate x;
         constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c;
         partial_result_of_code c r;
         is_candidate r ]
       ==> contained_in x r) ]

let closure_code_rules =
  saturate_in_order
    [ (let$ [x; wit; c] = ["x"; "wit"; "c"] in
       [ constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c;
         ~~(is_candidate x) ]
       ==> code_has_heap_closure c) ]

(* When calling conventions may not be changed, no function may be made to
   return at mode local. *)
let no_calling_convention_change_rules =
  saturate_in_order
    [ (let$ [s; r] = ["s"; "r"] in
       [return_of ~scope:s ~result:r] ==> cannot_return_local s) ]

module SCC = Strongly_connected_components.Make (Code_id_or_name)

let add_tail_cycles =
  let tail_edges =
    query
      (let$ [f; c] = ["f"; "c"] in
       [tail_edge f c] =>? [f; c])
  in
  fun db ->
    let add_node node graph =
      if Code_id_or_name.Map.mem node graph
      then graph
      else Code_id_or_name.Map.add node Code_id_or_name.Set.empty graph
    in
    let graph =
      Cursor.fold tail_edges db ~init:Code_id_or_name.Map.empty
        ~f:(fun [f; c] graph ->
          Code_id_or_name.Map.update f
            (fun callees ->
              let callees =
                Option.value callees ~default:Code_id_or_name.Set.empty
              in
              Some (Code_id_or_name.Set.add c callees))
            (add_node c graph))
    in
    let on_tail_cycle =
      Array.fold_left
        (fun on_tail_cycle (component : SCC.component) ->
          match component with
          | No_loop _ -> on_tail_cycle
          | Has_loop nodes ->
            List.fold_left
              (fun on_tail_cycle node ->
                Schemas.N.add_or_replace [node] () on_tail_cycle)
              on_tail_cycle nodes)
        Schemas.N.empty
        (SCC.connected_components_sorted_from_roots_to_leaf graph)
    in
    Datalog.set_table on_tail_cycle_tbl on_tail_cycle db

let tail_cycle_rules =
  saturate_in_order
    [ (let$ [f] = ["f"] in
       [on_tail_cycle f] ==> cannot_return_local f) ]

(* First stratum: where the allocated values flow, and the reasons for escaping
   that do not depend on region scopes. *)
let flow_rules =
  Schedule.fixpoint
    [ Schedule.saturate
        [ (let$ [x; y; a] = ["x"; "y"; "a"] in
           [candidate_source x a; rev_alias ~from:x ~to_:y]
           ==> candidate_source y a);
          (let$ [x; relation; base; a] = ["x"; "relation"; "base"; "a"] in
           [ candidate_source x a;
             rev_constructor ~from:x relation ~base;
             when1 Field.is_real_field relation;
             is_candidate base ]
           ==> contained_in a base);
          (* Stored in a block that is not a candidate. *)
          (let$ [x; relation; base; a] = ["x"; "relation"; "base"; "a"] in
           [ candidate_source x a;
             rev_constructor ~from:x relation ~base;
             when1 Field.is_real_field relation;
             ~~(is_candidate base) ]
           ==> escapes_anywhere a);
          (* Given as argument to a partial application (see the preamble). *)
          (let$ [p; r; a] = ["p"; "r"; "a"] in
           [candidate_source p a; partial_argument_holder p r; is_candidate r]
           ==> contained_in a r);
          (let$ [p; r; a] = ["p"; "r"; "a"] in
           [ candidate_source p a;
             partial_argument_holder p r;
             ~~(is_candidate r) ]
           ==> escapes_anywhere a);
          (* A partial application is itself the result of such an application
             (see [allocation]); the application producing it does not return
             it. *)
          (let$ [q; a] = ["q"; "a"] in
           [ indirect_call_result q;
             candidate_source q a;
             Datalog.filter
               (fun [q; a] -> not (Code_id_or_name.equal q a))
               [q; a] ]
           ==> reaches_indirect_call_result a);
          (let$ [q; a] = ["q"; "a"] in
           [unscoped_call_result q; candidate_source q a]
           ==> reaches_unscoped_call_result a);
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
           [allocation a s; ~~(escapes_anywhere a)]
           ==> and_ [owner a s; in_region a s]);
          (let$ [a; b; s] = ["a"; "b"; "s"] in
           [contained_in a b; delivered b s; ~~(escapes_anywhere a)]
           ==> and_ [delivered a s; in_region a s]);
          (let$ [a; b; s] = ["a"; "b"; "s"] in
           [contained_in a b; owner b s; ~~(escapes_anywhere a)] ==> owner a s);
          (let$ [s; r; a] = ["s"; "r"; "a"] in
           [ return_of ~scope:s ~result:r;
             candidate_source r a;
             ~~(escapes_anywhere a) ]
           ==> and_ [result_of a s; returned a s]);
          (let$ [a; b; s] = ["a"; "b"; "s"] in
           [contained_in a b; returned b s; ~~(escapes_anywhere a)]
           ==> returned a s);
          (let$ [a; b] = ["a"; "b"] in
           [contained_in a b; escapes b] ==> escapes a);
          (* See [code_has_heap_closure]. *)
          (let$ [c] = ["c"] in
           [tail_over_applied_code c] ==> code_has_heap_closure c);
          (let$ [x; wit; c] = ["x"; "wit"; "c"] in
           [ escapes x;
             constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
             constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
           ==> code_has_heap_closure c);
          (let$ [x; wit; c] = ["x"; "wit"; "c"] in
           [ is_candidate x;
             constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
             constructor ~base:wit !!Field.code_id_of_call_witness ~from:c;
             code_has_heap_closure c ]
           ==> escapes x);
          (let$ [c; r] = ["c"; "r"] in
           [code_has_heap_closure c; partial_result_of_code c r] ==> escapes r);
          (* Leaving a scope in whose region the value may be allocated. Driven
             by [in_region], which is small: the scope exits are then looked up
             in [candidate_source]. *)
          (let$ [a; s; x] = ["a"; "s"; "x"] in
           [in_region a s; scope_exit ~scope:s ~param:x; candidate_source x a]
           ==> escapes a);
          (let$ [a; s] = ["a"; "s"] in
           [owner a s; unsafe_scope s] ==> escapes a);
          (* Returned from a function that cannot return at mode local. *)
          (let$ [a; s] = ["a"; "s"] in
           [result_of a s; owner a s; cannot_return_local s] ==> escapes a);
          (* Returned to a call site that cannot host a local result. *)
          (let$ [a; s] = ["a"; "s"] in
           [result_of a s; owner a s; reaches_indirect_call_result a]
           ==> escapes a);
          (let$ [a; s] = ["a"; "s"] in
           [result_of a s; owner a s; reaches_unscoped_call_result a]
           ==> escapes a);
          (* Returned to a call site in scope [s']: the value is then also owned
             by [s']. *)
          (let$ [q; s'; s; a] = ["q"; "s'"; "s"; "a"] in
           [ call_result_of_code q s' s;
             ~~(cannot_return_local s);
             candidate_source q a;
             result_of a s;
             owner a s ]
           ==> and_ [owner a s'; delivered a s'; in_region a s']) ] ]

let decision_rules =
  saturate_in_order
    [ (let$ [a; s] = ["a"; "s"] in
       [allocation a s; ~~(escapes a)]
       ==> and_ [local_allocation a; scope_with_local_allocation s]);
      (* Unboxed allocations no longer exist, so need no local return (but the
         local allocations they contain may). *)
      (let$ [a; s] = ["a"; "s"] in
       [local_allocation a; owner a s; returned a s; ~~(to_unbox a)]
       ==> local_return s);
      (let$ [x; wit; c] = ["x"; "wit"; "c"] in
       [ local_allocation x;
         constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       ==> local_closure_code c);
      (let$ [q; s'; c] = ["q"; "s'"; "c"] in
       [call_result_of_code q s' c; local_return c]
       ==> scope_with_local_call_result s');
      (* A partial application made local is such a result too. *)
      (let$ [r; s] = ["r"; "s"] in
       [local_allocation r; indirect_call_result r; allocation r s]
       ==> scope_with_local_call_result s) ]

let scopes_of_allocation =
  let scopes =
    query
      (let^$ [a], [s] = ["a"], ["s"] in
       [allocation a s] =>? [s])
  in
  fun db a ->
    Cursor.fold_with_parameters scopes [a] db ~init:[] ~f:(fun [s] acc ->
        s :: acc)

let add_tail_over_applications =
  let callees =
    query
      (let$ [v; x; wit; c] = ["v"; "x"; "wit"; "c"] in
       [ tail_callee v;
         candidate_source v x;
         constructor ~base:x !!Field.known_arity_call_witness ~from:wit;
         constructor ~base:wit !!Field.code_id_of_call_witness ~from:c ]
       =>? [v; c])
  in
  fun graph db ->
    let tail_callees = Escape_graph.tail_callees graph in
    let db =
      Datalog.set_table tail_callee_tbl
        (Code_id_or_name.Map.fold
           (fun callee _ table -> Schemas.N.add_or_replace [callee] () table)
           tail_callees Schemas.N.empty)
        db
    in
    let codes =
      Cursor.fold callees db ~init:Schemas.N.empty ~f:(fun [v; c] codes ->
          let num_args = Code_id_or_name.Map.find v tail_callees in
          let over_applied =
            Code_id_or_name.pattern_match c
              ~code_id:(fun code_id ->
                match Escape_graph.code_arity graph code_id with
                | None -> true
                | Some num_params -> num_params >= 2 && num_params < num_args)
              ~symbol:(fun _ -> true)
              ~var:(fun _ -> true)
          in
          if over_applied then Schemas.N.add_or_replace [c] () codes else codes)
    in
    Datalog.set_table tail_over_applied_code_tbl codes db

let local_sources =
  let sources =
    query
      (let^$ [x], [a] = ["x"], ["a"] in
       [candidate_source x a; local_allocation a] =>? [a])
  in
  fun db x ->
    Cursor.fold_with_parameters sources [x] db ~init:[] ~f:(fun [a] acc ->
        a :: acc)

let fold_pairs q db =
  Cursor.fold q db ~init:Code_id_or_name.Map.empty ~f:(fun [key; a] acc ->
      Code_id_or_name.Map.update key
        (fun set ->
          let set = Option.value set ~default:Code_id_or_name.Set.empty in
          Some (Code_id_or_name.Set.add a set))
        acc)

let local_returned =
  fold_pairs
    (query
       (let$ [a; s] = ["a"; "s"] in
        [local_allocation a; returned a s] =>? [s; a]))

let local_contents =
  fold_pairs
    (query
       (let$ [a; b] = ["a"; "b"] in
        [local_allocation a; contained_in a b] =>? [b; a]))

let perform_analysis graph (unboxing : Unboxing_analysis.result) =
  if Escape_graph.enabled graph
  then
    Profile.record_call ~accumulate:true "escape_analysis" (fun () ->
        let db = Escape_graph.to_datalog graph unboxing.db in
        let stats =
          if Flambda_features.debug_reaper "stats"
          then Some (Datalog.Schedule.create_stats db)
          else None
        in
        let run db rules =
          List.fold_left (fun db rule -> Schedule.run ?stats rule db) db rules
        in
        let db = run db candidate_rules in
        let db =
          if Flambda_features.reaper_change_calling_conventions ()
          then run (add_tail_cycles db) tail_cycle_rules
          else run db no_calling_convention_change_rules
        in
        let db = run db closure_code_rules in
        let db = Schedule.run ?stats flow_rules db in
        let db = add_tail_over_applications graph db in
        let db = Schedule.run ?stats scope_rules db in
        let db = run db decision_rules in
        Option.iter (Format.eprintf "%a@." Datalog.Schedule.print_stats) stats;
        if Flambda_features.debug_reaper "escape-db"
        then Format.eprintf "%a@." Datalog.print db;
        Escape_graph.set_solved graph db)
