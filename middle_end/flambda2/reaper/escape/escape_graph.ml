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

(* The facts recorded by the traversal for the escape analysis (see
   [Escape_analysis]), kept alongside the [Global_flow_graph] and added to the
   Datalog database built from it. *)

module N = Datalog.Schema.Relation1 (Code_id_or_name)
module NN = Datalog.Schema.Relation2 (Code_id_or_name) (Code_id_or_name)
module NNN =
  Datalog.Schema.Relation3 (Code_id_or_name) (Code_id_or_name) (Code_id_or_name)

type t =
  { mutable allocated_in : NN.t;
    mutable scope_exit : NN.t;
    mutable unsafe_scope : N.t;
    mutable return_of : NN.t;
    mutable call_result : NNN.t;
    mutable unscoped_call_result : N.t;
    mutable indirect_call_result : N.t;
    mutable partial_application_node : NN.t;
    (* Not a Datalog relation: the continuations defined inside each region
       scope, for the traversal and the rebuilding. *)
    mutable scope_conts : Continuation.Set.t Code_id_or_name.Map.t
  }

let allocated_in = NN.create ~name:"allocated_in"

let scope_exit = NN.create ~name:"scope_exit"

let unsafe_scope = N.create ~name:"unsafe_scope"

let return_of = NN.create ~name:"return_of"

let call_result = NNN.create ~name:"call_result"

let unscoped_call_result = N.create ~name:"unscoped_call_result"

let indirect_call_result = N.create ~name:"indirect_call_result"

let partial_application_node = NN.create ~name:"partial_application_node"

(* Nothing is recorded when the analysis is disabled, so that it costs nothing
   then: the tables below stay empty. *)
let enabled =
  let flag = Oxcaml_args.Extra_options.bool __LOC__ "reaper-escape" in
  fun () -> flag () && Flambda_features.stack_allocation_enabled ()

let create () =
  { allocated_in = NN.empty;
    scope_exit = NN.empty;
    unsafe_scope = N.empty;
    return_of = NN.empty;
    call_result = NNN.empty;
    unscoped_call_result = N.empty;
    indirect_call_result = N.empty;
    partial_application_node = NN.empty;
    scope_conts = Code_id_or_name.Map.empty
  }

let to_datalog t db =
  Datalog.set_table allocated_in t.allocated_in
  @@ Datalog.set_table scope_exit t.scope_exit
  @@ Datalog.set_table unsafe_scope t.unsafe_scope
  @@ Datalog.set_table return_of t.return_of
  @@ Datalog.set_table call_result t.call_result
  @@ Datalog.set_table unscoped_call_result t.unscoped_call_result
  @@ Datalog.set_table indirect_call_result t.indirect_call_result
  @@ Datalog.set_table partial_application_node t.partial_application_node
  @@ db

module Relations = struct
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  let allocated_in ~alloc ~scope = Datalog.atom allocated_in [alloc; scope]

  let scope_exit ~scope ~param = Datalog.atom scope_exit [scope; param]

  let unsafe_scope scope = Datalog.atom unsafe_scope [scope]

  let return_of ~scope ~result = Datalog.atom return_of [scope; result]

  let call_result ~result ~scope ~call_widget =
    Datalog.atom call_result [result; scope; call_widget]

  let unscoped_call_result result = Datalog.atom unscoped_call_result [result]

  let indirect_call_result result = Datalog.atom indirect_call_result [result]

  let partial_application_node ~node ~code_id =
    Datalog.atom partial_application_node [node; code_id]
end

let add_allocated_in t ~alloc ~scope =
  if enabled ()
  then t.allocated_in <- NN.add_or_replace [alloc; scope] () t.allocated_in

let add_scope_exit t ~scope ~param =
  if enabled ()
  then
    t.scope_exit
      <- NN.add_or_replace [scope; Code_id_or_name.var param] () t.scope_exit

let add_unsafe_scope t scope =
  if enabled ()
  then t.unsafe_scope <- N.add_or_replace [scope] () t.unsafe_scope

let add_return_of t ~scope ~result =
  if enabled ()
  then
    t.return_of
      <- NN.add_or_replace [scope; Code_id_or_name.var result] () t.return_of

let add_call_result t ~result ~scope ~call_widget =
  if enabled ()
  then
    t.call_result
      <- NNN.add_or_replace
           [Code_id_or_name.var result; scope; call_widget]
           () t.call_result

let add_unscoped_call_result t result =
  if enabled ()
  then
    t.unscoped_call_result
      <- N.add_or_replace [Code_id_or_name.var result] () t.unscoped_call_result

let add_indirect_call_result t result =
  if enabled ()
  then
    t.indirect_call_result
      <- N.add_or_replace [Code_id_or_name.var result] () t.indirect_call_result

let add_partial_application_node t ~node ~code_id =
  if enabled ()
  then
    t.partial_application_node
      <- NN.add_or_replace
           [node; Code_id_or_name.code_id code_id]
           () t.partial_application_node

let add_scope_cont t ~scope cont =
  if enabled ()
  then
    t.scope_conts
      <- Code_id_or_name.Map.update scope
           (function
             | None -> Some (Continuation.Set.singleton cont)
             | Some conts -> Some (Continuation.Set.add cont conts))
           t.scope_conts

let scope_conts t = t.scope_conts

let scope_contains_continuation t ~scope cont =
  match Code_id_or_name.Map.find_opt scope t.scope_conts with
  | None -> false
  | Some conts -> Continuation.Set.mem cont conts
