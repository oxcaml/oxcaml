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

(* The facts recorded by the traversal for the escape analysis (see
   [Escape_analysis]), kept in the traversal accumulator alongside the
   [Global_flow_graph] and added to the solved Datalog database. *)

open Datalog_helpers.Schemas

type t =
  { enabled : bool;
    mutable allocated_in : NN.t;
    mutable scope_exit : NN.t;
    mutable unsafe_scope : N.t;
    mutable return_of : NN.t;
    mutable call_result : NN.t;
    mutable unscoped_call_result : N.t;
    mutable indirect_call_result : N.t;
    mutable scoped_indirect_call_result : NN.t;
    mutable unscoped_indirect_call_result : N.t;
    (* The remaining fields are not Datalog relations. *)
    mutable outer_conts : Continuation.Set.t Code_id_or_name.Map.t;
    mutable recursive_scopes : Variable.t Continuation.Map.t;
    (* The callees of the unknown-arity applications in tail position, with the
       greatest number of arguments they are applied to. *)
    mutable tail_callees : int Code_id_or_name.Map.t;
    (* The number of parameters of each function, as seen by the generic
       application functions. *)
    mutable code_arities : int Code_id.Map.t;
    mutable solved : Datalog.database option
  }

let allocated_in = NN.create ~name:"allocated_in"

let scope_exit = NN.create ~name:"scope_exit"

let unsafe_scope = N.create ~name:"unsafe_scope"

let return_of = NN.create ~name:"return_of"

let call_result = NN.create ~name:"call_result"

let unscoped_call_result = N.create ~name:"unscoped_call_result"

let indirect_call_result = N.create ~name:"indirect_call_result"

let scoped_indirect_call_result = NN.create ~name:"scoped_indirect_call_result"

let unscoped_indirect_call_result =
  N.create ~name:"unscoped_indirect_call_result"

let make ~enabled =
  { enabled;
    allocated_in = NN.empty;
    scope_exit = NN.empty;
    unsafe_scope = N.empty;
    return_of = NN.empty;
    call_result = NN.empty;
    unscoped_call_result = N.empty;
    indirect_call_result = N.empty;
    scoped_indirect_call_result = NN.empty;
    unscoped_indirect_call_result = N.empty;
    outer_conts = Code_id_or_name.Map.empty;
    recursive_scopes = Continuation.Map.empty;
    tail_callees = Code_id_or_name.Map.empty;
    code_arities = Code_id.Map.empty;
    solved = None
  }

let create () =
  make
    ~enabled:
      (Oxcaml_flags.Experimental.is_enabled
         Experimental_feature_registry.Feature_reaper_escape
      && Flambda_features.stack_allocation_enabled ())

(* Nothing is ever recorded in a disabled graph (see the [add_*] functions and
   [set_solved]), so this value is never mutated. *)
let disabled = make ~enabled:false

let enabled t = t.enabled

let to_datalog t db =
  Datalog.set_table allocated_in t.allocated_in
  @@ Datalog.set_table scope_exit t.scope_exit
  @@ Datalog.set_table unsafe_scope t.unsafe_scope
  @@ Datalog.set_table return_of t.return_of
  @@ Datalog.set_table call_result t.call_result
  @@ Datalog.set_table unscoped_call_result t.unscoped_call_result
  @@ Datalog.set_table indirect_call_result t.indirect_call_result
  @@ Datalog.set_table scoped_indirect_call_result t.scoped_indirect_call_result
  @@ Datalog.set_table unscoped_indirect_call_result
       t.unscoped_indirect_call_result
  @@ db

module Relations = struct
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  let allocated_in ~alloc ~scope = Datalog.atom allocated_in [alloc; scope]

  let scope_exit ~scope ~param = Datalog.atom scope_exit [scope; param]

  let unsafe_scope scope = Datalog.atom unsafe_scope [scope]

  let return_of ~scope ~result = Datalog.atom return_of [scope; result]

  let call_result ~result ~scope = Datalog.atom call_result [result; scope]

  let unscoped_call_result result = Datalog.atom unscoped_call_result [result]

  let indirect_call_result result = Datalog.atom indirect_call_result [result]

  let scoped_indirect_call_result ~result ~scope =
    Datalog.atom scoped_indirect_call_result [result; scope]

  let unscoped_indirect_call_result result =
    Datalog.atom unscoped_indirect_call_result [result]
end

let add_allocated_in t ~alloc ~scope =
  if t.enabled
  then t.allocated_in <- NN.add_or_replace [alloc; scope] () t.allocated_in

let add_scope_exit t ~scope ~param =
  if t.enabled
  then
    t.scope_exit
      <- NN.add_or_replace [scope; Code_id_or_name.var param] () t.scope_exit

let add_unsafe_scope t scope =
  if t.enabled then t.unsafe_scope <- N.add_or_replace [scope] () t.unsafe_scope

let add_return_of t ~scope ~result =
  if t.enabled
  then
    t.return_of
      <- NN.add_or_replace [scope; Code_id_or_name.var result] () t.return_of

let add_call_result t ~result ~scope =
  if t.enabled
  then
    t.call_result
      <- NN.add_or_replace [Code_id_or_name.var result; scope] () t.call_result

let add_unscoped_call_result t result =
  if t.enabled
  then
    t.unscoped_call_result
      <- N.add_or_replace [Code_id_or_name.var result] () t.unscoped_call_result

let add_indirect_call_result t result =
  if t.enabled
  then
    t.indirect_call_result
      <- N.add_or_replace [Code_id_or_name.var result] () t.indirect_call_result

let add_scoped_indirect_call_result t ~result ~scope =
  if t.enabled
  then
    t.scoped_indirect_call_result
      <- NN.add_or_replace
           [Code_id_or_name.var result; scope]
           () t.scoped_indirect_call_result

let add_unscoped_indirect_call_result t result =
  if t.enabled
  then
    t.unscoped_indirect_call_result
      <- N.add_or_replace
           [Code_id_or_name.var result]
           () t.unscoped_indirect_call_result

let add_scope t ~scope ~outer_conts =
  if t.enabled
  then t.outer_conts <- Code_id_or_name.Map.add scope outer_conts t.outer_conts

let scope_contains_continuation t ~scope cont =
  match Code_id_or_name.Map.find_opt scope t.outer_conts with
  | None ->
    Misc.fatal_errorf "[Escape_graph]: unknown region scope %a"
      Code_id_or_name.print scope
  | Some outer_conts -> not (Continuation.Set.mem cont outer_conts)

let add_recursive_scope t cont ~scope =
  if t.enabled
  then t.recursive_scopes <- Continuation.Map.add cont scope t.recursive_scopes

let recursive_scope t cont = Continuation.Map.find_opt cont t.recursive_scopes

let add_tail_callee t callee ~num_args =
  if t.enabled
  then
    t.tail_callees
      <- Code_id_or_name.Map.update
           (Code_id_or_name.var callee)
           (fun n -> Some (max num_args (Option.value n ~default:0)))
           t.tail_callees

let tail_callees t = t.tail_callees

let add_code_arity t code_id ~num_params =
  if t.enabled
  then t.code_arities <- Code_id.Map.add code_id num_params t.code_arities

let code_arity t code_id = Code_id.Map.find_opt code_id t.code_arities

let set_solved t db = if t.enabled then t.solved <- Some db

let solved t = t.solved
