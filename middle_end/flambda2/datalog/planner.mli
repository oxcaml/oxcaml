(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

open Datalog_imports
open Lang

type 'k column_iterator =
  | Column_iterator :
      ('t, 'k, 's) Column.id
      * 't variable
      * 's variable
      * ('s, _, 'v) Column.hlist
      * 'v Table.result_repr
      -> 'k column_iterator

type check_atom =
  | Distinct : (_, 'k, _) Column.id * 'k term * 'k term -> check_atom
  | Unless :
      ('t, 'k, 'v) Column.hlist * 't variable * 'k Term.hlist
      -> check_atom
  | Filter : ('k Constant.hlist -> bool) with_name * 'k Term.hlist -> check_atom

type stage =
  | Join_stage : 'k variable * 'k column_iterator list -> stage
  | Seek_stage : 'k term * 'k column_iterator list -> stage
  | Check_stage : check_atom -> stage
  | Existential_stage : stage iarray -> stage
(* Variables bound by existential stages are not accessible in the output, which
   allows skipping the full existential stage once we have found any satisfying
   instance. *)

type (_, _) union =
  | Union :
      ('t, 'k, 's) Column.hlist
      * 'k Term.hlist
      * 's term
      * ('s, _, 'v) Column.hlist
      * 'v Table.result_repr
      -> ('t, 'v) union

type head_atom =
  | Output : 't variable * ('t, _) union -> head_atom
  | Callback_with_bindings :
      (Bytecode.bindings_ref -> 'k Constant.hlist -> unit) with_name
      * 'k Term.hlist
      -> head_atom

type rule_plan =
  { rule_id : int;
        (** Index in the list of rules provided to [plan_incremental], or [0]
            for [plan_rule]. *)
    skip_if_empty : Variable.t_ list;
    input_stages : stage iarray;
    output_atoms : head_atom iarray
  }

module Variable_map : Table.Id.Map.S with type 'a elt = 'a variable

module Variable_and_plans_map :
  Table.Id.Map.S with type 'a elt = 'a variable * rule_plan list

type plan =
  { rule_plan : rule_plan;
    inputs : Variable_map.t;
    outputs : Variable_map.t
  }

val print_rule_plan : Format.formatter -> rule_plan -> unit

val print_plan : Format.formatter -> plan -> unit

val plan_rule : rule -> plan

type incremental_plan =
  { initial_plans : rule_plan list;
        (** Non-incremental version of the rules. *)
    incremental_plans : Variable_and_plans_map.t;
        (** For each table appearing as a monotone input of any rule, the
            variable representing the incremental update ("diff") for that table
            and the associated rules. *)
    current_inputs : Variable_map.t;
        (** For each table appearing as (monotone or non-monotone) input of any
            rule, the variable representing its "current" state. *)
    previous_inputs : Variable_map.t;
        (** For each table appearing as a monotone input of any rule, the
            variable representing its "previous" state. *)
    outputs : Variable_map.t
        (** For each table appearing as output of a rule, the variable
            representing the facts learned by the plan. *)
  }

val plan_incremental : rule list -> incremental_plan
