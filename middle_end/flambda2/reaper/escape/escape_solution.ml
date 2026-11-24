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

type t =
  { candidates : unit Code_id_or_name.Map.t;
    local_allocations : unit Code_id_or_name.Map.t;
    scopes_needing_region : unit Code_id_or_name.Map.t;
    local_returns : unit Code_id_or_name.Map.t;
    local_closure_code : unit Code_id_or_name.Map.t;
    scopes_with_local_call_result : unit Code_id_or_name.Map.t;
    db : Datalog.database;
    is_unboxed : Code_id_or_name.t -> bool;
    local_returned : Code_id_or_name.Set.t Code_id_or_name.Map.t Lazy.t;
    local_contents : Code_id_or_name.Set.t Code_id_or_name.Map.t Lazy.t
  }

let create db ~is_unboxed =
  let local_allocations =
    Datalog.get_table Escape_analysis.local_allocation_tbl db
  in
  (* A region is only needed in scopes where a local allocation survives
     rebuilding, that is, is not unboxed, or where a function returning at mode
     local is called. *)
  let scopes_needing_region =
    Code_id_or_name.Map.fold
      (fun alloc () acc ->
        if is_unboxed alloc
        then acc
        else
          List.fold_left
            (fun acc scope -> Code_id_or_name.Map.add scope () acc)
            acc
            (Escape_analysis.scopes_of_allocation db alloc))
      local_allocations
      (Datalog.get_table Escape_analysis.scope_with_local_call_result_tbl db)
  in
  { candidates = Datalog.get_table Escape_analysis.candidate_tbl db;
    local_allocations;
    scopes_needing_region;
    scopes_with_local_call_result =
      Datalog.get_table Escape_analysis.scope_with_local_call_result_tbl db;
    local_returns = Datalog.get_table Escape_analysis.local_return_tbl db;
    local_closure_code =
      Datalog.get_table Escape_analysis.local_closure_code_tbl db;
    db;
    is_unboxed;
    local_returned = lazy (Escape_analysis.local_returned db);
    local_contents = lazy (Escape_analysis.local_contents db)
  }

let is_candidate t var =
  Code_id_or_name.Map.mem (Code_id_or_name.var var) t.candidates

let is_local_allocation t var =
  Code_id_or_name.Map.mem (Code_id_or_name.var var) t.local_allocations

let scope_needs_region t scope =
  Code_id_or_name.Map.mem scope t.scopes_needing_region

let scope_has_local_call_result t scope =
  Code_id_or_name.Map.mem scope t.scopes_with_local_call_result

let returns_local t code_id =
  Code_id_or_name.Map.mem (Code_id_or_name.code_id code_id) t.local_returns

let is_unboxed t alloc = t.is_unboxed alloc

let local_sources t var =
  Escape_analysis.local_sources t.db (Code_id_or_name.var var)

let find_set map key =
  match Code_id_or_name.Map.find_opt key (Lazy.force map) with
  | Some set -> set
  | None -> Code_id_or_name.Set.empty

let local_returned t code_id =
  find_set t.local_returned (Code_id_or_name.code_id code_id)

let local_contents t alloc = find_set t.local_contents alloc

let is_local_closure_code t code_id =
  Code_id_or_name.Map.mem (Code_id_or_name.code_id code_id) t.local_closure_code

(* As for the functions of the type checker (see [Lambda.curried_function_kind]
   and [Typecore]): a function returning at mode local has [nlocal >= 1], and
   one whose closure is allocated locally has [nlocal] equal to its arity, which
   makes the curry functions allocate all of its partial applications locally
   (see [Escape_analysis]). *)
let adjust_code_metadata t code_id code_metadata =
  let num_params =
    Flambda_arity.num_params (Code_metadata.params_arity code_metadata)
  in
  let with_nlocal_at_least nlocal code_metadata =
    match Code_metadata.first_complex_local_param code_metadata with
    | Never_partially_applied -> code_metadata
    | Index index ->
      let index' = num_params - nlocal in
      if num_params > 0 && index' < index
      then
        Code_metadata.with_first_complex_local_param (Index index')
          code_metadata
      else code_metadata
  in
  let code_metadata =
    if returns_local t code_id
    then
      with_nlocal_at_least 1
        (Code_metadata.with_result_mode Lambda.maybe_alloc_stack code_metadata)
    else code_metadata
  in
  if is_local_closure_code t code_id
  then with_nlocal_at_least num_params code_metadata
  else code_metadata
