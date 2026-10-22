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

type t =
  { local_allocations : unit Code_id_or_name.Map.t;
    scopes_needing_region : unit Code_id_or_name.Map.t;
    local_returns : unit Code_id_or_name.Map.t
  }

let create db ~unboxed_fields =
  let local_allocations =
    Datalog.get_table Escape_analysis.local_allocation_tbl db
  in
  (* A region is only needed in scopes where a local allocation survives
     rebuilding, that is, is not unboxed, or where a function returning at mode
     local is called. *)
  let scopes_needing_region =
    Code_id_or_name.Map.fold
      (fun alloc () acc ->
        if Code_id_or_name.Map.mem alloc unboxed_fields
        then acc
        else
          List.fold_left
            (fun acc scope -> Code_id_or_name.Map.add scope () acc)
            acc
            (Escape_analysis.scopes_of_allocation db alloc))
      local_allocations
      (Datalog.get_table Escape_analysis.scope_with_local_call_result_tbl db)
  in
  { local_allocations;
    scopes_needing_region;
    local_returns = Datalog.get_table Escape_analysis.local_return_tbl db
  }

let is_local_allocation t var =
  Code_id_or_name.Map.mem (Code_id_or_name.var var) t.local_allocations

let scope_needs_region t scope =
  Code_id_or_name.Map.mem scope t.scopes_needing_region

let returns_local t code_id =
  Code_id_or_name.Map.mem (Code_id_or_name.code_id code_id) t.local_returns
