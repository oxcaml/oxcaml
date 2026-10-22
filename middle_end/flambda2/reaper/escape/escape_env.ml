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

type region_scope =
  | No_region_scope
  | Toplevel
  | Scope of Code_id_or_name.t

type t =
  { region_scope : region_scope;
    enclosing_region_scopes : Code_id_or_name.t list;
    foreign_regions : Variable.Set.t;
    try_regions : Variable.Set.t;
    handlers : (Variable.t list * Variable.Set.t) list;
    return_continuation : Continuation.t;
    exn_continuation : Continuation.t
  }

let create ~region_scope ~return_continuation ~exn_continuation =
  { region_scope;
    enclosing_region_scopes =
      (match region_scope with
      | No_region_scope | Toplevel -> []
      | Scope scope -> [scope]);
    foreign_regions = Variable.Set.empty;
    try_regions = Variable.Set.empty;
    handlers = [];
    return_continuation;
    exn_continuation
  }

let region_scope t =
  match t.region_scope with
  | No_region_scope | Toplevel -> None
  | Scope scope ->
    (* An allocation always goes to the innermost region open at runtime, so
       none may be made local in [scope] while a pre-existing region is open. *)
    if Variable.Set.is_empty t.foreign_regions then Some scope else None

let enclosing_region_scopes t = t.enclosing_region_scopes

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let enter_recursive_handler t ~scope =
  match t.region_scope with
  | No_region_scope -> t
  | (Toplevel | Scope _) when not (Variable.Set.is_empty t.foreign_regions) ->
    (* The region of the handlers would be nested inside the pre-existing
       region, which may be closed inside the handlers. *)
    t
  | Toplevel | Scope _ ->
    let scope = Code_id_or_name.var scope in
    { t with
      region_scope = Scope scope;
      enclosing_region_scopes = scope :: t.enclosing_region_scopes
    }

let enter_foreign_region t region =
  { t with foreign_regions = Variable.Set.add region t.foreign_regions }

let exit_foreign_region t region =
  { t with foreign_regions = Variable.Set.remove region t.foreign_regions }

let enter_try_region t region =
  { t with try_regions = Variable.Set.add region t.try_regions }

let exit_try_region t region =
  { t with try_regions = Variable.Set.remove region t.try_regions }

let enter_handler t ~params =
  { t with handlers = (params, t.try_regions) :: t.handlers }

let handler_params_freed_by_end_of_try_region t region =
  List.concat_map
    (fun (params, try_regions) ->
      if Variable.Set.mem region try_regions then params else [])
    t.handlers
