(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* If a value of type [t] maps [id1] to [id2], it means that [id1] is a newer
   version of [id2]. The relation forms a partial order. These relations are
   expected to be small in the majority of cases. *)
type t = Code_id.t Code_id.Map.t

let [@ocamlformat "disable"] print ppf t = Code_id.Map.print Code_id.print ppf t

let empty = Code_id.Map.empty

let get_older_version_of t code_id = Code_id.Map.find_opt code_id t

(* CR mshinwell: There should be a well-formedness check during [add], otherwise
   the functions below may not work correctly. *)

let add t ~newer ~older = Code_id.Map.add newer older t

exception Missing_cmx_file

let rec all_ids_up_to_root0_exn t ~resolver id all_ids_so_far =
  if Code_id.Set.mem id all_ids_so_far
  then all_ids_so_far
  else
    let all_ids_so_far = Code_id.Set.add id all_ids_so_far in
    match Code_id.Map.find id t with
    | exception Not_found -> (
      let comp_unit = Code_id.get_compilation_unit id in
      if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
      then all_ids_so_far
      else
        match resolver comp_unit with
        | exception _ ->
          Misc.fatal_errorf "Exception in resolver@ Backtrace is: %s"
            (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
        | None -> raise Missing_cmx_file
        | Some t -> (
          (* Inlining the base case, so that we do not recursively loop in case
             of a code_id that is not bound in the map *)
          match Code_id.Map.find id t with
          | exception Not_found -> all_ids_so_far
          | older -> all_ids_up_to_root0_exn t ~resolver older all_ids_so_far))
    | older -> all_ids_up_to_root0_exn t ~resolver older all_ids_so_far

let all_ids_up_to_root_exn t ~resolver id =
  all_ids_up_to_root0_exn t ~resolver id Code_id.Set.empty

let meet_set t ~resolver ids1 ids2 : _ Or_bottom.t =
  if Code_id.Set.equal ids1 ids2
  then Ok ids1
  else
    let should_keep_id other_ids id =
      match all_ids_up_to_root_exn t ~resolver id with
      | exception Missing_cmx_file ->
        (* CR bclement: we likely could do something more precise here rather
           than keeping all ids for which the code age relation involves a
           missing cmx. *)
        true
      | ids_to_root -> not (Code_id.Set.disjoint ids_to_root other_ids)
    in
    let ids =
      Code_id.Set.union
        (Code_id.Set.filter (should_keep_id ids1) ids2)
        (Code_id.Set.filter (should_keep_id ids2) ids1)
    in
    if Code_id.Set.is_empty ids then Bottom else Ok ids

type is_ancestor_of =
  | Definitely_not_ancestor
  | Definitely_ancestor
  | Maybe_ancestor

let is_ancestor_of t ~resolver ~descendant ancestor =
  match all_ids_up_to_root_exn t ~resolver descendant with
  | exception Missing_cmx_file ->
    (* CR bclement: in some situations, we might be able to determine that
       [ancestor] is definitely an ancestor of [descendant] if the missing cmx
       file is a dependency of the file that defines [ancestor]. *)
    Maybe_ancestor
  | all_ancestors_of_child ->
    if Code_id.Set.mem ancestor all_ancestors_of_child
    then Definitely_ancestor
    else Definitely_not_ancestor

let meet t ~resolver id1 id2 : _ Or_unknown_or_bottom.t =
  (* Whichever of [id1] and [id2] is newer (or the same as the other one), in
     the case where they are comparable; otherwise bottom. *)
  if Code_id.equal id1 id2
  then Ok id1
  else
    match
      ( is_ancestor_of t ~resolver ~descendant:id2 id1,
        is_ancestor_of t ~resolver ~descendant:id1 id2 )
    with
    | Definitely_ancestor, _ ->
      (* id1 is an ancestor of id2 -> id2 is more precise *)
      Ok id2
    | _, Definitely_ancestor ->
      (* id2 is an ancestor of id1 -> id1 is more precise *)
      Ok id1
    | Definitely_not_ancestor, Definitely_not_ancestor ->
      (* id1 and id2 are definitely not related *)
      Bottom
    | Maybe_ancestor, _ | _, Maybe_ancestor ->
      (* can't determine whether id1 and id2 are related *)
      Unknown

let union t1 t2 = Code_id.Map.disjoint_union ~eq:Code_id.equal t1 t2

let all_code_ids_for_export t =
  Code_id.Map.fold
    (fun key v acc -> Code_id.Set.add key (Code_id.Set.add v acc))
    t Code_id.Set.empty

let apply_renaming t renaming =
  let rename_code_id = Renaming.apply_code_id renaming in
  Code_id.Map.fold
    (fun key v acc ->
      Code_id.Map.add (rename_code_id key) (rename_code_id v) acc)
    t Code_id.Map.empty

let clean_for_export t ~reachable_names =
  Code_id.Map.filter
    (fun newer_code_id _older_code_id ->
      Name_occurrences.mem_code_id reachable_names newer_code_id)
    t
