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

let rec has_older_version_in t ~resolver id candidates : _ Or_unknown.t =
  if Code_id.Set.mem id candidates
  then Known true
  else
    match Code_id.Map.find id t with
    | exception Not_found -> (
      let comp_unit = Code_id.get_compilation_unit id in
      if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
      then Known false
      else
        match resolver comp_unit with
        | exception _ ->
          Misc.fatal_errorf "Exception in resolver@ Backtrace is: %s"
            (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
        | None -> Unknown
        | Some t -> (
          (* Inlining the base case, so that we do not recursively loop in case
             of a code_id that is not bound in the map *)
          match Code_id.Map.find id t with
          | exception Not_found -> Known false
          | older -> has_older_version_in t ~resolver older candidates))
    | older -> has_older_version_in t ~resolver older candidates

let meet_set t ~resolver ids1 ids2 : _ Or_bottom.t =
  if Code_id.Set.equal ids1 ids2
  then Ok ids1
  else
    (* We are trying to keep a reasonably precise set of the code ids that can
       actually be called, in order to avoid de-optimizing to a single direct
       call.

       The typical case is that we could meet { A, A1, A2 } with { A3, A4 }
       where A1, A2, A3, A4 are newer versions of A, in which case we want the
       output to be { A3, A4 } (from the second set, only newer versions of A3
       and A4 are possible, which excludes newer versions of A1 and A2).

       Returning { A } would be correct, but imprecise. *)
    let should_keep_id other_ids id =
      match has_older_version_in t ~resolver id other_ids with
      | Known has_older_version_in_other_ids -> has_older_version_in_other_ids
      | Unknown -> true
    in
    let ids =
      Code_id.Set.union
        (Code_id.Set.filter (should_keep_id ids1) ids2)
        (Code_id.Set.filter (should_keep_id ids2) ids1)
    in
    if Code_id.Set.is_empty ids then Bottom else Ok ids

let meet t ~resolver id1 id2 : _ Or_unknown_or_bottom.t =
  (* Whichever of [id1] and [id2] is newer (or the same as the other one), in
     the case where they are comparable; otherwise bottom. *)
  if Code_id.equal id1 id2
  then Ok id1
  else
    match
      ( has_older_version_in t ~resolver id2 (Code_id.Set.singleton id1),
        has_older_version_in t ~resolver id1 (Code_id.Set.singleton id2) )
    with
    | Known true, _ ->
      (* id1 is an older version of id2 -> id2 is more precise *)
      Ok id2
    | _, Known true ->
      (* id2 is an older version of id1 -> id1 is more precise *)
      Ok id1
    | Known false, Known false ->
      (* id1 and id2 are definitely not related *)
      Bottom
    | Unknown, _ | _, Unknown ->
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
