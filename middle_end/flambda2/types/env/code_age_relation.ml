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

let rec all_ids_up_to_root0 t ~resolver id all_ids_so_far trace =
  if Code_id.Set.mem id all_ids_so_far
  then
    let trace =
      Format.asprintf "%a: already seen, stopping"
        Code_id.print id
      :: trace
    in
    all_ids_so_far, trace
  else
    let all_ids_so_far = Code_id.Set.add id all_ids_so_far in
    match Code_id.Map.find id t with
    | exception Not_found -> (
      let comp_unit = Code_id.get_compilation_unit id in
      if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
      then
        let trace =
          Format.asprintf "%a: not in map, same compilation unit, stopping"
            Code_id.print id
          :: trace
        in
        all_ids_so_far, trace
      else
        match resolver comp_unit with
        | exception _ ->
          Misc.fatal_errorf "Exception in resolver@ Backtrace is: %s"
            (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
        | None ->
          let cu_name =
            Compilation_unit.name comp_unit
            |> Compilation_unit.Name.to_string
          in
          let trace =
            Format.asprintf
              "%a: not in map, resolver returned None for unit %s, \
               stopping"
              Code_id.print id
              cu_name
            :: trace
          in
          all_ids_so_far, trace
        | Some t -> (
          let cu_name =
            Compilation_unit.name comp_unit
            |> Compilation_unit.Name.to_string
          in
          match Code_id.Map.find id t with
          | exception Not_found ->
            let trace =
              Format.asprintf
                "%a: not in map, resolved unit %s but not found there \
                 either, stopping"
                Code_id.print id
                cu_name
              :: trace
            in
            all_ids_so_far, trace
          | older ->
            let trace =
              Format.asprintf
                "%a: not in local map, resolved unit %s, found older %a"
                Code_id.print id
                cu_name
                Code_id.print older
              :: trace
            in
            all_ids_up_to_root0 t ~resolver older all_ids_so_far trace))
    | older ->
      let trace =
        Format.asprintf "%a: found older %a"
          Code_id.print id
          Code_id.print older
        :: trace
      in
      all_ids_up_to_root0 t ~resolver older all_ids_so_far trace

let all_ids_up_to_root t ~resolver id =
  all_ids_up_to_root0 t ~resolver id Code_id.Set.empty [] |> fst

let closure_invalid_debug =
  Option.is_some (Sys.getenv_opt "FLAMBDA2_CLOSURE_INVALID_DEBUG")

let all_ids_up_to_root_traced t ~resolver id =
  all_ids_up_to_root0 t ~resolver id Code_id.Set.empty []

let meet_set t ~resolver ids1 ids2 : _ Or_bottom.t =
  if Code_id.Set.equal ids1 ids2
  then Ok ids1
  else
    let traces = ref [] in
    let should_keep_id other_ids id =
      let ids_to_root, trace =
        all_ids_up_to_root_traced t ~resolver id
      in
      let disjoint = Code_id.Set.disjoint ids_to_root other_ids in
      if closure_invalid_debug
      then
        traces :=
          (id, ids_to_root, List.rev trace, not disjoint) :: !traces;
      not disjoint
    in
    let ids =
      Code_id.Set.union
        (Code_id.Set.filter (should_keep_id ids1) ids2)
        (Code_id.Set.filter (should_keep_id ids2) ids1)
    in
    if Code_id.Set.is_empty ids
    then (
      if closure_invalid_debug
      then (
        Format.eprintf
          "@[<v>code_age_relation.meet_set returning Bottom:@ \
           ids1: %a@ ids2: %a@ "
          Code_id.Set.print ids1
          Code_id.Set.print ids2;
        List.iter
          (fun (id, ids_to_root, trace, kept) ->
            Format.eprintf
              "@[<v 2>walk from %a (kept=%b, \
               ids_to_root={%a}):@ %a@]@ "
              Code_id.print id
              kept
              Code_id.Set.print ids_to_root
              (Format.pp_print_list
                ~pp_sep:Format.pp_print_space
                Format.pp_print_string)
              trace)
          (List.rev !traces);
        Format.eprintf "@]@.");
      Bottom)
    else Ok ids

let _num_ids_up_to_root t ~resolver id =
  Code_id.Set.cardinal (all_ids_up_to_root t ~resolver id)

let meet t ~resolver id1 id2 : _ Or_bottom.t =
  (* Whichever of [id1] and [id2] is newer (or the same as the other one), in
     the case where they are comparable; otherwise bottom. *)
  if Code_id.equal id1 id2
  then Ok id1
  else
    let id1_to_root = all_ids_up_to_root t ~resolver id1 in
    let id2_to_root = all_ids_up_to_root t ~resolver id2 in
    if Code_id.Set.mem id1 id2_to_root
    then Ok id2
    else if Code_id.Set.mem id2 id1_to_root
    then Ok id1
    else Bottom

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
