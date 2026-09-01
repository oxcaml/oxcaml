let log_section = "short-paths"
let { Logger.log } = Logger.for_section log_section
let { Logger.log = log_dbg } = Logger.for_section (log_section ^ "-dbg")

let path_print = Format_doc.compat Path.print

module Out_type = struct
  (* This part is copied from upstream's [Out_type] *)

  (* Normalize paths *)

  type param_subst = Id | Nth of int | Map of int list

  let compose l1 = function
    | Id -> Map l1
    | Map l2 -> Map (List.map (List.nth l1) l2)
    | Nth n -> Nth (List.nth l1 n)

  let rec index l x =
    match l with
    | [] -> raise Not_found
    | a :: l -> if Types.eq_type x a then 0 else 1 + index l x

  let rec uniq = function
    | [] -> true
    | a :: l -> (not (List.memq (a : int) l)) && uniq l

  let rec normalize_type_path ?(cache = false) env p =
    let open Types in
    try
      let params (* type_params *), ty (* type_manifest *), _ =
        (* Find the manifest type associated to a type when appropriate:
           - the type should be public or should have a private row,
           - the type should have an associated manifest type. *)
        Env.find_type_expansion p env
      in
      match get_desc ty with
      | Tconstr (p1, tyl, _) ->
        log ~title:"normalize_type_path" "Found type expansion %a for %a"
          Logger.fmt (Fun.flip path_print p1) Logger.fmt (Fun.flip path_print p);
        if
          List.length params = List.length tyl
          && List.for_all2 eq_type params tyl
          && p1 <> p (* TODO if this the correct way to prevent looping ? *)
        then normalize_type_path ~cache env p1
        else if
          cache
          || List.length params <= List.length tyl
          || not (uniq (List.map get_id tyl))
        then (Env.normalize_type_path None env p, Id)
        else
          let l1 = List.map (index params) tyl in
          let p2, s2 = normalize_type_path ~cache env p1 in
          (p2, compose l1 s2)
      | _ -> (Env.normalize_type_path None env p, Nth (index params ty))
    with Not_found ->
      log ~title:"normalize_type_path"
        "Calling [Env.normalize_type_path] for %a" Logger.fmt
        (Fun.flip path_print p);
      (Env.normalize_type_path None env p, Id)

  let find_double_underscore s =
    let len = String.length s in
    let rec loop i =
      if i + 1 >= len then None
      else if s.[i] = '_' && s.[i + 1] = '_' then Some i
      else loop (i + 1)
    in
    loop 0

  let penalty s =
    if s <> "" && s.[0] = '_' then 10
    else
      match find_double_underscore s with
      | None -> 1
      | Some _ -> 10
end

let rec longident_cost = function
  | Longident.Lident id -> Out_type.penalty id
  | Ldot (l, id) -> Out_type.penalty id.txt + longident_cost l.txt
  | Lapply (l1, l2) -> longident_cost l1.txt + longident_cost l2.txt

let compare_strings s1 s2 =
  let length_diff = String.length s1 - String.length s2 in
  if length_diff <> 0 then length_diff else String.compare s1 s2

let compare_longidents l1 l2 =
  let l1_cost = longident_cost l1 in
  let l2_cost = longident_cost l2 in
  let cost_diff = l1_cost - l2_cost in
  if cost_diff <> 0 then cost_diff
  else Discourse_types.compare_longidents ~compare_strings l1 l2

type kind = Type | Module | Module_type
let kind_of_kind = function
  | Type -> Shape.Sig_component_kind.Type
  | Module -> Module
  | Module_type -> Module_type
let string_of_kind = function
  | Type -> "Type"
  | Module -> "Module"
  | Module_type -> "Module_type"

module Lid_path_set = struct
  module T = struct
    type t = kind * Longident.t * Path.t

    let compare (_, l1, p1) (_, l2, p2) =
      let c = compare_longidents l1 l2 in
      if c = 0 then Path.compare p1 p2 else c
  end
  let pp_lid_path fmt (l, p) =
    Format.fprintf fmt "%a (%a)" Pprintast.longident l path_print p
  let pp_elt fmt (_, l, p) = pp_lid_path fmt (l, p)

  include Set.Make (T)
end

module Uid = Shape.Uid
module Priority_queue = Lid_path_set
module Lid_trie = Discourse_types.Lid_trie
module Lid_set = Discourse_types.Lid_set
module Lid_map = Discourse_types.Lid_map
module String_map = Discourse_types.String_map

(* To shorten paths we will build a table that maps "canonical paths" - i.e. a
   path which is not itself an alias - to a set of paths from D that are aliases
   to it. We will build this table by putting the elements of D into a priority
   queue ordered by their length. Then we "process" each element by looking up
   its canonical path and then adding it to the map. We do this using whatever
   environment we are currently trying to shorten paths with (ignoring any local
   constraints). If an element of D is not available in that environment then we
   skip it and leave it in the priority queue. *)

(* When shortenning:
   - We empty the Discourse into the priority queue.
   - We compute all possible paths substitutions and add them to the priority
     queue. TODO: there might be a better, lazier way.
   - We read the priority queue in orderto sort paths by their canonical form.
   - Once we found a valid path for the request we stop after finishing the
     current level (when the next path in the queue is longer).
   - The priority queue and canonical paths map are kept between queries. *)

let priority_queue : Priority_queue.t ref =
  Local_store.s_ref Priority_queue.empty
let not_in_env : Lid_trie.t ref = Local_store.s_ref Lid_trie.empty
let canon_table : Lid_path_set.t Path.Tbl.t ref =
  Local_store.s_table Path.Tbl.create 256

let pp_table fmt t =
  Path.Tbl.iter
    (fun p lids ->
      let lids = Lid_path_set.to_list lids in
      Format.fprintf fmt "@;%a -> {%a}" path_print p
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Lid_path_set.pp_elt)
        lids)
    t

let normalize_type_path = Out_type.normalize_type_path ~cache:false

let apply_one_substitution ~target ~replacements (t : Lid_trie.t) =
  match Lid_trie.reach t target with
  | None -> Lid_trie.empty
  | Some (Trie (paths, children)) ->
    let new_lids =
      Lid_set.fold
        (fun lid acc ->
          Lid_trie.union acc @@ Lid_trie.trie_of_lid ~children lid paths)
        replacements Lid_trie.empty
    in
    new_lids

let apply_substitutions substs t =
  Lid_map.fold
    (fun target replacements acc ->
      let r = apply_one_substitution t ~target ~replacements in
      Lid_trie.union acc r)
    substs Lid_trie.empty

let apply_substitutions_fixpoint t substs =
  let rec aux ~fuel acc t =
    let new_lids = apply_substitutions substs t in
    if Lid_trie.is_empty new_lids then acc
    else begin
      let acc_length = Lid_trie.size acc in
      let acc = Lid_trie.union acc new_lids in
      if fuel = 0 || acc_length = Lid_trie.size acc then acc
      else aux ~fuel:(fuel - 1) acc new_lids
    end
  in
  aux ~fuel:2 t t

let fill_queue (paths : Lid_trie.t) queue =
  Lid_trie.to_seq paths
  |> Seq.fold_left
       (fun acc (lid, paths) ->
         Discourse_types.Paths.fold
           (fun (kind, path) acc ->
             match kind with
             | Type -> Priority_queue.add (Type, lid, path) acc
             | Module -> Priority_queue.add (Module, lid, path) acc
             | Module_type -> Priority_queue.add (Module_type, lid, path) acc
             | _ -> acc)
           paths acc)
       queue

(* This function should be called before any attempt to [shorten] paths in an
   environment different that the one of the previous attempt.

   This is called after any function wrapped with [Printyp.wrap_printing_env].
*)
let restore_ignored_paths id =
  match id with
  | None ->
    priority_queue := fill_queue !not_in_env !priority_queue;
    not_in_env := Lid_trie.empty
  | Some ident -> (
    match Lid_trie.take (Ident.name ident) !not_in_env with
    | None, _ -> ()
    | Some t, remaining ->
      priority_queue := fill_queue t !priority_queue;
      not_in_env := remaining)

let find_by_name env kind lid =
  match kind with
  | Type -> fst (Env.find_type_by_name lid env)
  | Module -> fst (Env.find_module_by_name_lazy lid env)
  | Module_type -> fst (Env.find_modtype_by_name_lazy lid env)

let normalize env kind path =
  match kind with
  | Type -> fst (normalize_type_path env path)
  | Module -> Env.normalize_module_path None env path
  | Module_type -> Env.normalize_modtype_path env path

let find_uid env kind path =
  match kind with
  | Type -> (Env.find_type path env).type_uid
  | Module -> (Env.find_module_lazy path env).md_uid
  | Module_type -> (Env.find_modtype_lazy path env).mtd_uid

let find_path_in_env env (kind, lid, path) =
  (* TODO it might be worth it to memoïze this function *)
  match find_by_name env kind lid with
  (* TODO Env.find_type_by_name can raise Assertion_failure when the case Papply
     is reached in Env.lookup_type_path_full *)
  | exception (Not_found | Assert_failure _) ->
    log ~title:"find_path_in_env" "%s name not found: %a" (string_of_kind kind)
      Logger.fmt
      (Fun.flip Pprintast.longident lid);
    None
  | path_in_env -> (
    log ~title:"find_path_in_env" "Lid: %a [%a] Path in env: %a" Logger.fmt
      (Fun.flip Pprintast.longident lid)
      Logger.fmt (Fun.flip path_print path) Logger.fmt
      (Fun.flip path_print path_in_env);
    if Path.compare path path_in_env == 0 then Some path_in_env
    else
      try
        let path = normalize env kind path in
        let path' = normalize env kind path_in_env in
        log ~title:"find_path_in_env" "%a <>? %a" Logger.fmt
          (Fun.flip path_print path) Logger.fmt
          (Fun.flip path_print path');
        if Path.compare path path' == 0 then Some path_in_env
        else begin
          (* [find_uid] might trigger more cmi loading. we only do it if we were
             not able to check validity by comparing the paths. *)
          let uid = find_uid env kind path in
          let uid' = find_uid env kind path' in
          log ~title:"find_path_in_env" "%a <>? %a" Logger.fmt
            (Fun.flip Uid.print uid) Logger.fmt (Fun.flip Uid.print uid');
          if Uid.compare uid uid' == 0 then Some path_in_env else None
        end
      with Not_found -> None)

let find_best_lid env ~canon_path table target_kind =
  match Path.Tbl.find_opt table canon_path with
  | None -> None
  | Some lids ->
    Lid_path_set.to_seq lids
    |> Seq.find_map (fun ((kind, lid, _) as item) ->
        if kind <> target_kind then None
        else Option.map (fun path -> (lid, path)) @@ find_path_in_env env item)

let improve_lid env ~canon_path table kind lid =
  find_best_lid env ~canon_path table kind
  |> Option.fold ~none:lid ~some:Option.some

type state = { queue : Priority_queue.t; not_in_env : Lid_trie.t }
let process_queue env state ~table ~canon_path target_kind best =
  let rec fill_by_level ~compare seq state best_lid =
    log ~title:"fill_by_level" "Current best: %a" Logger.fmt (fun f ->
        Format.pp_print_option Lid_path_set.pp_lid_path f best_lid);
    match seq () with
    | Seq.Nil ->
      log ~title:"fill_by_level" "Empty queue";
      let best_path = improve_lid env ~canon_path table target_kind best_lid in
      (best_path, state)
    | Seq.Cons ((kind, next_lid, path), next) ->
      let next_level = compare next_lid < 0 in
      if next_level then
        begin match improve_lid env ~canon_path table kind best_lid with
        | Some (best_lid, path) when compare_longidents best_lid next_lid < 0 ->
          log ~title:"fill_by_level"
            "Finished level and found a name shorter than the previous level:\n\
            \ %a (%a)"
            Logger.fmt
            (fun fmt -> Pprintast.longident fmt best_lid)
            Logger.fmt (Fun.flip path_print path);
          (Some (best_lid, path), state)
        | best_lid ->
          log ~title:"fill_by_level" "Finished a level. Current best: %a"
            Logger.fmt (fun f ->
              Format.pp_print_option Lid_path_set.pp_lid_path f best_lid);
          add_lid_to_table state (kind, next_lid, path) next best_lid
        end
      else add_lid_to_table state (kind, next_lid, path) next best_lid
  and add_lid_to_table state ((kind, lid, path) as item) next best_lid =
    log ~title:"fill_by_level" "Treating %a (%a)" Logger.fmt
      (fun fmt -> Pprintast.longident fmt lid)
      Logger.fmt
      (fun fmt -> path_print fmt path);
    let state =
      let is_valid_in_current_env =
        (* In the presence of `open` statements the Discourse contains partial
           paths that cannot be looked-up in the environement directly. We can
           find by name instead. However we must then check that we did actually
           find the type we were looking (same ident) for and not an homonym. *)
        find_path_in_env env (kind, lid, path)
      in
      match is_valid_in_current_env with
      | Some _path_in_env -> begin
        (* Even if it would feel natural to use Uids as keys in the table,
           Trying to `Env.find_*` them naively here would results in unwanted
           cmi loading. TODO: we could try to isolate these cases were that
           happen, but even then it's unclear which Uid we would use to identity
           them. *)
        let canonical_path = normalize env kind path in
        log ~title:"fill_by_level" "Updating table: %a -> { %a [%a] }"
          Logger.fmt
          (fun fmt -> path_print fmt canonical_path)
          Logger.fmt
          (fun fmt -> Pprintast.longident fmt lid)
          Logger.fmt
          (fun fmt -> path_print fmt path);
        let () =
          let update =
            match Path.Tbl.find_opt table canonical_path with
            | None -> Lid_path_set.singleton item
            | Some set -> Lid_path_set.add item set
          in
          Path.Tbl.replace table canonical_path update
        in
        (* And remove it from the queue *)
        let queue = Priority_queue.remove item state.queue in
        { state with queue }
        end
      | _ -> begin
        log ~title:"fill_by_level" "Name: %a invalid in the current env"
          Logger.fmt (fun fmt -> Pprintast.longident fmt lid);

        (* Elements not valid in the current environement are kept in a separate
           queue. This prevent uselessly re-testing them when [shorten] is
           called multiple times with the same environment. This notably happens
           when printing module signatures, and they can be quite large. *)
        let queue = Priority_queue.remove item state.queue in
        let not_in_env =
          Lid_trie.add lid (Type, path) state.not_in_env
        in
        { queue; not_in_env }
        end
    in
    fill_by_level ~compare:(compare_longidents lid) next state best_lid
  in
  match (Priority_queue.min_elt_opt state.queue, best) with
  | None, _ -> (best, state)
  | Some (_kind, shortest_lid_in_queue, _path), Some (best', _)
    when compare_longidents shortest_lid_in_queue best' > 0 ->
    (* There cannot be a better candidate in the queue *)
    log ~title:"process_queue" "No shorter longidents in the queue. (> %a)"
      Logger.fmt
    @@ Fun.flip Pprintast.longident shortest_lid_in_queue;
    (best, state)
  | Some (_kind, shortest_lid, _path), best ->
    let compare = compare_longidents shortest_lid in
    let seq = Priority_queue.to_seq state.queue in
    fill_by_level ~compare seq state best

(* Given a Path and a Longident that represents a suffix  of that path,
   [path_mask] returns the paths corresponding to that suffix. *)

(* This cache prevent the same ident to be given different stamps in the same query *)
(* todo: we should probably store the masking in the table instead. *)
let path_masks_cache : (Longident.t * Path.t, Path.t) Hashtbl.t ref =
  Local_store.s_table Hashtbl.create 32

(* The print function expect a path but we have longidents.

   TODO What we should really do is modify the printing function to expect a
   longident. *)
let rec path_mask (path : Path.t) (lid : Longident.t) : Path.t =
  match Hashtbl.find_opt !path_masks_cache (lid, path) with
  | Some path -> path
  | None ->
    let masked_path : Path.t =
      match (path, lid) with
      | Pident id, Lident _ -> Pident id
      | Pdot (p, s), Ldot (l, _) -> Pdot (path_mask p l.txt, s)
      | Papply (p1, p2), Lapply (l1, l2) ->
        Papply (path_mask p1 l1.txt, path_mask p2 l2.txt)
      | (Pdot _ | Papply _), Lident s ->
        let scope = Path.scope path in
        Pident (Ident.create_scoped ~scope s)
      | _ -> path
    in
    Hashtbl.add !path_masks_cache (lid, path) masked_path;
    masked_path

let shorten ~env ~initial ~canon_path kind =
  let discourse = Discourse.get () in
  let queue, table = (!priority_queue, !canon_table) in
  log_dbg ~title:"shorten" "Current discourse: %a\n%!" Logger.fmt (fun fmt ->
      Discourse.debug_print fmt);
  log_dbg ~title:"shorten" "Current queue: %a" Logger.fmt (fun fmt ->
      Format.pp_print_seq ~pp_sep:Format.pp_print_space Lid_path_set.pp_elt fmt
        (Priority_queue.to_seq queue));
  log_dbg ~title:"shorten" "Current notinenv: %a" Logger.fmt (fun fmt ->
      Discourse_types.Lid_trie.pp_seq fmt !not_in_env);
  log_dbg ~title:"shorten" "Current table: %a" Logger.fmt (fun fmt ->
      pp_table fmt table);

  let queue =
    let paths =
      Lid_trie.add
        (Untypeast.lident_of_path initial)
        (kind_of_kind kind, initial)
        discourse.paths
      |> Lid_trie.add
           (Untypeast.lident_of_path canon_path)
           (kind_of_kind kind, canon_path)
    in
    let paths = apply_substitutions_fixpoint paths discourse.substs in
    log_dbg ~title:"shorten" "Discourse after substitutions: %a\n%!" Logger.fmt
      (fun fmt -> Lid_trie.pp_seq fmt paths);
    fill_queue paths queue
  in

  log_dbg ~title:"shorten" "Current queue: %a" Logger.fmt (fun fmt ->
      Format.pp_print_seq ~pp_sep:Format.pp_print_space Lid_path_set.pp_elt fmt
        (Priority_queue.to_seq queue));

  (* Do we already have a candidate ? *)
  let best = find_best_lid env ~canon_path table kind in

  log ~title:"shorten" "Initial: %a; Canon: %a; Current best: %a" Logger.fmt
    (Fun.flip path_print initial) Logger.fmt (Fun.flip path_print canon_path)
    Logger.fmt (fun f ->
      let best = Option.map (fun (l, p) -> (kind, l, p)) best in
      Format.pp_print_option Lid_path_set.pp_elt f best);

  (* Is there a better one in the queue ? *)
  let best_lid, { queue = queue'; not_in_env = not_in_env' } =
    let not_in_env = !not_in_env in
    process_queue env { queue; not_in_env } ~table ~canon_path kind best
  in

  (* Empty the discourse *)
  Discourse.set { Discourse.empty_u with substs = discourse.substs };

  (* Update the persistent queues and table *)
  priority_queue := queue';
  not_in_env := not_in_env';

  match best_lid with
  | None ->
    log ~title:"shorten" "Falling-back on initial path %a" Logger.fmt
      (Fun.flip path_print initial);
    initial
  | Some (lid, path) ->
    log ~title:"shorten" "%a" Logger.fmt (fun fmt ->
        Format.fprintf fmt "Masking path %a with lid %a" path_print path
          Pprintast.longident lid);
    path_mask path lid

type type_result = Short_paths.type_result =
  | Nth of int
  | Path of int list option * Path.t

type type_resolution = Short_paths.type_resolution =
  | Nth of int
  | Subst of int list
  | Id

let find_type env initial : type_result =
  match normalize_type_path env initial with
  | _, Nth i -> (* TODO, this looks like this is incorrect *) Nth i
  | canon_path, Id -> Path (None, shorten ~env ~initial ~canon_path Type)
  | canon_path, Map l -> Path (Some l, shorten ~env ~initial ~canon_path Type)

let find_type_resolution env path : type_resolution =
  match normalize_type_path env path with
  | _, Nth i -> Nth i
  | _, Id -> Id
  | _, Map l -> Subst l

let find_type_simple env initial =
  let canon_path, _subst = normalize_type_path env initial in
  let short = shorten ~env ~initial ~canon_path Type in
  short

let find_module env initial =
  let canon_path = Env.normalize_module_path None env initial in
  shorten ~env ~initial ~canon_path Module

let find_module_type env initial =
  let canon_path = Env.normalize_modtype_path env initial in
  shorten ~env ~initial ~canon_path Module_type
