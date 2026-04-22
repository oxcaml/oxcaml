(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Delayed renaming comes in two stages so that finding the type of a single
   name does not have to apply the pending renaming to all other types in the
   level.

   [pending_renaming] is applied to everything: the keys and values of
   [names_to_types], [aliases] and [symbol_projections]. The first operation
   that needs to see the keys (or [aliases] or [symbol_projections]) calls
   [force_structures], which eagerly applies [pending_renaming] to those three
   fields while moving it into [pending_value_renaming]. Applying a renaming to
   the keys is cheap compared to applying one to the values: only the values
   involve a traversal of Flambda types.

   [pending_value_renaming] is applied only to the values in [names_to_types].
   When the type of a single name is looked up via [find_opt],
   [force_structures] is called and the renaming is applied to just that one
   type. Bulk operations (e.g. [ids_for_export]) call [force_everything] to
   eagerly apply the renaming to every value.

   Either pending renaming may carry an import map. The only restriction is that
   [Renaming.compose ~second:r ~first:s] requires [r] to be import-map free (or
   [s] to be identity); [apply_renaming] and [force_structures] therefore
   arrange to force just enough pending work to keep this restriction satisfied
   when composing. In the common cmx-import case the incoming import-map
   renaming is applied to an otherwise-empty level so no forcing happens and the
   renaming is simply stored.

   The mutation performed by the [force_*] functions preserves the logical value
   of [t], so it is safe to mutate even if other code holds the same [t]: any
   such holder will simply see the more-forced representation next time it
   looks. *)
type t =
  { mutable names_to_types :
      (Type_grammar.t * Binding_time.With_name_mode.t) Name.Map.t;
    mutable aliases : Aliases.t;
    mutable symbol_projections : Symbol_projection.t Variable.Map.t;
    mutable pending_renaming : Renaming.t;
    mutable pending_value_renaming : Renaming.t
  }

let rename_map_keys_names_to_types names_to_types renaming =
  Name.Map.fold
    (fun name entry acc ->
      Name.Map.add (Renaming.apply_name renaming name) entry acc)
    names_to_types Name.Map.empty

let rename_symbol_projections symbol_projections renaming =
  Variable.Map.fold
    (fun var proj acc ->
      Variable.Map.add
        (Renaming.apply_variable renaming var)
        (Symbol_projection.apply_renaming proj renaming)
        acc)
    symbol_projections Variable.Map.empty

let apply_renaming_to_values names_to_types renaming =
  Name.Map.map_sharing
    (fun ((ty, btm) as info) ->
      let ty' = Type_grammar.apply_renaming ty renaming in
      if ty == ty' then info else ty', btm)
    names_to_types

(* Eagerly apply [pending_renaming] to keys of [names_to_types], to [aliases]
   and to [symbol_projections], moving it into [pending_value_renaming]. *)
let force_structures t =
  if not (Renaming.is_identity t.pending_renaming)
  then (
    let r = t.pending_renaming in
    t.names_to_types <- rename_map_keys_names_to_types t.names_to_types r;
    t.aliases <- Aliases.apply_renaming t.aliases r;
    t.symbol_projections <- rename_symbol_projections t.symbol_projections r;
    t.pending_renaming <- Renaming.empty;
    (* Compose [r] into [pending_value_renaming]. [Renaming.compose] does not
       allow [second] (here [r]) to carry an import map when [first] is
       non-identity, so in that case we first eagerly apply the existing
       [pending_value_renaming] to all values and then set
       [pending_value_renaming] to [r] alone. *)
    if
      Renaming.has_import_map r
      && not (Renaming.is_identity t.pending_value_renaming)
    then (
      t.names_to_types
        <- apply_renaming_to_values t.names_to_types t.pending_value_renaming;
      t.pending_value_renaming <- r)
    else
      t.pending_value_renaming
        <- Renaming.compose ~second:r ~first:t.pending_value_renaming)

(* Also apply [pending_value_renaming] eagerly to all values. *)
let force_everything t =
  force_structures t;
  if not (Renaming.is_identity t.pending_value_renaming)
  then (
    t.names_to_types
      <- apply_renaming_to_values t.names_to_types t.pending_value_renaming;
    t.pending_value_renaming <- Renaming.empty)

let print_kind_and_mode ~min_binding_time ppf (ty, binding_time_and_mode) =
  let kind = Type_grammar.kind ty in
  let mode =
    Binding_time.With_name_mode.scoped_name_mode binding_time_and_mode
      ~min_binding_time
  in
  Format.fprintf ppf ":: %a %a" Flambda_kind.print kind Name_mode.print mode

let print_name_modes ~restrict_to ~min_binding_time ppf t =
  force_structures t;
  Name.Map.print
    (print_kind_and_mode ~min_binding_time)
    ppf
    (Name.Map.filter
       (fun name _ -> Name.Set.mem name restrict_to)
       t.names_to_types)

(* [empty] is shared across all callers. Its pending renamings are both
   [Renaming.empty], so the [force_*] functions are no-ops on it and the shared
   record is never mutated. *)
let empty =
  { names_to_types = Name.Map.empty;
    aliases = Aliases.empty;
    symbol_projections = Variable.Map.empty;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let names_to_types t =
  force_everything t;
  t.names_to_types

let aliases t =
  force_structures t;
  t.aliases

let symbol_projections t =
  force_structures t;
  t.symbol_projections

let find_opt t name =
  force_structures t;
  match Name.Map.find_opt name t.names_to_types with
  | None -> None
  | Some (ty, btm) ->
    if Renaming.is_identity t.pending_value_renaming
    then Some (ty, btm)
    else
      let ty' = Type_grammar.apply_renaming ty t.pending_value_renaming in
      Some (ty', btm)

let name_domain t =
  force_structures t;
  Name.Map.keys t.names_to_types

let mem t name =
  force_structures t;
  Name.Map.mem name t.names_to_types

let add_or_replace_binding t (name : Name.t) ty binding_time name_mode =
  force_everything t;
  let binding_time_and_mode =
    Binding_time.With_name_mode.create binding_time name_mode
  in
  let names_to_types =
    Name.Map.add name (ty, binding_time_and_mode) t.names_to_types
  in
  { names_to_types;
    aliases = t.aliases;
    symbol_projections = t.symbol_projections;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let replace_variable_binding t var ty =
  force_everything t;
  let names_to_types =
    Name.Map.replace (Name.var var)
      (function _old_ty, binding_time_and_mode -> ty, binding_time_and_mode)
      t.names_to_types
  in
  { names_to_types;
    aliases = t.aliases;
    symbol_projections = t.symbol_projections;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let with_aliases t ~aliases =
  force_everything t;
  { names_to_types = t.names_to_types;
    aliases;
    symbol_projections = t.symbol_projections;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let add_symbol_projection t var proj =
  force_everything t;
  let symbol_projections = Variable.Map.add var proj t.symbol_projections in
  { names_to_types = t.names_to_types;
    aliases = t.aliases;
    symbol_projections;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let find_symbol_projection t var =
  force_structures t;
  match Variable.Map.find var t.symbol_projections with
  | exception Not_found -> None
  | proj -> Some proj

let clean_for_export t ~reachable_names =
  force_everything t;
  (* Names coming from other compilation units or unreachable are removed *)
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let names_to_types =
    Name.Map.filter_map
      (fun name (ty, binding_time_and_mode) ->
        if
          Name_occurrences.mem_name reachable_names name
          && Compilation_unit.equal
               (Name.compilation_unit name)
               current_compilation_unit
        then (
          let binding_time_and_mode =
            if Name.is_var name
            then Binding_time.With_name_mode.imported_variables
            else binding_time_and_mode
          in
          (match Type_grammar.get_alias_opt ty with
          | None -> ()
          | Some alias ->
            Misc.fatal_errorf "Remaining alias after cleanup: %a -> %a@."
              Name.print name Simple.print alias);
          Some (ty, binding_time_and_mode))
        else None)
      t.names_to_types
  in
  { names_to_types;
    aliases = Aliases.empty;
    symbol_projections = t.symbol_projections;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let apply_renaming t renaming =
  if Renaming.is_identity renaming
  then t
  else (
    (* [Renaming.compose ~second:renaming ~first:pending_renaming] fails only
       when [renaming] has an import map AND [pending_renaming] is non-identity.
       In that one case we force the existing [pending_renaming] into
       [pending_value_renaming] so that the subsequent [compose] has [first =
       Renaming.empty] and succeeds. Crucially, [force_structures] does not
       traverse any types in [names_to_types]: only the keys, [aliases] and
       [symbol_projections] are touched. *)
    if
      Renaming.has_import_map renaming
      && not (Renaming.is_identity t.pending_renaming)
    then force_structures t;
    { names_to_types = t.names_to_types;
      aliases = t.aliases;
      symbol_projections = t.symbol_projections;
      pending_renaming =
        Renaming.compose ~second:renaming ~first:t.pending_renaming;
      pending_value_renaming = t.pending_value_renaming
    })

let merge t1 t2 =
  force_everything t1;
  force_everything t2;
  let names_to_types =
    Name.Map.disjoint_union t1.names_to_types t2.names_to_types
  in
  let aliases = Aliases.empty in
  let symbol_projections =
    Variable.Map.union_total_shared
      (fun var proj1 proj2 ->
        if Symbol_projection.equal proj1 proj2
        then proj1
        else
          Misc.fatal_errorf
            "Cannot merge symbol projections for %a:@ %a@ and@ %a"
            Variable.print var Symbol_projection.print proj1
            Symbol_projection.print proj2)
      t1.symbol_projections t2.symbol_projections
  in
  { names_to_types;
    aliases;
    symbol_projections;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let canonicalise t simple =
  force_structures t;
  Simple.pattern_match simple
    ~const:(fun _ -> simple)
    ~name:(fun name ~coercion ->
      Simple.apply_coercion_exn
        (Aliases.get_canonical_ignoring_name_mode t.aliases name)
        coercion)

let remove_unused_value_slots_and_shortcut_aliases t ~used_value_slots =
  force_everything t;
  let canonicalise = canonicalise t in
  let names_to_types =
    Name.Map.map_sharing
      (fun ((ty, binding_time_and_mode) as info) ->
        let ty' =
          Type_grammar.remove_unused_value_slots_and_shortcut_aliases ty
            ~used_value_slots ~canonicalise
        in
        if ty == ty' then info else ty', binding_time_and_mode)
      t.names_to_types
  in
  { names_to_types;
    aliases = t.aliases;
    symbol_projections = t.symbol_projections;
    pending_renaming = Renaming.empty;
    pending_value_renaming = Renaming.empty
  }

let free_function_slots_and_value_slots t =
  force_everything t;
  let from_projections =
    Variable.Map.fold
      (fun _var proj free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.restrict_to_value_slots_and_function_slots
             (Symbol_projection.free_names proj)))
      t.symbol_projections Name_occurrences.empty
  in
  Name.Map.fold
    (fun _name (ty, _binding_time) free_names ->
      let free_names_of_ty = Type_grammar.free_names ty in
      Name_occurrences.union free_names
        (Name_occurrences.restrict_to_value_slots_and_function_slots
           free_names_of_ty))
    t.names_to_types from_projections

let ids_for_export t =
  force_everything t;
  if not (Aliases.is_empty t.aliases)
  then
    Misc.fatal_error
      "Aliases structure must be empty for export; did you forget to call \
       [clean_for_export]?";
  Name.Map.fold
    (fun name (typ, _binding_time_and_mode) ids ->
      Ids_for_export.add_name
        (Ids_for_export.union ids (Type_grammar.ids_for_export typ))
        name)
    t.names_to_types Ids_for_export.empty
