(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Guillaume Bury, OCamlPro                 *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Continuations = Permutation.Make [@inlined hint] (Continuation)
module Variables = Permutation.Make [@inlined hint] (Variable)
module Coercion = Int_ids.Coercion
module Const = Reg_width_const
module Simple = Int_ids.Simple

module Import_map : sig
  type t

  val create :
    symbols:Symbol.importer ->
    variables:Variable.importer ->
    simples:Simple.importer ->
    consts:Const.importer ->
    code_ids:Code_id.importer ->
    continuations:Continuation.importer ->
    used_value_slots:Value_slot.Set.t ->
    original_compilation_unit:Compilation_unit.t ->
    t

  val const : t -> Const.t -> Const.t

  val variable : t -> Variable.t -> Variable.t

  val fresh_variable : t -> Variable.t -> Variable.t

  val variables : t -> Variable.importer

  val symbol : t -> Symbol.t -> Symbol.t

  val symbols : t -> Symbol.importer

  val simple :
    t -> Simple.t -> import_var:(Variable.t -> Variable.t) -> Simple.t

  val code_id : t -> Code_id.t -> Code_id.t

  val continuation : t -> Continuation.t -> Continuation.t

  val fresh_continuation : t -> Continuation.t -> Continuation.t

  val value_slot_is_used : t -> Value_slot.t -> bool
end = struct
  type t =
    { symbols : Symbol.importer;
      variables : Variable.importer;
      simples : Simple.importer;
      consts : Const.importer;
      code_ids : Code_id.importer;
      continuations : Continuation.importer;
      used_value_slots : Value_slot.Set.t;
      (* CR vlaviron: [used_value_slots] is here because we need to rewrite the
         types to remove occurrences of unused value slots, as otherwise the
         types can contain references to code that is neither exported nor
         present in the actual object file. But this means rewriting types, and
         the only place a rewriting traversal is done at the moment is during
         import. This solution is not ideal because the missing code IDs will
         still be present in the emitted cmx files, and during the traversal in
         [Flambda_cmx.compute_reachable_names_and_code] we have to assume that
         code IDs can be missing (and so we cannot detect code IDs that are
         really missing at this point). *)
      (* CR lmaurer: We should consider storing the _unused_ value slots rather
         than the used ones. This is the only place in this file where a bigger
         set means _fewer_ changes, and it means we can never know when an
         import map will have no effect (see PR #1398). *)
      original_compilation_unit : Compilation_unit.t
          (* This complements [used_value_slots]. Removal of value slots is only
             allowed for variables that are not used in the compilation unit
             they are defined in. *)
    }

  let create ~symbols ~variables ~simples ~consts ~code_ids ~continuations
      ~used_value_slots ~original_compilation_unit =
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
      continuations;
      used_value_slots;
      original_compilation_unit
    }

  let symbol t orig = Symbol.import t.symbols orig

  let symbols t = t.symbols

  let variable t orig = Variable.import t.variables orig

  let variables t = t.variables

  let fresh_variable t orig = Variable.import_and_rename t.variables orig

  let const t orig = Const.import t.consts orig

  let code_id t orig = Code_id.import t.code_ids orig

  let continuation t orig = Continuation.import t.continuations orig

  let fresh_continuation t orig =
    Continuation.import_and_rename t.continuations orig

  let simple t simple ~import_var =
    (* Constants and symbols are never permuted, only freshened upon import. *)
    Simple.import t.simples simple
      ~import_const:(fun orig -> const t orig)
      ~import_symbol:(fun orig -> symbol t orig)
      ~import_var

  let value_slot_is_used t var =
    if Value_slot.in_compilation_unit var t.original_compilation_unit
    then Value_slot.Set.mem var t.used_value_slots
    else (* This value slot might be used in other units *)
      true
end

type import_map = Import_map.t

let imported_variables = Import_map.variables

let imported_symbols = Import_map.symbols

type t =
  { continuations : Continuations.t;
    variables : Variables.t;
    import_map : Import_map.t option;
    (* [bound_variables] and [bound_continuations] replace importing and must
       not go through the import map. *)
    bound_variables : Variable.t Variable.Map.t;
    bound_continuations : Continuation.t Continuation.Map.t
  }

let empty =
  { continuations = Continuations.empty;
    variables = Variables.empty;
    import_map = None;
    bound_variables = Variable.Map.empty;
    bound_continuations = Continuation.Map.empty
  }

let create_import_map ~symbols ~variables ~simples ~consts ~code_ids
    ~continuations ~used_value_slots ~original_compilation_unit =
  Import_map.create ~symbols ~variables ~simples ~consts ~code_ids
    ~continuations ~used_value_slots ~original_compilation_unit

let from_import_map import_map =
  (* It's tempting to set [import_map] to [None] if everything is empty, but
     this is incorrect: an import map of [None] is equivalent to having _all_
     value slots used, not none (see [value_slot_is_used]). *)
  { empty with import_map = Some import_map }

let has_import_map t = Option.is_some t.import_map

let [@ocamlformat "disable"] print ppf
      { continuations; variables; import_map = _;
        bound_variables; bound_continuations } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(bound_continuations@ %a)@])@ \
      @[<hov 1>(variables@ %a)@])@ \
      @[<hov 1>(bound_variables@ %a)@])@ \
      @]"
    Continuations.print continuations
    (Continuation.Map.print Continuation.print) bound_continuations
    Variables.print variables
    (Variable.Map.print Variable.print) bound_variables

let is_identity
    { continuations;
      variables;
      import_map;
      bound_variables;
      bound_continuations
    } =
  Continuations.is_empty continuations
  && Variables.is_empty variables
  && Continuation.Map.is_empty bound_continuations
  && Variable.Map.is_empty bound_variables
  &&
  match import_map with
  | None -> true
  | Some _ ->
    (* If there is any import map at all, then this renaming is not necessarily
       the identity: any value slots _not_ present in [used_value_slots] will be
       removed from closures. *)
    false

let compose0
    ~second:
      ({ continuations = continuations2;
         variables = variables2;
         import_map = import_map2;
         bound_variables = bound_variables2;
         bound_continuations = bound_continuations2
       } as second)
    ~first:
      ({ continuations = continuations1;
         variables = variables1;
         import_map = import_map1;
         bound_variables = bound_variables1;
         bound_continuations = bound_continuations1
       } as first) =
  { continuations =
      Continuations.compose ~second:continuations2 ~first:continuations1;
    variables = Variables.compose ~second:variables2 ~first:variables1;
    (* The process of simplification of terms together with the collection of
       [Ids_for_export] from types, prior to writing of .cmx files, should
       ensure that only [first] (and not [second]) has an import map. *)
    import_map =
      (match import_map1, import_map2 with
      | None, None -> None
      | Some _, None -> import_map1
      | (None | Some _), Some _ ->
        Misc.fatal_errorf
          "Cannot compose renamings; only the [first] renaming may have an \
           import map.  first:@ %a@ second:@ %a"
          print first print second);
    bound_variables =
      (if Variable.Map.is_empty bound_variables2
       then bound_variables1
       else
         Misc.fatal_errorf
           "Cannot compose renamings; only the [first] renaming may have bound \
            variables.  first:@ %a@ second:@ %a"
           print first print second);
    bound_continuations =
      (if Continuation.Map.is_empty bound_continuations2
       then bound_continuations1
       else
         Misc.fatal_errorf
           "Cannot compose renamings; only the [first] renaming may have bound \
            continuations.  first:@ %a@ second:@ %a"
           print first print second)
  }

let compose ~second ~first =
  if is_identity second
  then first
  else if is_identity first
  then second
  else compose0 ~second ~first

let add_variable t var1 var2 =
  { t with variables = Variables.compose_one ~first:t.variables var1 var2 }

let add_fresh_variable t var1 ~guaranteed_fresh:var2 =
  { t with
    variables = Variables.compose_one_fresh t.variables var1 ~fresh:var2
  }

let apply_variable t var =
  let var =
    match t.import_map with
    | None -> var
    | Some import_map -> (
      match Variable.Map.find_or_null var t.bound_variables with
      | This var -> var
      | Null -> Import_map.variable import_map var)
  in
  Variables.apply t.variables var

let bind_variable t var1 =
  match t.import_map with
  | None ->
    let var2 = Variable.rename var1 in
    add_fresh_variable t (apply_variable t var1) ~guaranteed_fresh:var2, var2
  | Some import_map ->
    let var2 = Import_map.fresh_variable import_map var1 in
    let bound_variables = Variable.Map.add var1 var2 t.bound_variables in
    { t with bound_variables }, var2

let apply_variable_set t vars =
  Variable.Set.fold
    (fun var result ->
      let var = apply_variable t var in
      Variable.Set.add var result)
    vars Variable.Set.empty

let apply_symbol t symbol =
  match t.import_map with
  | None -> symbol
  | Some import_map -> Import_map.symbol import_map symbol

let apply_symbol_set t symbols =
  Symbol.Set.fold
    (fun symbol result ->
      let symbol = apply_symbol t symbol in
      Symbol.Set.add symbol result)
    symbols Symbol.Set.empty

let apply_name t name =
  Name.pattern_match name
    ~var:(fun var -> Name.var (apply_variable t var))
    ~symbol:(fun symbol -> Name.symbol (apply_symbol t symbol))

let add_continuation t k1 k2 =
  { t with
    continuations = Continuations.compose_one ~first:t.continuations k1 k2
  }

let add_fresh_continuation t k1 ~guaranteed_fresh:k2 =
  { t with
    continuations = Continuations.compose_one_fresh t.continuations k1 ~fresh:k2
  }

let apply_continuation t k =
  let k =
    match t.import_map with
    | None -> k
    | Some import_map -> (
      match Continuation.Map.find_or_null k t.bound_continuations with
      | This k -> k
      | Null -> Import_map.continuation import_map k)
  in
  Continuations.apply t.continuations k

let bind_continuation t k1 =
  match t.import_map with
  | None ->
    let k2 = Continuation.rename k1 in
    add_fresh_continuation t (apply_continuation t k1) ~guaranteed_fresh:k2, k2
  | Some import_map ->
    let k2 = Import_map.fresh_continuation import_map k1 in
    let bound_continuations =
      Continuation.Map.add k1 k2 t.bound_continuations
    in
    { t with bound_continuations }, k2

let apply_code_id t code_id =
  match t.import_map with
  | None -> code_id
  | Some import_map -> Import_map.code_id import_map code_id

let apply_const t cst =
  match t.import_map with
  | None -> cst
  | Some import_map -> Import_map.const import_map cst

let apply_simple t simple =
  match t.import_map with
  | None ->
    (* Constants are never permuted, only freshened upon import. *)
    let[@inline always] const cst = Simple.const (apply_const t cst) in
    let[@inline always] name old_name ~coercion:old_coercion =
      let new_name = apply_name t old_name in
      let new_coercion =
        Coercion.map_depth_variables old_coercion ~f:(fun dv ->
            apply_variable t dv)
      in
      if old_name == new_name && old_coercion == new_coercion
      then simple
      else Simple.with_coercion (Simple.name new_name) new_coercion
    in
    Simple.pattern_match simple ~name ~const
  | Some import_map ->
    (* This is a bit tricky -- we want to be able to use [apply_variable] here
       so the variables in [Import_map.simple] cannot have been imported yet. *)
    Import_map.simple import_map simple ~import_var:(fun var ->
        apply_variable t var)

let value_slot_is_used t value_slot =
  match t.import_map with
  | None -> true (* N.B. not false! *)
  | Some import_map -> Import_map.value_slot_is_used import_map value_slot
