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
module Code_ids = Permutation.Make [@inlined hint] (Code_id)
module Symbols = Permutation.Make [@inlined hint] (Symbol)
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

  val symbol : t -> Symbol.t -> Symbol.t

  val simple :
    t ->
    Simple.t ->
    name:(Name.t -> coercion:Coercion.t -> 'a) ->
    const:(Const.t -> 'a) ->
    'a

  val code_id : t -> Code_id.t -> Code_id.t

  val continuation : t -> Continuation.t -> Continuation.t

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

  let variable t orig = Variable.import t.variables orig

  let const t orig = Const.import t.consts orig

  let code_id t orig = Code_id.import t.code_ids orig

  let continuation t orig = Continuation.import t.continuations orig

  let simple t simple ~name ~const =
    (* [t.simples] only holds those [Simple]s with [Coercion] (analogously to
       the grand table of [Simple]s, see int_ids.ml). *)
    Simple.import t.simples simple ~name ~const

  let value_slot_is_used t var =
    if Value_slot.in_compilation_unit var t.original_compilation_unit
    then Value_slot.Set.mem var t.used_value_slots
    else (* This value slot might be used in other units *)
      true
end

type t =
  { continuations : Continuations.t;
    variables : Variables.t;
    code_ids : Code_ids.t;
    symbols : Symbols.t;
    import_map : Import_map.t option
  }

let empty =
  { continuations = Continuations.empty;
    variables = Variables.empty;
    code_ids = Code_ids.empty;
    symbols = Symbols.empty;
    import_map = None
  }

let create_import_map ~symbols ~variables ~simples ~consts ~code_ids
    ~continuations ~used_value_slots ~original_compilation_unit =
  let import_map =
    Import_map.create ~symbols ~variables ~simples ~consts ~code_ids
      ~continuations ~used_value_slots ~original_compilation_unit
  in
  (* It's tempting to set [import_map] to [None] if everything is empty, but
     this is incorrect: an import map of [None] is equivalent to having _all_
     value slots used, not none (see [value_slot_is_used]). *)
  { empty with import_map = Some import_map }

let has_import_map t = Option.is_some t.import_map

let [@ocamlformat "disable"] print ppf
      { continuations; variables; code_ids; symbols; import_map = _; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(variables@ %a)@])@ \
      @[<hov 1>(code_ids@ %a)@])@ \
      @[<hov 1>(symbols@ %a)@])@ \
      @]"
    Continuations.print continuations
    Variables.print variables
    Code_ids.print code_ids
    Symbols.print symbols

let is_identity { continuations; variables; code_ids; symbols; import_map } =
  Continuations.is_empty continuations
  && Variables.is_empty variables
  && Code_ids.is_empty code_ids && Symbols.is_empty symbols
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
         code_ids = code_ids2;
         symbols = symbols2;
         import_map = import_map2
       } as second)
    ~first:
      ({ continuations = continuations1;
         variables = variables1;
         code_ids = code_ids1;
         symbols = symbols1;
         import_map = import_map1
       } as first) =
  { continuations =
      Continuations.compose ~second:continuations2 ~first:continuations1;
    variables = Variables.compose ~second:variables2 ~first:variables1;
    code_ids = Code_ids.compose ~second:code_ids2 ~first:code_ids1;
    symbols = Symbols.compose ~second:symbols2 ~first:symbols1;
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
    | Some import_map -> Import_map.variable import_map var
  in
  Variables.apply t.variables var

let apply_variable_set t vars =
  Variable.Set.fold
    (fun var result ->
      let var = apply_variable t var in
      Variable.Set.add var result)
    vars Variable.Set.empty

let add_symbol t symbol1 symbol2 =
  { t with symbols = Symbols.compose_one ~first:t.symbols symbol1 symbol2 }

let add_fresh_symbol t symbol1 ~guaranteed_fresh:symbol2 =
  { t with
    symbols = Symbols.compose_one_fresh t.symbols symbol1 ~fresh:symbol2
  }

let apply_symbol t symbol =
  let symbol =
    match t.import_map with
    | None -> symbol
    | Some import_map -> Import_map.symbol import_map symbol
  in
  Symbols.apply t.symbols symbol

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
    | Some import_map -> Import_map.continuation import_map k
  in
  Continuations.apply t.continuations k

let add_code_id t code_id1 code_id2 =
  { t with code_ids = Code_ids.compose_one ~first:t.code_ids code_id1 code_id2 }

let add_fresh_code_id t code_id1 ~guaranteed_fresh:code_id2 =
  { t with
    code_ids = Code_ids.compose_one_fresh t.code_ids code_id1 ~fresh:code_id2
  }

let apply_code_id t code_id =
  let code_id =
    match t.import_map with
    | None -> code_id
    | Some import_map -> Import_map.code_id import_map code_id
  in
  Code_ids.apply t.code_ids code_id

let apply_const t cst =
  match t.import_map with
  | None -> cst
  | Some import_map -> Import_map.const import_map cst

let apply_simple t simple =
  (* Constants are never permuted, only freshened upon import. *)
  let[@inline always] const cst = Simple.const (apply_const t cst) in
  match t.import_map with
  | None ->
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
    let[@inline always] name old_name ~coercion:old_coercion =
      let new_name = apply_name t old_name in
      let new_coercion =
        Coercion.map_depth_variables old_coercion ~f:(fun dv ->
            apply_variable t dv)
      in
      Simple.with_coercion (Simple.name new_name) new_coercion
    in
    Import_map.simple import_map simple ~name ~const

let value_slot_is_used t value_slot =
  match t.import_map with
  | None -> true (* N.B. not false! *)
  | Some import_map -> Import_map.value_slot_is_used import_map value_slot
