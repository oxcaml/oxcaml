(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module MTC = More_type_creators
module TG = Type_grammar
module TE = Typing_env
module TEL = Typing_env_level

type 'a meet_return_value =
  | Left_input
  | Right_input
  | Both_inputs
  | New_result of 'a

type meet_type =
  TE.t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t meet_return_value * TE.t) Or_bottom.t

let replace_concrete_equation t name ty =
  match TG.must_be_singleton ty with
  | None ->
    (* [ty] must be a concrete type. *)
    (match TG.get_alias_opt ty with
    | None -> ()
    | Some alias ->
      Misc.fatal_errorf "Expected concrete type for %a but got an alias to %a"
        Name.print name Simple.print alias);
    TE.replace_equation t name ty
  | Some const -> (
    match
      TE.add_alias t ~canonical_element1:(Simple.name name)
        ~canonical_element2:(Simple.const const)
    with
    | Bottom ->
      Misc.fatal_error "Unexpected bottom while adding alias to constant"
    | Unknown ->
      (* This should only happen when adding aliases between names defined in
         external compilation units, but we are adding an alias to a
         constant. *)
      Misc.fatal_error "Unexpected failure while adding alias to constant"
    | Ok { canonical_element; demoted_name; t } ->
      if (not (Name.equal demoted_name name))
         || not (Simple.equal canonical_element (Simple.const const))
      then Misc.fatal_error "Unexpected demotion of constant.";
      let kind = MTC.kind_for_const const in
      let ty = TG.alias_type_of kind canonical_element in
      TE.replace_equation t demoted_name ty)

exception Bottom_equation

let add_concrete_equation_on_canonical ~raise_on_bottom t simple ty
    ~(meet_type : meet_type) =
  (* When adding a type to a canonical name, we need to call [meet] with the
     existing type for that name in order to ensure we record the most precise
     type available.

     For example, suppose [p] is defined earlier than [x], with [p] of type
     [ty1] and [x] of type [ty2]. If the caller says that the type of [p] is now
     to be "= x", then we will instead add a type "= p" on [x] and demote [x] to
     [p], due to the definition ordering. We then need to record the information
     that [p] now has type [ty1 meet ty2], otherwise the type [ty2] would be
     lost.

     If instead we say that the type of [p] is to be "= c", where [c] is a
     constant, we will add the type "= c" to [p] and demote [p] to [c]. We have
     no type to record for [c], however we still need to check that [c] is
     compatible with the previous type of [p].

     Note also that [p] and [x] may have different name modes! *)
  Simple.pattern_match simple
    ~const:(fun const ->
      match meet_type t ty (MTC.type_for_const const) with
      | Ok (_, env) -> env
      | Bottom -> if raise_on_bottom then raise Bottom_equation else t)
    ~name:(fun name ~coercion ->
      (* If [(coerce name coercion)] has type [ty], then [name] has type
         [(coerce ty coercion^-1)]. *)
      let ty = TG.apply_coercion ty (Coercion.inverse coercion) in
      (* Note: this will check that the [existing_ty] has the expected kind. *)
      let existing_ty = TE.find t name (Some (TG.kind ty)) in
      match meet_type t ty existing_ty with
      | Bottom ->
        if raise_on_bottom
        then raise Bottom_equation
        else TE.replace_equation t name (MTC.bottom (TG.kind ty))
      | Ok ((Right_input | Both_inputs), env) -> env
      | Ok (Left_input, env) -> replace_concrete_equation env name ty
      | Ok (New_result ty', env) -> replace_concrete_equation env name ty')

let record_demotion ~raise_on_bottom t kind demoted canonical ~meet_type =
  (* We have demoted [demoted], which used to be canonical, to [canonical] in
     the aliases structure.

     We now need to record that information in the types structure, and add the
     previous type of [demoted] to [canonical] to ensure we do not lose
     information that was only stored on the type of [demoted]. *)
  let ty_of_demoted = TE.find t demoted (Some kind) in
  (if Flambda_features.check_light_invariants ()
  then
    match TG.get_alias_opt ty_of_demoted with
    | None -> ()
    | Some alias ->
      Misc.fatal_errorf
        "Expected %a to have a concrete type, not an alias type to %a"
        Name.print demoted Simple.print alias);
  let t = TE.replace_equation t demoted (TG.alias_type_of kind canonical) in
  add_concrete_equation_on_canonical ~raise_on_bottom t canonical ty_of_demoted
    ~meet_type

let add_alias_between_canonicals ~raise_on_bottom t kind canonical_element1
    canonical_element2 ~meet_type =
  (* We are adding an equality between two canonical simples [canonical1] and
     [canonical2].

     We'll ask the aliases structure to record the equality and determine which
     of [canonical1] or [canonical2] should remain canonical, then forward to
     [record_demotion] which takes care of recording an alias type on the
     demoted element and updating the type of the element that remains
     canonical. *)
  if Simple.equal canonical_element1 canonical_element2
  then t
  else
    match TE.add_alias t ~canonical_element1 ~canonical_element2 with
    | Bottom -> if raise_on_bottom then raise Bottom_equation else t
    | Unknown ->
      (* Addition of aliases between names that are both in external compilation
         units failed, e.g. due to a missing .cmx file. Simply drop the
         equation. *)
      t
    | Ok { demoted_name; canonical_element; t } ->
      record_demotion ~raise_on_bottom t kind demoted_name canonical_element
        ~meet_type

let add_equation_on_canonical ~raise_on_bottom t simple ty ~meet_type =
  (* We are adding a type [ty] to [simple], which must be canonical. There are
     two general cases to consider:

     - Either [ty] is a concrete (non-alias) type, to be recorded in the types
     structure on the [canonical_simple];

     - or [ty] is an alias "= alias" to another simple, to be recorded in the
     aliases structure. *)
  match TG.get_alias_opt ty with
  | None ->
    add_concrete_equation_on_canonical ~raise_on_bottom t simple ty ~meet_type
  | Some alias ->
    let alias = TE.get_canonical_simple_ignoring_name_mode t alias in
    add_alias_between_canonicals ~raise_on_bottom t (TG.kind ty) simple alias
      ~meet_type

let add_equation_on_simple ~raise_on_bottom t simple ty ~meet_type =
  let canonical = TE.get_canonical_simple_ignoring_name_mode t simple in
  add_equation_on_canonical ~raise_on_bottom t canonical ty ~meet_type

let add_equation ~raise_on_bottom t name ty ~meet_type =
  add_equation_on_simple ~raise_on_bottom t (Simple.name name) ty ~meet_type

let add_env_extension ~raise_on_bottom t
    (env_extension : Typing_env_extension.t) ~meet_type =
  Typing_env_extension.fold
    ~equation:(fun name ty t ->
      add_equation ~raise_on_bottom t name ty ~meet_type)
    env_extension t

let add_env_extension_with_extra_variables t
    (env_extension : Typing_env_extension.With_extra_variables.t) ~meet_type =
  Typing_env_extension.With_extra_variables.fold
    ~variable:(fun var kind t ->
      TE.add_variable_definition t var kind Name_mode.in_types)
    ~equation:(fun name ty t ->
      try add_equation ~raise_on_bottom:true t name ty ~meet_type
      with Bottom_equation -> TE.make_bottom t)
    env_extension t

let add_env_extension_from_level t level ~meet_type : TE.t =
  let t =
    TEL.fold_on_defined_vars
      (fun var kind t ->
        TE.add_variable_definition t var kind Name_mode.in_types)
      level t
  in
  let t =
    Name.Map.fold
      (fun name ty t ->
        try add_equation ~raise_on_bottom:true t name ty ~meet_type
        with Bottom_equation -> TE.make_bottom t)
      (TEL.equations level) t
  in
  Variable.Map.fold
    (fun var proj t -> TE.add_symbol_projection t var proj)
    (TEL.symbol_projections level)
    t

let add_equation_strict t name ty ~meet_type : _ Or_bottom.t =
  if TE.is_bottom t
  then Bottom
  else
    try Ok (add_equation ~raise_on_bottom:true t name ty ~meet_type)
    with Bottom_equation -> Bottom

let add_env_extension_strict t env_extension ~meet_type : _ Or_bottom.t =
  if TE.is_bottom t
  then Bottom
  else
    try Ok (add_env_extension ~raise_on_bottom:true t env_extension ~meet_type)
    with Bottom_equation -> Bottom

let add_env_extension_maybe_bottom t env_extension ~meet_type =
  add_env_extension ~raise_on_bottom:false t env_extension ~meet_type

let add_equation t name ty ~meet_type =
  try add_equation ~raise_on_bottom:true t name ty ~meet_type
  with Bottom_equation -> TE.make_bottom t

let add_env_extension t env_extension ~meet_type =
  try add_env_extension ~raise_on_bottom:true t env_extension ~meet_type
  with Bottom_equation -> TE.make_bottom t

let check_params_and_types ~params ~param_types =
  if Flambda_features.check_invariants ()
     && List.compare_lengths (Bound_parameters.to_list params) param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameters.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space TG.print)
      param_types

let add_equations_on_params t ~params ~param_types ~meet_type =
  check_params_and_types ~params ~param_types;
  List.fold_left2
    (fun t param param_type ->
      add_equation t (Bound_parameter.name param) param_type ~meet_type)
    t
    (Bound_parameters.to_list params)
    param_types
