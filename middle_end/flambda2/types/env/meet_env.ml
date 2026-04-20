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
module K = Flambda_kind
module ET = Expand_head.Expanded_type

type t =
  { typing_env : TE.t;
    adding_equations_for_names : Name.Set.t
  }

type 'a meet_return_value =
  | Left_input
  | Right_input
  | Both_inputs
  | New_result of 'a

type 'a meet_result =
  | Bottom of unit meet_return_value
  | Ok of 'a meet_return_value * t

type meet_expanded_head = t -> ET.t -> ET.t -> ET.t meet_result

let map_result ~f = function
  | Bottom r -> Bottom r
  | Ok (Left_input, env) -> Ok (Left_input, env)
  | Ok (Right_input, env) -> Ok (Right_input, env)
  | Ok (Both_inputs, env) -> Ok (Both_inputs, env)
  | Ok (New_result x, env) -> Ok (New_result (f x), env)

let create typing_env =
  { typing_env; adding_equations_for_names = Name.Set.empty }

let typing_env { typing_env; _ } = typing_env

let code_age_relation env = TE.code_age_relation (typing_env env)

let code_age_relation_resolver env =
  TE.code_age_relation_resolver (typing_env env)

let machine_width env = TE.machine_width (typing_env env)

let with_typing_env t typing_env = { t with typing_env }

let use_meet_env t ~f = typing_env (f (create t))

let use_meet_env_strict t ~f : _ Or_bottom.t =
  if TE.is_bottom t
  then Bottom
  else
    let t = f (create t) in
    let tenv = typing_env t in
    if TE.is_bottom tenv then Bottom else Ok tenv

let map_typing_env t ~f = with_typing_env t (f (typing_env t))

let adding_equation_for_name t name ~f =
  (* If we were to add an equation on [x] while already adding an equation on
     [x], either the inner equation would get overriden by the outer equation
     and would not be visible, or it would get captured in an env extension.

     That env extension would then likely end up stored on [x] itself, which
     currently would cause an infinite loop next time we try to add an equation
     on [x].

     Instead, we simply ignore such recursive equations. *)
  (* CR bclement: Implement support for recursive extensions (i.e. extensions
     stored on [x] that can contain equations on [x]), then get rid of this. *)
  if Name.Set.mem name t.adding_equations_for_names
  then t
  else
    let adding_equations_for_names =
      Name.Set.add name t.adding_equations_for_names
    in
    let t' = f { t with adding_equations_for_names } in
    { t' with adding_equations_for_names = t.adding_equations_for_names }

let replace_concrete_equation t name ty =
  match TG.must_be_singleton ty ~machine_width:(TE.machine_width t) with
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
      if
        (not (Name.equal demoted_name name))
        || not (Simple.equal canonical_element (Simple.const const))
      then Misc.fatal_error "Unexpected demotion of constant.";
      let kind = MTC.kind_for_const const in
      let ty = TG.alias_type_of kind canonical_element in
      TE.replace_equation t demoted_name ty)

exception Bottom_equation

let add_concrete_equation_on_canonical ~raise_on_bottom t simple ty
    ~(meet_expanded_head : meet_expanded_head) =
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
      match meet_expanded_head t ty (ET.create_const const) with
      | Ok (_, env) -> env
      | Bottom _ -> if raise_on_bottom then raise Bottom_equation else t)
    ~name:(fun name ~coercion ->
      adding_equation_for_name t name ~f:(fun t ->
          let kind = TG.kind (ET.to_type ty) in
          (* Note: this will check that the [existing_ty] has the expected
             kind. *)
          let existing_ty_of_name = TE.find (typing_env t) name (Some kind) in
          (* If [name] has type [existing_ty], then [(coerce name coercion)] has
             type [(coerce ty coercion)]. *)
          let existing_ty_of_simple =
            TG.apply_coercion existing_ty_of_name coercion
          in
          let existing_ty =
            Expand_head.expand_head0 (typing_env t) existing_ty_of_simple
              ~known_canonical_simple_at_in_types_mode:(Some simple)
          in
          match meet_expanded_head t ty existing_ty with
          | Bottom _ ->
            if raise_on_bottom
            then raise Bottom_equation
            else
              map_typing_env t ~f:(fun t ->
                  TE.replace_equation t name (MTC.bottom kind))
          | Ok ((Right_input | Both_inputs), env) -> env
          | Ok (Left_input, env) ->
            map_typing_env env ~f:(fun env ->
                replace_concrete_equation env name (ET.to_type ty))
          | Ok (New_result ty', env) ->
            map_typing_env env ~f:(fun env ->
                replace_concrete_equation env name (ET.to_type ty'))))

let record_demotion ~raise_on_bottom t kind demoted canonical
    ~meet_expanded_head =
  (* We have demoted [demoted], which used to be canonical, to [canonical] in
     the aliases structure.

     We now need to record that information in the types structure, and add the
     previous type of [demoted] to [canonical] to ensure we do not lose
     information that was only stored on the type of [demoted]. *)
  let ty_of_demoted = TE.find (typing_env t) demoted (Some kind) in
  (if Flambda_features.check_light_invariants ()
   then
     match TG.get_alias_opt ty_of_demoted with
     | None -> ()
     | Some alias ->
       Misc.fatal_errorf
         "Expected %a to have a concrete type, not an alias type to %a"
         Name.print demoted Simple.print alias);
  let t =
    map_typing_env t ~f:(fun t ->
        TE.replace_equation t demoted (TG.alias_type_of kind canonical))
  in
  let ty_of_demoted =
    Expand_head.expand_head0 (typing_env t) ty_of_demoted
      ~known_canonical_simple_at_in_types_mode:(Some (Simple.name demoted))
  in
  add_concrete_equation_on_canonical ~raise_on_bottom t canonical ty_of_demoted
    ~meet_expanded_head

let add_alias_between_canonicals ~raise_on_bottom t kind canonical_element1
    canonical_element2 ~meet_expanded_head =
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
    match
      TE.add_alias (typing_env t) ~canonical_element1 ~canonical_element2
    with
    | Bottom -> if raise_on_bottom then raise Bottom_equation else t
    | Unknown ->
      (* Addition of aliases between names that are both in external compilation
         units failed, e.g. due to a missing .cmx file. Simply drop the
         equation. *)
      t
    | Ok { demoted_name; canonical_element; t = typing_env } ->
      let t = with_typing_env t typing_env in
      record_demotion ~raise_on_bottom t kind demoted_name canonical_element
        ~meet_expanded_head

let add_equation_on_canonical ~raise_on_bottom t simple ty ~meet_expanded_head =
  (* We are adding a type [ty] to [simple], which must be canonical. There are
     two general cases to consider:

     - Either [ty] is a concrete (non-alias) type, to be recorded in the types
     structure on the [canonical_simple];

     - or [ty] is an alias "= alias" to another simple, to be recorded in the
     aliases structure. *)
  match TG.get_alias_opt ty with
  | None ->
    let ty = ET.of_non_alias_type ty in
    add_concrete_equation_on_canonical ~raise_on_bottom t simple ty
      ~meet_expanded_head
  | Some alias ->
    let alias =
      TE.get_canonical_simple_ignoring_name_mode (typing_env t) alias
    in
    add_alias_between_canonicals ~raise_on_bottom t (TG.kind ty) simple alias
      ~meet_expanded_head

let add_equation_on_simple ~raise_on_bottom t simple ty ~meet_expanded_head =
  let canonical =
    TE.get_canonical_simple_ignoring_name_mode (typing_env t) simple
  in
  add_equation_on_canonical ~raise_on_bottom t canonical ty ~meet_expanded_head

let add_equation ~raise_on_bottom t name ty ~meet_expanded_head =
  add_equation_on_simple ~raise_on_bottom t (Simple.name name) ty
    ~meet_expanded_head

let add_env_extension ~raise_on_bottom t
    (env_extension : Typing_env_extension.t) ~meet_expanded_head =
  Typing_env_extension.fold
    ~equation:(fun name ty t ->
      add_equation ~raise_on_bottom t name ty ~meet_expanded_head)
    env_extension t

let add_env_extension_with_extra_variables t
    (env_extension : Typing_env_extension.With_extra_variables.t)
    ~meet_expanded_head =
  Typing_env_extension.With_extra_variables.fold
    ~variable:(fun var kind t ->
      map_typing_env t ~f:(fun t ->
          TE.add_variable_definition t var kind Name_mode.in_types))
    ~equation:(fun name ty t ->
      try add_equation ~raise_on_bottom:true t name ty ~meet_expanded_head
      with Bottom_equation -> map_typing_env ~f:TE.make_bottom t)
    env_extension t

let add_env_extension_from_level t level ~meet_expanded_head =
  let t =
    map_typing_env t ~f:(fun t ->
        TEL.fold_on_defined_vars
          (fun var kind t ->
            TE.add_variable_definition t var kind Name_mode.in_types)
          level t)
  in
  let t =
    Name.Map.fold
      (fun name ty t ->
        try add_equation ~raise_on_bottom:true t name ty ~meet_expanded_head
        with Bottom_equation -> map_typing_env ~f:TE.make_bottom t)
      (TEL.equations level) t
  in
  map_typing_env t ~f:(fun t ->
      Variable.Map.fold
        (fun var proj t -> TE.add_symbol_projection t var proj)
        (TEL.symbol_projections level)
        t)

let add_equation_strict t name ty ~meet_expanded_head : _ Or_bottom.t =
  if TE.is_bottom (typing_env t)
  then Bottom
  else
    try Ok (add_equation ~raise_on_bottom:true t name ty ~meet_expanded_head)
    with Bottom_equation -> Bottom

let add_env_extension_strict t env_extension ~meet_expanded_head : _ Or_bottom.t
    =
  if TE.is_bottom (typing_env t)
  then Bottom
  else
    try
      Ok
        (add_env_extension ~raise_on_bottom:true t env_extension
           ~meet_expanded_head)
    with Bottom_equation -> Bottom

let add_env_extension_maybe_bottom t env_extension ~meet_expanded_head =
  add_env_extension ~raise_on_bottom:false t env_extension ~meet_expanded_head

let add_equation t name ty ~meet_expanded_head =
  try add_equation ~raise_on_bottom:true t name ty ~meet_expanded_head
  with Bottom_equation -> map_typing_env ~f:TE.make_bottom t

let add_equation_on_simple t simple ty ~meet_expanded_head =
  try
    add_equation_on_simple ~raise_on_bottom:true t simple ty ~meet_expanded_head
  with Bottom_equation -> map_typing_env ~f:TE.make_bottom t

let add_env_extension t env_extension ~meet_expanded_head =
  try
    add_env_extension ~raise_on_bottom:true t env_extension ~meet_expanded_head
  with Bottom_equation -> map_typing_env ~f:TE.make_bottom t

let check_params_and_types ~params ~param_types =
  if
    Flambda_features.check_invariants ()
    && List.compare_lengths (Bound_parameters.to_list params) param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameters.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space TG.print)
      param_types

let add_equations_on_params t ~params ~param_types ~meet_expanded_head =
  check_params_and_types ~params ~param_types;
  List.fold_left2
    (fun t param param_type ->
      add_equation t (Bound_parameter.name param) param_type ~meet_expanded_head)
    t
    (Bound_parameters.to_list params)
    param_types

let current_scope env = TE.current_scope (typing_env env)

let increment_scope env = map_typing_env env ~f:TE.increment_scope

let add_definition env bound_name kind =
  map_typing_env env ~f:(fun env -> TE.add_definition env bound_name kind)

let add_symbol_projection env var symbol_projection =
  map_typing_env env ~f:(fun env ->
      TE.add_symbol_projection env var symbol_projection)

let cut env ~cut_after = TE.cut (typing_env env) ~cut_after

let cut_as_extension env ~cut_after =
  TE.cut_as_extension (typing_env env) ~cut_after

let add_variable_definition env var kind name_mode =
  map_typing_env env ~f:(fun env ->
      TE.add_variable_definition env var kind name_mode)

let add_alias ~raise_on_bottom env simple1 simple2 ~meet_expanded_head =
  let canonical1 =
    TE.get_canonical_simple_ignoring_name_mode (typing_env env) simple1
  in
  let canonical2 =
    TE.get_canonical_simple_ignoring_name_mode (typing_env env) simple2
  in
  add_alias_between_canonicals ~raise_on_bottom env (Simple.kind canonical1)
    canonical1 canonical2 ~meet_expanded_head

let add_alias env simple1 simple2 ~meet_expanded_head : _ Or_bottom.t =
  match
    add_alias ~raise_on_bottom:true env simple1 simple2 ~meet_expanded_head
  with
  | exception Bottom_equation -> Bottom
  | env -> Ok env

let meet env (t1 : TG.t) (t2 : TG.t) ~(meet_expanded_head : meet_expanded_head)
    : ET.t meet_result =
  (* Kind mismatches should have been caught (either turned into Invalid or a
     fatal error) before we get here. *)
  if not (K.equal (TG.kind t1) (TG.kind t2))
  then
    Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a" TG.print t1
      TG.print t2;
  let kind = TG.kind t1 in
  let tenv = typing_env env in
  let simple1 =
    match
      TE.get_alias_then_canonical_simple_exn tenv t1
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let simple2 =
    match
      TE.get_alias_then_canonical_simple_exn tenv t2
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  match simple1 with
  | None -> (
    let expanded1 =
      Expand_head.expand_head0 tenv t1
        ~known_canonical_simple_at_in_types_mode:simple1
    in
    match simple2 with
    | None ->
      let expanded2 =
        Expand_head.expand_head0 tenv t2
          ~known_canonical_simple_at_in_types_mode:simple2
      in
      meet_expanded_head env expanded1 expanded2
    | Some simple2 -> (
      (* Here we are meeting a non-alias type on the left with an alias on the
         right. In all cases, the return type is the alias, so we will always
         return [Right_input]; the interesting part will be the environment.

         [add_equation] will meet [expanded1] with the existing type of
         [simple2]. *)
      match
        add_concrete_equation_on_canonical ~raise_on_bottom:true env simple2
          expanded1 ~meet_expanded_head
      with
      | exception Bottom_equation -> Bottom (New_result ())
      | env -> Ok (Right_input, env)))
  | Some simple1 -> (
    match simple2 with
    | None -> (
      let expanded2 =
        Expand_head.expand_head0 tenv t2
          ~known_canonical_simple_at_in_types_mode:simple2
      in
      (* We always return [Left_input] (see comment above) *)
      match
        add_concrete_equation_on_canonical ~raise_on_bottom:true env simple1
          expanded2 ~meet_expanded_head
      with
      | exception Bottom_equation -> Bottom (New_result ())
      | env -> Ok (Left_input, env))
    | Some simple2 -> (
      (* We are doing a meet between two alias types. Whatever happens, the
         resulting environment will contain an alias equation between the two
         inputs, so both the left-hand alias and the right-hand alias are
         correct results for the meet, allowing us to return [Both_inputs] in
         all cases.

         [add_alias_between_canonicals] will have called [meet] on the
         underlying types, so [env] now contains all extra equations arising
         from meeting the expanded heads.

         Note that [add_alias_between_canonicals] does nothing if [simple1] and
         [simple2] are equal. *)
      match
        add_alias_between_canonicals ~raise_on_bottom:true env kind simple1
          simple2 ~meet_expanded_head
      with
      | exception Bottom_equation -> Bottom (New_result ())
      | env -> Ok (Both_inputs, env)))

(* CR bclement: is this wrapper really necessary? *)
let[@inline always] meet_type env t1 t2 ~meet_expanded_head : TG.t meet_result =
  map_result ~f:ET.to_type
    ((meet [@inlined never]) env t1 t2 ~meet_expanded_head)
