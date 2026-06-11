(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* [Simplify_import] cannot be used owing to a circular dependency. *)
module EA = Continuation_extra_params_and_args.Extra_arg
module EP = Flambda_primitive.Eligible_for_cse
module EPA = Continuation_extra_params_and_args
module K = Flambda_kind
module BP = Bound_parameter
module NM = Name_mode
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda2_types
module TE = Flambda2_types.Typing_env
module TEE = Flambda2_types.Typing_env_extension
module List = ListLabels

module T0 : sig
  (** A CSE map split into two halves: equations on primitives without coeffects
      (which can never be invalidated), and equations on primitives with
      coeffects (which can be invalidated by arbitrary effects, e.g. across a
      non-inlined function call). Keeping the coeffectful equations in a
      separate map lets us drop them all in O(1) and avoid scanning the
      non-coeffectful equations during the join filter. *)
  type sub = private
    { by_scope : Simple.t EP.Map.t Scope.Map.t;
      combined : Simple.t EP.Map.t
    }

  type t = private
    { no_coeffects : sub;
      coeffectful : sub
    }

  val print : Format.formatter -> t -> unit

  val empty : t

  val add : t -> EP.t -> bound_to:Simple.t -> Scope.t -> t

  val find : t -> EP.t -> Simple.t option

  val clear_equations_on_coeffectful_primitives : t -> t

  val has_coeffectful_equations : t -> bool

  (** Drop any equation on a coeffectful primitive that is not present in every
      [t] in [must_be_in]. Used by the join to ensure that a CSE equation
      present at the fork point is only propagated to the join point if it has
      not been killed (e.g. by a non-inlined function call) on any path to the
      join. Non-coeffectful equations cannot be killed and are kept as-is. *)
  val keep_only_equations_present_in_all : t -> must_be_in:t list -> t
end = struct
  type sub =
    { by_scope : Simple.t EP.Map.t Scope.Map.t;
      combined : Simple.t EP.Map.t
    }

  type t =
    { no_coeffects : sub;
      coeffectful : sub
    }

  let [@ocamlformat "disable"] print_sub ppf { by_scope; combined; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(by_scope@ %a)@]@ \
        @[<hov 1>(combined@ %a)@]\
        @]"
      (Scope.Map.print (EP.Map.print Simple.print)) by_scope
      (EP.Map.print Simple.print) combined

  let [@ocamlformat "disable"] print ppf { no_coeffects; coeffectful; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(no_coeffects@ %a)@]@ \
        @[<hov 1>(coeffectful@ %a)@]\
        @]"
      print_sub no_coeffects
      print_sub coeffectful

  let empty_sub = { by_scope = Scope.Map.empty; combined = EP.Map.empty }

  let empty = { no_coeffects = empty_sub; coeffectful = empty_sub }

  let primitive_has_coeffects prim = P.has_coeffects (EP.to_primitive prim)

  let add_sub sub prim ~bound_to scope =
    match EP.Map.find prim sub.combined with
    | exception Not_found ->
      let level =
        match Scope.Map.find scope sub.by_scope with
        | exception Not_found -> EP.Map.singleton prim bound_to
        | level -> EP.Map.add prim bound_to level
      in
      let by_scope = Scope.Map.add (* replace *) scope level sub.by_scope in
      let combined = EP.Map.add prim bound_to sub.combined in
      { by_scope; combined }
    | _bound_to -> sub

  let add t prim ~bound_to scope =
    if primitive_has_coeffects prim
    then { t with coeffectful = add_sub t.coeffectful prim ~bound_to scope }
    else { t with no_coeffects = add_sub t.no_coeffects prim ~bound_to scope }

  let find t prim =
    (* Avoid computing effects and coeffects here: a primitive can only ever be
       in one of the two maps. *)
    match EP.Map.find_opt prim t.no_coeffects.combined with
    | Some _ as result -> result
    | None -> EP.Map.find_opt prim t.coeffectful.combined

  let clear_equations_on_coeffectful_primitives t =
    if EP.Map.is_empty t.coeffectful.combined
    then t
    else { t with coeffectful = empty_sub }

  let has_coeffectful_equations t = not (EP.Map.is_empty t.coeffectful.combined)

  let keep_only_equations_present_in_all t ~must_be_in =
    (* Non-coeffectful equations cannot be killed on any path, so they are
       guaranteed to be present in all uses; only the coeffectful ones need to
       be checked. *)
    if EP.Map.is_empty t.coeffectful.combined
    then t
    else
      let keep prim bound_to =
        List.for_all must_be_in ~f:(fun other ->
            match EP.Map.find_opt prim other.coeffectful.combined with
            | None -> false
            | Some other_bound_to -> Simple.equal bound_to other_bound_to)
      in
      let combined = EP.Map.filter keep t.coeffectful.combined in
      if combined == t.coeffectful.combined
      then t
      else
        (* [combined] is the union of the levels in [by_scope], so membership in
           the filtered [combined] determines which equations to keep. *)
        let by_scope =
          Scope.Map.map
            (fun level ->
              EP.Map.filter (fun prim _ -> EP.Map.mem prim combined) level)
            t.coeffectful.by_scope
        in
        { t with coeffectful = { by_scope; combined } }
end

include T0

module Rhs_kind : sig
  type t =
    | Needs_extra_binding of { bound_to : Simple.t }
    | Rhs_in_scope of { bound_to : Simple.t }

  val bound_to : t -> Simple.t

  include Container_types.S with type t := t
end = struct
  type t =
    | Needs_extra_binding of { bound_to : Simple.t }
    | Rhs_in_scope of { bound_to : Simple.t }

  let bound_to t =
    match t with
    | Needs_extra_binding { bound_to } | Rhs_in_scope { bound_to } -> bound_to

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf t =
      match t with
      | Needs_extra_binding { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Needs_extra_binding@ %a)@]"
          Simple.print bound_to
      | Rhs_in_scope { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Rhs_in_scope@ %a)@]"
          Simple.print bound_to

    let hash _ = Misc.fatal_error "Rhs_kind.hash not yet implemented"

    let equal _ = Misc.fatal_error "Rhs_kind.equal not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | ( Needs_extra_binding { bound_to = bound_to1 },
          Needs_extra_binding { bound_to = bound_to2 } ) ->
        Simple.compare bound_to1 bound_to2
      | ( Rhs_in_scope { bound_to = bound_to1 },
          Rhs_in_scope { bound_to = bound_to2 } ) ->
        Simple.compare bound_to1 bound_to2
      | Needs_extra_binding _, _ -> -1
      | Rhs_in_scope _, _ -> 1
  end)
end

let cse_with_eligible_lhs ~typing_env_at_fork ~cse_at_each_use ~params prev_cse
    (extra_bindings : EPA.t) env_extension =
  let params_set =
    List.map params ~f:Bound_parameter.name |> Name.Set.of_list
  in
  let params = List.map params ~f:Bound_parameter.simple in
  let is_param simple =
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ -> Name.Set.mem name params_set)
      ~const:(fun _ -> false)
  in
  List.fold_left cse_at_each_use ~init:EP.Map.empty
    ~f:(fun eligible (env_at_use, id, cse) ->
      let find_new_name =
        let find_param simple params =
          List.find_opt
            ~f:(fun param ->
              match
                TE.get_canonical_simple_exn env_at_use param
                  ~min_name_mode:NM.normal
                  ~name_mode_of_existing_simple:NM.normal
              with
              | exception Not_found -> false
              | arg -> Simple.equal arg simple)
            params
        in
        match (extra_bindings : EPA.t) with
        | Empty -> fun arg -> find_param arg params
        | Non_empty { extra_args; extra_params } -> (
          match RI.Map.find id extra_args with
          | Invalid -> fun _arg -> None
          | Ok extra_args -> (
            let rec find_name simple params args =
              match args, params with
              | [], [] -> None
              | [], _ | _, [] ->
                Misc.fatal_error "Mismatching params and args arity"
              | arg :: args, param :: params -> (
                match (arg : EA.t) with
                | Already_in_scope arg when Simple.equal arg simple ->
                  (* If [param] has an extra equation associated to it, we
                     shouldn't propagate equations on it as it will mess with
                     the application of constraints later *)
                  if TEE.has_equation (BP.name param) env_extension
                  then None
                  else Some (BP.simple param)
                | Already_in_scope _ | New_let_binding _
                | New_let_binding_with_named_args _ ->
                  find_name simple params args)
            in
            fun arg ->
              match find_param arg params with
              | None ->
                find_name arg (Bound_parameters.to_list extra_params) extra_args
              | Some _ as r -> r))
      in
      EP.Map.fold
        (fun prim bound_to eligible ->
          let prim =
            EP.filter_map_args prim ~f:(fun arg ->
                match
                  TE.get_canonical_simple_exn env_at_use arg
                    ~min_name_mode:NM.normal
                    ~name_mode_of_existing_simple:NM.normal
                with
                | exception Not_found -> None
                | arg -> (
                  match find_new_name arg with
                  | None ->
                    if TE.mem_simple typing_env_at_fork arg
                    then Some arg
                    else None
                  | Some _ as arg_opt -> arg_opt))
          in
          match prim with
          | None -> eligible
          | Some prim when EP.Map.mem prim prev_cse ->
            (* We've already got it from a previous round *)
            eligible
          | Some prim -> (
            match
              TE.get_canonical_simple_exn env_at_use bound_to
                ~min_name_mode:NM.normal ~name_mode_of_existing_simple:NM.normal
            with
            | exception Not_found -> eligible
            | bound_to -> (
              let bound_to =
                (* CR-someday mshinwell: Think about whether this is the best
                   fix. The canonical simple might end up being one of the
                   [params] since they are defined in [env_at_fork]. However
                   these aren't bound at the use sites, so we must choose
                   another alias that is. *)
                if not (is_param bound_to)
                then Some bound_to
                else
                  let aliases =
                    TE.aliases_of_simple env_at_use ~min_name_mode:NM.normal
                      bound_to
                    |> TE.Alias_set.filter ~f:(fun simple ->
                        not (is_param simple))
                  in
                  (* CR-someday lmaurer: Do we need to make sure there's only
                     one alias? If not, we can use [Aliases.Alias_set.find_best]
                     here. *)
                  TE.Alias_set.get_singleton aliases
              in
              match bound_to with
              | None -> eligible
              | Some bound_to -> (
                let bound_to : Rhs_kind.t =
                  if TE.mem_simple typing_env_at_fork bound_to
                  then Rhs_in_scope { bound_to }
                  else Needs_extra_binding { bound_to }
                in
                (* CR-someday mshinwell: Add [Map.add_or_replace]. *)
                match EP.Map.find prim eligible with
                | exception Not_found ->
                  EP.Map.add prim (RI.Map.singleton id bound_to) eligible
                | from_prev_levels ->
                  let map = RI.Map.add id bound_to from_prev_levels in
                  EP.Map.add prim map eligible))))
        cse eligible)

type extra_binding =
  { extra_param : BP.t;
    extra_args : EA.t RI.Map.t
  }

let join_one_cse_equation ~cse_at_each_use prim bound_to_map
    (cse, extra_bindings, env_extension, allowed) =
  let has_value_on_all_paths =
    List.for_all cse_at_each_use ~f:(fun (_, id, _) ->
        RI.Map.mem id bound_to_map)
  in
  if not has_value_on_all_paths
  then cse, extra_bindings, env_extension, allowed
  else
    let bound_to_set = RI.Map.data bound_to_map |> Rhs_kind.Set.of_list in
    match Rhs_kind.Set.get_singleton bound_to_set with
    | Some (Rhs_kind.Rhs_in_scope { bound_to }) ->
      EP.Map.add prim bound_to cse, extra_bindings, env_extension, allowed
    | None | Some (Rhs_kind.Needs_extra_binding { bound_to = _ }) ->
      let prim_result_kind = P.result_kind' (EP.to_primitive prim) in
      let var = Variable.create "cse_param" prim_result_kind in
      let var_duid = Flambda_debug_uid.none in
      let extra_param =
        BP.create var (K.With_subkind.anything prim_result_kind) var_duid
      in
      let bound_to = RI.Map.map Rhs_kind.bound_to bound_to_map in
      let cse = EP.Map.add prim (Simple.var var) cse in
      let extra_args =
        RI.Map.map (fun simple : EA.t -> Already_in_scope simple) bound_to
      in
      let extra_binding = { extra_param; extra_args } in
      let env_extension =
        (* For the primitives Is_int and Get_tag, they're strongly linked to
           their argument: additional information on the cse parameter should
           translate into additional information on the argument. This can be
           done by giving them the appropriate type. The same could be done for
           a lot of the other non-arithmetic primitives, but in the other cases
           the join of the types will usually give us the relevant equation
           anyway. *)
        match[@ocaml.warning "-fragile-match"] EP.to_primitive prim with
        | Unary (Is_int { variant_only = true }, scrutinee) ->
          TEE.add_is_int_relation env_extension (Name.var var) ~scrutinee
        | Unary (Get_tag, block) ->
          TEE.add_get_tag_relation env_extension (Name.var var) ~scrutinee:block
        | _ -> env_extension
      in
      let allowed =
        Name_occurrences.add_name allowed (Name.var var) NM.normal
      in
      cse, (prim, extra_binding) :: extra_bindings, env_extension, allowed

let cut_cse_environment t ~scope_at_fork ~include_coeffectful =
  (* This extracts those CSE equations that arose between the fork point and
     each use of the continuation in question. *)
  let cut_sub (sub : T0.sub) =
    let _, _, levels = Scope.Map.split scope_at_fork sub.by_scope in
    Scope.Map.fold
      (fun scope equations result ->
        try EP.Map.disjoint_union equations result
        with Invalid_argument _ as exn ->
          Format.eprintf
            "cut_cse_environment failed:@ \n\
             t = %a@ \n\
             scope_at_fork = %a@ \n\
             scope = %a@ \n\
             equations = %a@ \n\
             result=%a@ \n\n"
            print t Scope.print scope_at_fork Scope.print scope
            (EP.Map.print Simple.print)
            equations
            (EP.Map.print Simple.print)
            result;
          raise exn)
      levels EP.Map.empty
  in
  let no_coeffects = cut_sub t.no_coeffects in
  if not include_coeffectful
  then no_coeffects
  else EP.Map.disjoint_union no_coeffects (cut_sub t.coeffectful)

module Join_result = struct
  type nonrec t =
    { cse_at_join_point : t;
      extra_params : EPA.t;
      env_extension : TEE.t;
      extra_allowed_names : Name_occurrences.t
    }
end

let join0 ~typing_env_at_fork ~cse_at_fork ~cse_at_each_use ~params
    ~scope_at_fork =
  let params = Bound_parameters.to_list params in
  (* CSE equations have a left-hand side specifying a primitive and a right-hand
     side specifying a [Simple]. The left-hand side is matched against portions
     of terms. As such, the [Simple]s therein must have name mode [Normal],
     since we do not do CSE for phantom bindings (see [Simplify_common]). It
     follows that any CSE equation whose left-hand side involves a name not
     defined at the fork point, having canonicalised such name, cannot be
     propagated. This step also canonicalises the right-hand sides of the CSE
     equations. *)
  let compute_cse_one_round prev_cse extra_params typing_env_with_extra_params
      env_extension ~allowed =
    let new_cse =
      cse_with_eligible_lhs ~typing_env_at_fork ~cse_at_each_use ~params
        prev_cse extra_params env_extension
    in
    (* To make use of a CSE equation at or after the join point, its right-hand
       side must have the same value, no matter which path is taken from the
       fork point to the join point. We filter out equations that do not satisfy
       this. Sometimes we can force an equation to satisfy the property by
       explicitly passing the value of the right-hand side as an extra parameter
       to the continuation at the join point. *)
    let cse', extra_bindings, env_extension', allowed =
      EP.Map.fold
        (join_one_cse_equation ~cse_at_each_use)
        new_cse
        (EP.Map.empty, [], TEE.empty, allowed)
    in
    let sorted_extra_bindings =
      List.sort extra_bindings ~cmp:(fun (prim1, _) (prim2, _) ->
          P.compare_primitive_application
            ~compare_simple:
              (TE.stable_compare_simples typing_env_with_extra_params)
            (EP.to_primitive prim1) (EP.to_primitive prim2))
    in
    let extra_params', typing_env_with_extra_params' =
      List.fold_left sorted_extra_bindings
        ~init:(EPA.empty, typing_env_with_extra_params)
        ~f:(fun
            (extra_bindings, typing_env_with_extra_params)
            (_, { extra_param; extra_args })
          ->
          let extra_bindings =
            EPA.add extra_bindings ~extra_param ~extra_args
              ~invalids:Apply_cont_rewrite_id.Set.empty
          in
          (* We need to add the new variable to the typing env in order to be
             able to compute binding times for the following rounds. *)
          let typing_env_with_extra_params =
            TE.add_definition typing_env_with_extra_params
              (Bound_name.create (BP.name extra_param) Name_mode.normal)
              (K.With_subkind.kind (BP.kind extra_param))
          in
          extra_bindings, typing_env_with_extra_params)
    in
    let need_other_round =
      (* If we introduce new parameters, then CSE equations involving the
         corresponding arguments can be considered again, so we need another
         round. *)
      not (EPA.is_empty extra_params')
    in
    let cse = EP.Map.disjoint_union prev_cse cse' in
    (* The order of cse arguments does not matter since only simples already in
       scope are used as extra arguments. *)
    let extra_params = EPA.concat ~outer:extra_params' ~inner:extra_params in
    let env_extension = TEE.disjoint_union env_extension env_extension' in
    ( cse,
      extra_params,
      typing_env_with_extra_params',
      env_extension,
      allowed,
      need_other_round )
  in
  let cse, extra_params, env_extension, allowed =
    let rec do_rounds current_round cse extra_params
        typing_env_with_extra_params env_extension allowed =
      let ( cse,
            extra_params,
            typing_env_with_extra_params,
            env_extension,
            allowed,
            need_other_round ) =
        compute_cse_one_round cse extra_params typing_env_with_extra_params
          env_extension ~allowed
      in
      if need_other_round && current_round < Flambda_features.cse_depth ()
      then
        do_rounds (succ current_round) cse extra_params
          typing_env_with_extra_params env_extension allowed
      else
        ( (* Either a fixpoint has been reached or we've already explored far
             enough *)
          cse,
          extra_params,
          env_extension,
          allowed )
    in
    do_rounds 1 EP.Map.empty EPA.empty typing_env_at_fork TEE.empty
      Name_occurrences.empty
  in
  let have_propagated_something = ref false in
  let cse_at_join_point =
    (* Any CSE equation whose right-hand side identifies a name in the [allowed]
       set is propagated. We don't need to check the left-hand sides because we
       know all of those names are in [typing_env_at_fork]. *)
    EP.Map.fold
      (fun prim bound_to cse ->
        let propagate =
          Simple.pattern_match bound_to
            ~const:(fun _ -> true)
            ~name:(fun name ~coercion:_ ->
              Name_occurrences.mem_name allowed name)
        in
        if not propagate
        then cse
        else (
          have_propagated_something := true;
          add cse prim ~bound_to (Scope.next scope_at_fork)))
      cse cse_at_fork
  in
  if not !have_propagated_something
  then None
  else
    Some
      { Join_result.cse_at_join_point;
        extra_params;
        env_extension;
        extra_allowed_names = allowed
      }

let join ~typing_env_at_fork ~cse_at_fork ~is_recursive ~use_info
    ~get_typing_env ~get_rewrite_id ~get_cse ~params =
  let scope_at_fork = TE.current_scope typing_env_at_fork in
  let cse_at_fork =
    if is_recursive
    then
      (* The handler of a recursive continuation (and anything reachable from
         it, in particular via the recursive uses, which have not been seen yet)
         may perform arbitrary effectful operations, so no equation on a
         coeffectful primitive may be propagated into the handler. *)
      clear_equations_on_coeffectful_primitives cse_at_fork
    else if has_coeffectful_equations cse_at_fork
    then
      (* Drop from [cse_at_fork] any equation that does not survive on every
         path to the join point (e.g. because of a non-inlined function call or
         an effectful primitive). Without this, an equation present at the fork
         would be propagated to the join point even though it has been
         killed. *)
      keep_only_equations_present_in_all cse_at_fork
        ~must_be_in:(List.map use_info ~f:get_cse)
    else cse_at_fork
  in
  let no_equations = ref false in
  let cse_at_each_use =
    List.map use_info ~f:(fun use ->
        let t = get_cse use in
        (* For recursive continuations the coeffectful equations are excluded
           here too, so that [join0] does not construct extra parameters for
           equations that could never be used in the handler (see above). *)
        let cse_between_fork_and_use =
          cut_cse_environment t ~scope_at_fork
            ~include_coeffectful:(not is_recursive)
        in
        (* If one branch doesn't have any equations, then the join is going to
           be empty *)
        if EP.Map.is_empty cse_between_fork_and_use then no_equations := true;
        get_typing_env use, get_rewrite_id use, cse_between_fork_and_use)
  in
  let result =
    if !no_equations
    then None
    else
      join0 ~typing_env_at_fork ~cse_at_fork ~cse_at_each_use ~params
        ~scope_at_fork
  in
  match result with
  | Some result -> result
  | None ->
    (* Even when no equations could be joined, [cse_at_fork] must become the
       join point's CSE state: equations on coeffectful primitives may have been
       removed from it (above), and they would incorrectly reappear if the
       caller were instead to fall back to the environment at the fork point. *)
    { Join_result.cse_at_join_point = cse_at_fork;
      extra_params = EPA.empty;
      env_extension = TEE.empty;
      extra_allowed_names = Name_occurrences.empty
    }
