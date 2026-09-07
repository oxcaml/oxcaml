(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Basile Clément, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { params : Bound_parameters.t;
    continuation : Continuation.t;
    args : Simple.t list;
    check_actions : Check_action.t list
  }

let[@ocamlformat "disable"] print ppf
    { params; continuation; args; check_actions } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(params@ %a)@]@ \
      @[<hov 1>(continuation@ %a)@]@ \
      @[<hov 1>(args@ %a)@]@ \
      @[<hov 1>(check_actions@ %a)@]\
    )@]"
    Bound_parameters.print params
    Continuation.print continuation
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       Simple.print)
    args
    (Format.pp_print_list Check_action.print)
    check_actions

let create ~params ~check_actions continuation args =
  { params; continuation; args; check_actions }

let continuation { continuation; _ } = continuation

let apply { params; continuation; args; check_actions } shortcut_args =
  let subst =
    List.fold_left2
      (fun subst param arg -> Variable.Map.add param arg subst)
      Variable.Map.empty
      (Bound_parameters.vars params)
      shortcut_args
  in
  let subst_simple arg =
    Simple.pattern_match' arg
      ~var:(fun var ~coercion ->
        match Variable.Map.find_or_null var subst with
        | Null -> arg
        | This simple -> Simple.apply_coercion_exn simple coercion)
      ~symbol:(fun _ ~coercion:_ -> arg)
      ~const:(fun _ -> arg)
  in
  let check_actions =
    List.map
      (fun (check_action : Check_action.t) ->
        match check_action with
        | Close_alloc_region { region; exit } ->
          let region = subst_simple (Simple.var region) in
          let region =
            match Simple.must_be_var region with
            | Some (region, _) -> region
            | None ->
              Misc.fatal_errorf
                "Region parameter substituted by a non-variable in a \
                 continuation shortcut:@ %a"
                Simple.print region
          in
          Check_action.Close_alloc_region { region; exit })
      check_actions
  in
  continuation, List.map subst_simple args, check_actions

let to_alias t =
  let params = Bound_parameters.simples t.params in
  if
    List.is_empty t.check_actions
    && Misc.Stdlib.List.equal Simple.equal t.args params
  then Some t.continuation
  else None
