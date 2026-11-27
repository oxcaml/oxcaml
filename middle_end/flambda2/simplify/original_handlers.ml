(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Recursive of
      { invariant_params : Bound_parameters.t;
        lifted_params : Bound_parameters.t;
        lifted_free_vars : Lifted_free_var.t list;
        continuation_handlers : One_recursive_handler.t Continuation.Lmap.t
      }
  | Non_recursive of Non_recursive_handler.t

let[@ocamlformat "disable"] print ppf t =
  match t with
  | Non_recursive non_rec_handler ->
    Non_recursive_handler.print ppf non_rec_handler
  | Recursive
      { invariant_params; lifted_params; lifted_free_vars;
        continuation_handlers; } ->
    Format.fprintf ppf "@[<hov 1>(\
        @[<hv 1>(invariant params@ %a)@]@ \
        @[<hv 1>(lifted_params@ %a)@]@ \
        @[<hv 1>(lifted_free_vars@ %a)@]@ \
        @[<hv 1>(continuation handlers@ %a)@]\
      )@]"
      Bound_parameters.print invariant_params Bound_parameters.print
      lifted_params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Lifted_free_var.print)
      lifted_free_vars
      (Continuation.Lmap.print One_recursive_handler.print)
      continuation_handlers

let create_recursive ~invariant_params ~lifted_params ~lifted_free_vars
    ~continuation_handlers =
  Recursive
    { invariant_params; lifted_params; lifted_free_vars; continuation_handlers }

let create_non_recursive non_rec_handler = Non_recursive non_rec_handler

let bind_phantom_vars phantom_vars handler =
  List.fold_right
    (fun bound_var body ->
      if not (Name_mode.is_phantom (Bound_var.name_mode bound_var))
      then
        Misc.fatal_errorf
          "Variable %a must be bound at phantom name mode, but is not"
          Bound_var.print bound_var;
      let kind = Variable.kind (Bound_var.var bound_var) in
      Flambda.Let.create
        (Bound_pattern.singleton bound_var)
        (Flambda.Named.create_prim (Nullary (Optimised_out kind)) Debuginfo.none)
        ~body ~free_names_of_body:Unknown
      |> Flambda.Expr.create_let)
    phantom_vars handler

let add_params_to_lift t (lifted_free_vars : Lifted_free_var.t list)
    ~phantom_vars =
  let renaming, lifted_params =
    Lifted_free_var.rename_list_as_bound_parameters lifted_free_vars
  in
  let add_lifted_params ~is_exn_handler params =
    Bound_parameters.create
      (Lifted_free_var.place_lifted ~is_exn_handler
         ~lifted:(Bound_parameters.to_list lifted_params)
         (Bound_parameters.to_list params))
  in
  let lift handler =
    bind_phantom_vars phantom_vars
      (Flambda.Expr.apply_renaming handler renaming)
  in
  let[@inline] fail () =
    Misc.fatal_errorf
      "Cannot add lifted params to a continuation that already has lifted \
       params:@ lifted_free_vars=%a@ t=%a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Lifted_free_var.print)
      lifted_free_vars print t
  in
  match t with
  | Recursive
      { invariant_params;
        lifted_params = _;
        lifted_free_vars = old_free_vars;
        continuation_handlers
      } ->
    if not (List.is_empty old_free_vars) then fail ();
    let continuation_handlers =
      Continuation.Lmap.map
        (fun ({ params; handler; is_cold } : One_recursive_handler.t) ->
          One_recursive_handler.create ~params ~handler:(lift handler) ~is_cold)
        continuation_handlers
    in
    Recursive
      { invariant_params =
          add_lifted_params ~is_exn_handler:false invariant_params;
        lifted_params;
        lifted_free_vars;
        continuation_handlers
      }
  | Non_recursive
      { cont;
        params;
        lifted_params = _;
        lifted_free_vars = old_free_vars;
        handler;
        is_exn_handler;
        is_cold
      } ->
    if not (List.is_empty old_free_vars) then fail ();
    Non_recursive
      (Non_recursive_handler.create ~cont
         ~params:(add_lifted_params ~is_exn_handler params)
         ~lifted_params ~lifted_free_vars ~handler:(lift handler)
         ~is_exn_handler ~is_cold)
