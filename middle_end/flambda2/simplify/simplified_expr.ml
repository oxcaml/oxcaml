(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Flambda.Import
module CSE = Common_subexpression_elimination
module EPA = Continuation_extra_params_and_args
module T = Flambda2_types
module TE = T.Typing_env
module LCS = Lifted_constant_state

type t =
  { simplified_lets_and_let_conts : simplified_lets_and_let_conts;
    simplified_terminator : simplified_terminator;
    removed_operations : Removed_operations.t;
    is_cold : bool
  }

and simplified_lets_and_let_conts =
  | Simplified_terminator
  | Simplified_let of simplified_defining_expr * simplified_lets_and_let_conts
  | Simplified_let_cont of
      simplified_let_cont_handlers * simplified_lets_and_let_conts

and simplified_defining_expr =
  { bindings_to_place : Simplified_named.binding_to_place list;
    removed_operations : Removed_operations.t;
    lifted_constants_from_defining_expr : LCS.t;
    at_unit_toplevel : bool;
    closure_info : Closure_info.t;
    rewrite_id : Named_rewrite_id.t
  }

and simplified_handler =
  { params : Bound_parameters.t;
    simplified_handler : t;
    is_exn_handler : bool;
    is_cold : bool;
    extra_params_and_args : EPA.t;
    (* Note: EPA.extra_params invariant_extra_params_and_args should always be
       equal to invariant_extra_params in stage4 *)
    invariant_extra_params_and_args : EPA.t;
    rewrite_ids : Apply_cont_rewrite_id.Set.t
  }

and simplified_handlers_group =
  | Recursive of
      { simplified_continuation_handlers : simplified_handler Continuation.Map.t
      }
  | Non_recursive of
      { cont : Continuation.t;
        handler : simplified_handler;
        is_single_inlinable_use : bool
      }

and simplified_let_cont_handlers =
  { at_unit_toplevel : bool;
    handlers_from_the_outside_to_the_inside : simplified_handlers_group list;
    original_invariant_params : Bound_parameters.t;
    invariant_extra_params : Bound_parameters.t
  }

and simplified_apply =
  { apply : Apply.t;
    use_id : Apply_cont_rewrite_id.t option;
    exn_cont_use_id : Apply_cont_rewrite_id.t;
    result_arity : [`Complex] Flambda_arity.t;
    is_cold : bool
  }

and simplified_terminator =
  | Simplified_apply of simplified_apply
  | Simplified_apply_cont of simplified_apply_cont
  | Simplified_switch of simplified_switch
  | Simplified_invalid of Flambda.Invalid.t

and simplified_apply_cont =
  { apply_cont : Apply_cont.t;
    args : Simple.t list;
    rewrite_id : Apply_cont_rewrite_id.t
  }

and simplified_switch =
  { arms :
      (Apply_cont_expr.t
      * annotations:Branch_annotations.t
      * Apply_cont_rewrite_id.t
      * [`Unarized] Flambda_arity.t
      * TE.t)
      Target_ocaml_int.Map.t;
    condition_dbg : Debuginfo.t;
    scrutinee : Simple.t;
    scrutinee_ty : T.t;
    shareable_constants : Symbol.t Static_const.Map.t;
    typing_env_before_switch : TE.t;
    cse_before_switch : CSE.t;
    fdo_region : Fdo_region_counters.Region.t option
        (* The region the switch is in (see [Fdo_region_counters]). *)
  }

let print_conf_removed_operations =
  Oxcaml_args.Extra_options.bool __LOC__ "dsimplified-removed-operations"

let print_elide pp ppf x =
  Format.fprintf ppf "%t<%a>%t" Flambda_colours.elide pp x Flambda_colours.pop

let print_flag ppf (b, name) =
  if b then print_elide Format.pp_print_string ppf name

let print_flags ppf l =
  List.iter (print_flag ppf) l;
  if List.exists fst l then Format.pp_print_space ppf ()

type print_ctx = { mutable epa : EPA.t Continuation.Map.t }

let rec print ~ctx ppf
    { simplified_lets_and_let_conts;
      simplified_terminator;
      removed_operations;
      is_cold
    } =
  Format.fprintf ppf "@[<hv>%a%a%a%a@]" print_flag (is_cold, "cold")
    print_removed_operations removed_operations
    (print_lets_and_let_conts ~ctx)
    simplified_lets_and_let_conts (print_terminator ~ctx) simplified_terminator

and print_rewrite_id_exn ppf id =
  Format.fprintf ppf "%t<exn:%a>%t" Flambda_colours.elide
    Apply_cont_rewrite_id.print id Flambda_colours.pop

and print_toplevel ppf toplevel =
  if toplevel
  then Format.fprintf ppf "%t{top}%t" Flambda_colours.elide Flambda_colours.pop
  else ()

and print_removed_operations ppf removed_operations =
  if not (print_conf_removed_operations ())
  then ()
  else begin
    Format.fprintf ppf "removed_operations: [@[<hov>%a@]]@ "
      Removed_operations.print removed_operations
  end

and print_lets_and_let_conts ~ctx ppf = function
  | Simplified_terminator -> ()
  | Simplified_let (let_expr, body) -> print_let_expr ~ctx ppf ~body let_expr
  | Simplified_let_cont (let_cont, body) ->
    print_let_cont ~ctx ppf ~body let_cont

and print_let_expr ~ctx ~body ppf
    { bindings_to_place;
      removed_operations = _;
      lifted_constants_from_defining_expr;
      at_unit_toplevel = _;
      closure_info = _;
      rewrite_id
    } =
  Format.fprintf ppf "@[<hv 2>let%a@ %a%a@;<1 -2>in@]@\n"
    (print_elide Named_rewrite_id.print)
    rewrite_id print_lifted_constants lifted_constants_from_defining_expr
    print_bindings_to_place bindings_to_place;
  print_lets_and_let_conts ~ctx ppf body

and print_lifted_constants ppf lcs =
  LCS.fold lcs ~init:() ~f:(fun () lc ->
      let defs = Lifted_constant.definitions lc in
      List.iter
        (fun def ->
          let expr = Lifted_constant.Definition.defining_expr def in
          match Lifted_constant.Definition.descr def with
          | Code code_id ->
            Format.fprintf ppf "@[<hov 2>%a = <code>@]@\n" Code_id.print code_id
          | Set_of_closures
              { typing_env = _;
                closure_symbols_with_types;
                symbol_projections = _
              } ->
            let symbols =
              Function_slot.Lmap.data closure_symbols_with_types |> List.map fst
            in
            Format.fprintf ppf "@[<hv 2>%a =@ %a@]@\n"
              print_set_of_closures_symbols symbols Rebuilt_static_const.print
              expr
          | Block_like
              { symbol; typing_env = _; ty = _; symbol_projections = _ } ->
            Format.fprintf ppf "@[<hv 2>%a = @ %a@]@\n" Symbol.print symbol
              Rebuilt_static_const.print expr)
        defs)

and print_set_of_closures_symbols ppf = function
  | [] -> assert false (* CR gbury: this should never happen I think *)
  | [symbol] -> Symbol.print ppf symbol
  | _ :: _ as symbols ->
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Symbol.print)
      symbols

and print_bindings_to_place ppf = function
  | [] ->
    Format.fprintf ppf "%t{empty}%t" Flambda_colours.elide Flambda_colours.pop
  | _ :: _ as l ->
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
      print_binding_to_place ppf l

and print_binding_to_place ppf binding =
  match (binding : Simplified_named.binding_to_place) with
  | Delete_binding { original_defining_expr = _ } ->
    Format.fprintf ppf "%t<deleted_binding>%t" Flambda_colours.elide
      Flambda_colours.pop
  | Keep_binding
      { let_bound; simplified_defining_expr; original_defining_expr = _ } ->
    Format.fprintf ppf "@[<hv 2>%a =@ %a@]" print_bound_pattern let_bound
      Simplified_named.print simplified_defining_expr

and print_bound_pattern ppf bound_pattern =
  let colour =
    match (bound_pattern : Bound_pattern.t) with
    | Singleton _ -> Flambda_colours.variable
    | Set_of_closures _ | Static _ -> Flambda_colours.none
  in
  Format.fprintf ppf "%t%a%t" colour Bound_pattern.print bound_pattern
    Flambda_colours.pop

and print_let_cont ~ctx ~body ppf
    { at_unit_toplevel;
      handlers_from_the_outside_to_the_inside = handlers;
      original_invariant_params;
      invariant_extra_params
    } =
  Format.fprintf ppf "@[<hv 2>let_cont%a@ %a@;<1 -2>in@]@\n" print_toplevel
    at_unit_toplevel
    (print_handlers ~ctx ~original_invariant_params ~invariant_extra_params)
    handlers;
  print_lets_and_let_conts ~ctx ppf body

and print_handlers ~ctx ~original_invariant_params ~invariant_extra_params ppf =
  function
  | [] ->
    Format.fprintf ppf "%t{empty}%t" Flambda_colours.elide Flambda_colours.pop
  | _ :: _ as l ->
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
      (print_handler_group ~ctx ~original_invariant_params
         ~invariant_extra_params)
      ppf l

and print_handler_group ~ctx ~original_invariant_params ~invariant_extra_params
    ppf = function
  | Recursive { simplified_continuation_handlers } ->
    print_recursive_handlers ~ctx ~original_invariant_params
      ~invariant_extra_params ppf simplified_continuation_handlers
  | Non_recursive { cont; handler; is_single_inlinable_use } ->
    print_one_handler ~ctx ~original_invariant_params ~invariant_extra_params
      ppf
      (cont, handler, is_single_inlinable_use, ~recursive:false)

and print_recursive_handlers ~ctx ~original_invariant_params
    ~invariant_extra_params ppf map =
  Continuation.Map.iter
    (fun cont handler ->
      Format.fprintf ppf "%a@ "
        (print_one_handler ~ctx ~original_invariant_params
           ~invariant_extra_params)
        (cont, handler, false, ~recursive:true))
    map

and print_one_handler ~ctx ~original_invariant_params ~invariant_extra_params
    ppf (cont, handler, is_single_inlinable_use, ~recursive) =
  let { params;
        simplified_handler;
        is_exn_handler;
        is_cold;
        extra_params_and_args;
        invariant_extra_params_and_args = _;
        rewrite_ids = _
      } =
    handler
  in
  ctx.epa <- Continuation.Map.add cont extra_params_and_args ctx.epa;
  let extra_params = EPA.extra_params extra_params_and_args in
  Format.fprintf ppf "@[<hov 2>%a%s%a@[<hv>%a@] =@ %a@]" print_flags
    [is_single_inlinable_use, "single"; is_exn_handler, "exn"; is_cold, "cold"]
    (if recursive then "rec " else "")
    Continuation.print cont print_cont_params_lists
    (if recursive
     then
       [ ~extra:false, original_invariant_params;
         ~extra:true, invariant_extra_params;
         ~extra:false, params;
         ~extra:true, extra_params ]
     else [~extra:false, params; ~extra:true, extra_params])
    (print ~ctx) simplified_handler

and print_cont_params_lists ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,")
    print_cont_params ppf l

and print_cont_params ppf (~extra, bound_params) =
  let sep_begin, sep_end = if extra then "{", "}" else "(", ")" in
  Format.fprintf ppf "%s%a%s" sep_begin Bound_parameters.print bound_params
    sep_end

and print_terminator ~ctx ppf terminator : unit =
  match terminator with
  | Simplified_apply apply_expr -> (print_apply_expr ~ctx) ppf apply_expr
  | Simplified_apply_cont apply_cont -> (print_apply_cont ~ctx) ppf apply_cont
  | Simplified_switch switch -> print_switch ppf switch
  | Simplified_invalid invalid -> print_invalid ppf invalid

and print_apply_expr ~ctx:_ ppf
    { apply; use_id; exn_cont_use_id; result_arity = _; is_cold } =
  (* CR gbury: print the extra params and args for the return conts of the
     apply *)
  Format.fprintf ppf "%a%a%a%a" print_flag (is_cold, "cold")
    (Format.pp_print_option (print_elide Apply_cont_rewrite_id.print))
    use_id print_rewrite_id_exn exn_cont_use_id Apply.print apply

and print_apply_cont ~ctx ppf { apply_cont; args; rewrite_id } =
  Format.fprintf ppf "@[<hv 2>%a%a%a@]"
    (print_elide Apply_cont_rewrite_id.print)
    rewrite_id Apply_cont.print
    (Apply_cont.update_args apply_cont ~args)
    (print_apply_cont_extra_params_and_args ~ctx)
    (apply_cont, rewrite_id)

and print_apply_cont_extra_params_and_args ~ctx ppf (apply_cont, rewrite_id) =
  let extra_params_and_args_here =
    match
      Continuation.Map.find_opt (Apply_cont.continuation apply_cont) ctx.epa
    with
    | None -> Or_invalid.Ok []
    | Some epa -> (
      let extra_params = EPA.extra_params epa in
      let extra_args =
        Apply_cont_rewrite_id.Map.find rewrite_id (EPA.extra_args epa)
      in
      match extra_args with
      | Invalid -> Or_invalid.Invalid
      | Ok l ->
        Or_invalid.Ok (List.combine (Bound_parameters.to_list extra_params) l))
  in
  match extra_params_and_args_here with
  | Or_invalid.Ok [] -> ()
  | Or_invalid.Invalid ->
    Format.fprintf ppf "%t{invalid}%t" Flambda_colours.invalid_keyword
      Flambda_colours.pop
  | Or_invalid.Ok (_ :: _ as l) ->
    Format.fprintf ppf "@,{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         print_extra_param_and_arg)
      l

and print_extra_arg ppf extra_arg =
  match (extra_arg : EPA.Extra_arg.t) with
  | Already_in_scope simple -> Simple.print ppf simple
  | New_let_binding (v, prim) ->
    Format.fprintf ppf "%a=%a" Variable.print v Flambda_primitive.print prim
  | New_let_binding_with_named_args _ ->
    (* CR gbury: consider printing this in some way *)
    Format.fprintf ppf "<new_let_binding_with_named_args>"

and print_extra_param_and_arg ppf (param, extra_arg) =
  Format.fprintf ppf "@[<hov>%a ->@ %a@]" Bound_parameter.print param
    print_extra_arg extra_arg

and print_switch ppf
    { arms;
      scrutinee;
      scrutinee_ty = _;
      condition_dbg;
      shareable_constants = _;
      typing_env_before_switch = _;
      cse_before_switch = _
    } =
  Format.fprintf ppf "@[<v 0>%tswitch%t %a%s%t%a%t@ %a@]"
    Flambda_colours.expr_keyword Flambda_colours.pop Simple.print scrutinee
    (if Debuginfo.is_none condition_dbg then "" else " ")
    Flambda_colours.debuginfo Debuginfo.print_compact condition_dbg
    Flambda_colours.pop print_switch_arms arms

and print_switch_arms ppf arms =
  Target_ocaml_int.Map.iter (print_switch_arm ppf) arms

and print_switch_arm ppf discr
    (apply_cont, ~annotations:_, rewrite_id, _arity, _typing_env) =
  Format.fprintf ppf "| %a -> @[<hov>%a%a@]@," Target_ocaml_int.print discr
    (print_elide Apply_cont_rewrite_id.print)
    rewrite_id Apply_cont_expr.print apply_cont

and print_invalid ppf invalid =
  Format.fprintf ppf "@[<hov 1>(%tinvalid%t@ %s@]"
    Flambda_colours.invalid_keyword Flambda_colours.pop
    (Flambda.Invalid.to_string invalid)

let print = print ~ctx:{ epa = Continuation.Map.empty }

let notify_removed ~operation t =
  { t with
    removed_operations = Removed_operations.( + ) operation t.removed_operations
  }

let mark_as_cold t = if t.is_cold then t else { t with is_cold = true }

let simplified_let ~bindings_to_place ~removed_operations
    ~lifted_constants_from_defining_expr ~at_unit_toplevel ~closure_info
    ~rewrite_id t =
  { t with
    simplified_lets_and_let_conts =
      Simplified_let
        ( { bindings_to_place;
            removed_operations;
            lifted_constants_from_defining_expr;
            at_unit_toplevel;
            closure_info;
            rewrite_id
          },
          t.simplified_lets_and_let_conts )
  }

let simplified_let_cont simplified_handlers t =
  { t with
    simplified_lets_and_let_conts =
      Simplified_let_cont (simplified_handlers, t.simplified_lets_and_let_conts)
  }

let simplified_terminator simplified_terminator =
  { simplified_lets_and_let_conts = Simplified_terminator;
    simplified_terminator;
    removed_operations = Removed_operations.zero;
    is_cold = false
  }

let simplified_apply ?result_arity apply ~use_id ~exn_cont_use_id ~is_cold =
  let result_arity =
    match result_arity with
    | None -> Apply_expr.return_arity apply
    | Some result_arity -> result_arity
  in
  simplified_terminator
    (Simplified_apply { apply; use_id; exn_cont_use_id; result_arity; is_cold })

let simplified_apply_cont apply_cont ~args ~rewrite_id =
  simplified_terminator (Simplified_apply_cont { apply_cont; args; rewrite_id })

let simplified_switch ~arms ~condition_dbg ~scrutinee ~scrutinee_ty
    ~shareable_constants ~typing_env_before_switch ~cse_before_switch
    ~fdo_region =
  simplified_terminator
    (Simplified_switch
       { arms;
         condition_dbg;
         scrutinee;
         scrutinee_ty;
         shareable_constants;
         typing_env_before_switch;
         cse_before_switch;
         fdo_region
       })

let simplified_invalid invalid =
  simplified_terminator (Simplified_invalid invalid)
