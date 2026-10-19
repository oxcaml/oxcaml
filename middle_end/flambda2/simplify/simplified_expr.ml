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
    cse_before_switch : CSE.t
  }

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
    ~shareable_constants ~typing_env_before_switch ~cse_before_switch =
  simplified_terminator
    (Simplified_switch
       { arms;
         condition_dbg;
         scrutinee;
         scrutinee_ty;
         shareable_constants;
         typing_env_before_switch;
         cse_before_switch
       })

let simplified_invalid invalid =
  simplified_terminator (Simplified_invalid invalid)
