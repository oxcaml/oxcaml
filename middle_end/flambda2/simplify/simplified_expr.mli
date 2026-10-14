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

type t = private
  { simplified_lets_and_let_conts : simplified_lets_and_let_conts;
    simplified_terminator : simplified_terminator;
    removed_operations : Removed_operations.t
  }

and simplified_lets_and_let_conts = private
  | Simplified_terminator
  | Simplified_let of simplified_defining_expr * simplified_lets_and_let_conts
  | Simplified_let_cont of
      simplified_let_cont_handlers * simplified_lets_and_let_conts

and simplified_defining_expr =
  private
  (* CR-soon bclement: it seems like some fields don't actually have their place
     here. [removed_operations] could be accumulated in the basic block
     directly, and [at_unit_toplevel] and [closure_info] could be stored there
     as well (something went very wrong if these change in the middle of a basic
     block). *)
  { bindings_to_place : Simplified_named.binding_to_place list;
    removed_operations : Removed_operations.t;
    lifted_constants_from_defining_expr : Lifted_constant_state.t;
    at_unit_toplevel : bool;
    closure_info : Closure_info.t;
    rewrite_id : Named_rewrite_id.t
  }

and simplified_handler =
  { params : Bound_parameters.t;
    simplified_handler : t;
    is_exn_handler : bool;
    is_cold : bool;
    extra_params_and_args : Continuation_extra_params_and_args.t;
    (* Note: EPA.extra_params invariant_extra_params_and_args should always be
       equal to invariant_extra_params in stage4 *)
    invariant_extra_params_and_args : Continuation_extra_params_and_args.t;
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

and simplified_apply = private
  | Simplified_non_ocaml_function_call of
      { apply : Apply.t;
        use_id : Apply_cont_rewrite_id.t option;
        exn_cont_use_id : Apply_cont_rewrite_id.t
      }
  | Simplified_function_call_where_callee's_type_unavailable of
      { apply : Apply.t;
        use_id : Apply_cont_rewrite_id.t option;
        exn_cont_use_id : Apply_cont_rewrite_id.t
      }
  | Simplified_non_inlined_direct_full_application of
      { apply : Apply.t;
        use_id : Apply_cont_rewrite_id.t option;
        exn_cont_use_id : Apply_cont_rewrite_id.t;
        result_arity : [`Unarized] Flambda_arity.t;
        coming_from_indirect : bool;
        callee's_code_metadata : Code_metadata.t
      }

and simplified_terminator = private
  | Simplified_apply of simplified_apply
  | Simplified_apply_cont of simplified_apply_cont
  | Simplified_switch of simplified_switch
  | Simplified_invalid of Flambda.Invalid.t

and simplified_apply_cont = private
  { apply_cont : Apply_cont.t;
    args : Simple.t list;
    rewrite_id : Apply_cont_rewrite_id.t
  }

and simplified_switch = private
  { arms :
      (Apply_cont_expr.t
      * Apply_cont_rewrite_id.t
      * [`Unarized] Flambda_arity.t
      * Typing_env.t)
      Target_ocaml_int.Map.t;
    condition_dbg : Debuginfo.t;
    scrutinee : Simple.t;
    scrutinee_ty : flambda_type;
    shareable_constants : Symbol.t Static_const.Map.t;
    typing_env_before_switch : Typing_env.t;
    cse_before_switch : Common_subexpression_elimination.t
  }

val notify_removed : operation:Removed_operations.t -> t -> t

val simplified_let :
  bindings_to_place:Simplified_named.binding_to_place list ->
  removed_operations:Removed_operations.t ->
  lifted_constants_from_defining_expr:Lifted_constant_state.t ->
  at_unit_toplevel:bool ->
  closure_info:Closure_info.t ->
  rewrite_id:Named_rewrite_id.t ->
  t ->
  t

val simplified_let_cont : simplified_let_cont_handlers -> t -> t

val simplified_non_ocaml_function_call :
  Apply.t ->
  use_id:Apply_cont_rewrite_id.t option ->
  exn_cont_use_id:Apply_cont_rewrite_id.t ->
  t

val simplified_function_call_where_callee's_type_unavailable :
  Apply.t ->
  use_id:Apply_cont_rewrite_id.t option ->
  exn_cont_use_id:Apply_cont_rewrite_id.t ->
  t

val simplified_non_inlined_direct_full_application :
  Apply.t ->
  use_id:Apply_cont_rewrite_id.t option ->
  exn_cont_use_id:Apply_cont_rewrite_id.t ->
  result_arity:[`Unarized] Flambda_arity.t ->
  coming_from_indirect:bool ->
  callee's_code_metadata:Code_metadata.t ->
  t

val simplified_apply_cont :
  Apply_cont.t -> args:Simple.t list -> rewrite_id:Apply_cont_rewrite_id.t -> t

val simplified_switch :
  arms:
    (Apply_cont_expr.t
    * Apply_cont_rewrite_id.t
    * [`Unarized] Flambda_arity.t
    * Typing_env.t)
    Target_ocaml_int.Map.t ->
  condition_dbg:Debuginfo.t ->
  scrutinee:Simple.t ->
  scrutinee_ty:flambda_type ->
  shareable_constants:Symbol.t Static_const.Map.t ->
  typing_env_before_switch:Typing_env.t ->
  cse_before_switch:Common_subexpression_elimination.t ->
  t

val simplified_invalid : Flambda.Invalid.t -> t
