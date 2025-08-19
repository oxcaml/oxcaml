(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

open! Jsoo_imports.Import

(** Translation environment for Flambda to Js_of_ocaml IR translation. *)
type t

(* CR selee: probably we will end up needing to store more info *)

(** Create a new environment.

    [return_continuation] and [exn_continuation] refer to the top-level
    return/exception continuations, and does not change once the environment
    is created.
*)
val create :
  module_symbol:Symbol.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Continuation.t

(** Enter a function body, with the corresponding return and exception continuations. *)
val enter_function_body :
  t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** Symbol corresponding to the module currently compiling. *)
val module_symbol : t -> Symbol.t

(** Map a Flambda2 continuation to the address of the corresponding block.
    Not to be used for continuations used as exception handlers
    (use [add_exn_handler]). *)
val add_continuation : t -> Continuation.t -> Jsir.Addr.t -> t

type exn_handler =
  { addr : Jsir.Addr.t;
    exn_param : Jsir.Var.t;
    extra_args : Jsir.Var.t list
  }

(** Add continuations used as exception handlers, along with its exception parameter
    and any variables used to pass in [extra_args]. *)
val add_exn_handler :
  t ->
  Continuation.t ->
  addr:Jsir.Addr.t ->
  exn_param:Jsir.Var.t ->
  extra_args:Jsir.Var.t list ->
  t

(** Map a Flambda2 variable to a JSIR variable. *)
val add_var : t -> Variable.t -> Jsir.Var.t -> t

(** Set [var] to be an alias of [alias_of]. Raises if [alias_of] is not found
    in the environment. *)
val add_var_alias_of_var_exn : t -> var:Variable.t -> alias_of:Variable.t -> t

(** Map a Flambda2 symbol to a JSIR variable, and register it to the global symbol
    table. *)
val add_symbol :
  t -> res:To_jsir_result.t -> Symbol.t -> Jsir.Var.t -> t * To_jsir_result.t

(** Symbols added through this function must be registered to the global symbol table
    after the definitions for them are added to the result, using [register_symbol_exn].
    Otherwise, they will not be available to other compilation units. *)
val add_symbol_without_registering : t -> Symbol.t -> Jsir.Var.t -> t

(** Register the given symbol to the global symbol table. Raises if the symbol is not in
    the environment.

    Note that calling this function is usually a mistake, as most functions that
    add symbols will automatically call this function. However, it is necessary for
    [add_symbol_without_registering]. *)
val register_symbol_exn :
  t -> res:To_jsir_result.t -> Symbol.t -> To_jsir_result.t

(** Set [var]/[symbol] to be an alias of [alias_of]. Raises if [alias_of] is from the current
    compilation unit and is not found in the environment. *)
val add_var_alias_of_symbol_exn :
  t ->
  res:To_jsir_result.t ->
  var:Variable.t ->
  alias_of:Symbol.t ->
  t * To_jsir_result.t

val add_symbol_alias_of_var_exn :
  t ->
  res:To_jsir_result.t ->
  symbol:Symbol.t ->
  alias_of:Variable.t ->
  t * To_jsir_result.t

type code_id =
  { addr : Jsir.Addr.t;
    params : Jsir.Var.t list;
    closure : Jsir.Var.t
  }

(** Map a Flambda2 code ID to the address of the corresponding JSIR block, its parameters,
    and the JSIR varible corresponding to its closure. *)
val add_code_id :
  t ->
  Code_id.t ->
  addr:Jsir.Addr.t ->
  params:Jsir.Var.t list ->
  closure:Jsir.Var.t ->
  t

(** Map a Flambda2 function slot to the corresponding JSIR closure variable. *)
val add_function_slot : t -> Function_slot.t -> Jsir.Var.t -> t

(** Map a Flambda2 value slot to the corresponding JSIR closure variable. *)
val add_value_slot : t -> Value_slot.t -> Jsir.Var.t -> t

(** Return the block address for the given continuation. Raises if given an
    unbound continuation. *)
val get_continuation_exn : t -> Continuation.t -> Jsir.Addr.t

(** Return the block address and parameters for exception-handling continuations.
    Raises if given an unbound exception handler. *)
val get_exn_handler_exn : t -> Continuation.t -> exn_handler

(** Return the JSIR variable for the given Flambda variable. Raises if given
    an unbound variable. *)
val get_var_exn : t -> Variable.t -> Jsir.Var.t

(** Return the JSIR variable for the given Flambda symbol.

    If the symbol is from the current compilation unit, we look it up in the environment;
    otherwise, we fetch from the global symbol table. *)
val get_symbol :
  t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  (Jsir.Var.t * To_jsir_result.t) option

val get_symbol_exn :
  t -> res:To_jsir_result.t -> Symbol.t -> Jsir.Var.t * To_jsir_result.t

(** Return the block address, parameter variables and closure variable corresponding to
    the given [Code_id.t]. *)
val get_code_id_exn : t -> Code_id.t -> code_id

(** Return the variable corresponding to a function slot. *)
val get_function_slot : t -> Function_slot.t -> Jsir.Var.t option

val get_function_slot_exn : t -> Function_slot.t -> Jsir.Var.t

(** Return the variable corresponding to a value slot. *)
val get_value_slot : t -> Value_slot.t -> Jsir.Var.t option

val get_value_slot_exn : t -> Value_slot.t -> Jsir.Var.t

(** These functions first check whether the given item exists in the environment.
    If it exists, the environment is unchanged. Otherwise, we create a fresh variable,
    and add the mapping to the environment. *)

val add_function_slot_if_not_found : t -> Function_slot.t -> t

val add_value_slot_if_not_found : t -> Value_slot.t -> t

(** Keep track of the [my_closure] of the current code block being translated, and
    maintain a mapping to its JSIR equivalent. *)
val set_my_closure : t -> Variable.t -> Jsir.Var.t -> t

val is_my_closure : t -> Variable.t -> bool
