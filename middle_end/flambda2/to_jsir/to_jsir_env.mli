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

(** Create a new environment.

    [return_continuation] and [exn_continuation] refer to the top-level
    return/exception continuations, and does not change once the environment is
    created. *)
val create :
  module_symbol:Symbol.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  find_imported_code:(Code_id.t -> Code_or_metadata.t option) ->
  t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Continuation.t

(** Enter a function body, with the corresponding return and exception
    continuations. *)
val enter_function_body :
  t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

(** Symbol corresponding to the module currently compiling. *)
val module_symbol : t -> Symbol.t

(** Map a Flambda2 continuation to the address of the corresponding block. Not
    to be used for continuations used as exception handlers (use
    [add_exn_handler]). *)
val add_continuation : t -> Continuation.t -> Jsir.Addr.t -> t

type exn_handler =
  { addr : Jsir.Addr.t;
    exn_param : Jsir.Var.t;
    extra_args : Jsir.Var.t list
  }

(** Add continuations used as exception handlers, along with its exception
    parameter and any variables used to pass in [extra_args]. *)
val add_exn_handler :
  t ->
  Continuation.t ->
  addr:Jsir.Addr.t ->
  exn_param:Jsir.Var.t ->
  extra_args:Jsir.Var.t list ->
  t

(** Map a Flambda2 variable to a JSIR variable. *)
val add_var : t -> Variable.t -> Jsir.Var.t -> t

(** Set [var] to be an alias of [alias_of]. Raises if [alias_of] is not found in
    the environment. *)
val add_var_alias_of_var_exn : t -> var:Variable.t -> alias_of:Variable.t -> t

(** Map a Flambda2 symbol to a JSIR variable, and register it to the global
    symbol table. *)
val add_symbol :
  t -> res:To_jsir_result.t -> Symbol.t -> Jsir.Var.t -> t * To_jsir_result.t

(** Symbols added through this function must be registered to the global symbol
    table after the definitions for them are added to the result, using
    [register_symbol_exn]. Otherwise, they will not be available to other
    compilation units. *)
val add_symbol_without_registering : t -> Symbol.t -> Jsir.Var.t -> t

(** Register the given symbol to the global symbol table, with an [Event] for
    [loc] before the registration. Raises if the symbol is not in the
    environment.

    Note that calling this function is usually a mistake, as most functions that
    add symbols will automatically call this function. However, it is necessary
    for [add_symbol_without_registering]. *)
val register_symbol_exn :
  t -> res:To_jsir_result.t -> ?loc:Parse_info.t -> Symbol.t -> To_jsir_result.t

(** Set [var]/[symbol] to be an alias of [alias_of]. Raises if [alias_of] is
    from the current compilation unit and is not found in the environment. *)
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

type closure =
  { addr : Jsir.Addr.t;
    params : Jsir.Var.t list;
    var : Jsir.Var.t;
    hint : Optimization_hint.closure_hint option;
    loc : Parse_info.t option
  }

type shared =
  | Current_unit of closure
  | Other_unit of Code_metadata.t

type code =
  | Shared of shared
      (** Translated once, by the compilation unit defining the code, with a
          single closure reused across all [Set_of_closures]. *)
  | Instantiated of Code.t
      (** Translated afresh with a new closure for each [Set_of_closures]
          containing it. Code of another compilation unit is translated from its
          definition in the .cmx file of that unit. *)

val code_is_shared : Code.t -> bool

val add_code : t -> Code_id.t -> code -> t

type slots =
  { function_slots : Jsir.Var.t Function_slot.Map.t;
    value_slots : Jsir.Var.t Value_slot.Map.t
  }

(** Associate [closure] with the slots of its set of closures so that slot
    projections from [closure] can be resolved. *)
val add_closure_slots : t -> closure:Jsir.Var.t -> slots -> t

(** Return the block address for the given continuation. Raises if given an
    unbound continuation. *)
val get_continuation_exn : t -> Continuation.t -> Jsir.Addr.t

(** Return the block address and parameters for exception-handling
    continuations. Raises if given an unbound exception handler. *)
val get_exn_handler_exn : t -> Continuation.t -> exn_handler

(** Return the JSIR variable for the given Flambda variable. Raises if given an
    unbound variable. *)
val get_var_exn : t -> Variable.t -> Jsir.Var.t

(** Return the JSIR variable for the given Flambda symbol.

    If the symbol is from the current compilation unit, we look it up in the
    environment; otherwise, we fetch it from the global symbol table (or with
    [caml_get_global] for module blocks) once, at the start of the entry block,
    and reuse that variable for every use. *)
val get_symbol :
  t ->
  res:To_jsir_result.t ->
  Symbol.t ->
  (Jsir.Var.t * To_jsir_result.t) option

val get_symbol_exn :
  t -> res:To_jsir_result.t -> Symbol.t -> Jsir.Var.t * To_jsir_result.t

(** Code of the current compilation unit is looked up in the environment. Code
    of other units is looked up in their .cmx files instead, and is [Shared] or
    [Instantiated] as it is in the unit defining it. *)
val get_code_exn : t -> Code_id.t -> code

(** Return the JSIR variable for the closure of the given [Shared] code. *)
val get_shared_closure :
  res:To_jsir_result.t -> Code_id.t -> shared -> Jsir.Var.t * To_jsir_result.t

(** Look up a slot in the set of closures associated with [closure]. Raises if
    [closure] was not registered via [add_closure_slots] or does not contain the
    slot. *)
val get_function_slot_exn :
  t -> closure:Jsir.Var.t -> Function_slot.t -> Jsir.Var.t

val get_value_slot_exn : t -> closure:Jsir.Var.t -> Value_slot.t -> Jsir.Var.t
