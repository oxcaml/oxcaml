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

type exn_handler =
  { addr : Jsir.Addr.t;
    exn_param : Jsir.Var.t;
    extra_args : Jsir.Var.t list
  }

type closure =
  { addr : Jsir.Addr.t;
    params : Jsir.Var.t list;
    var : Jsir.Var.t;
    hint : Optimization_hint.closure_hint option;
    loc : Parse_info.t option
  }

type code =
  | Shared of closure
  | Instantiated of Code.t

type slots =
  { function_slots : Jsir.Var.t Function_slot.Map.t;
    value_slots : Jsir.Var.t Value_slot.Map.t
  }

type t =
  { module_symbol : Symbol.t;
    return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    continuations : Jsir.Addr.t Continuation.Map.t;
    exn_handlers : exn_handler Continuation.Map.t;
    vars : Jsir.Var.t Variable.Map.t;
    symbols : Jsir.Var.t Symbol.Map.t;
    code : code Code_id.Map.t;
    closure_slots : slots Jsir.Var.Map.t
  }

let create ~module_symbol ~return_continuation ~exn_continuation =
  { module_symbol;
    return_continuation;
    exn_continuation;
    continuations = Continuation.Map.empty;
    exn_handlers = Continuation.Map.empty;
    vars = Variable.Map.empty;
    symbols = Symbol.Map.empty;
    code = Code_id.Map.empty;
    closure_slots = Jsir.Var.Map.empty
  }

let module_symbol t = t.module_symbol

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let enter_function_body t ~return_continuation ~exn_continuation =
  { t with return_continuation; exn_continuation }

let add_continuation t cont addr =
  { t with continuations = Continuation.Map.add cont addr t.continuations }

let add_exn_handler t cont ~addr ~exn_param ~extra_args =
  { t with
    exn_handlers =
      Continuation.Map.add cont { addr; exn_param; extra_args } t.exn_handlers
  }

(* With [-g], name JSIR variables after the user-visible Flambda variables they
   stand for. When aliases share a JSIR variable, the first non-generated name
   wins. *)
let name_var fvar jvar =
  if Flambda_features.debug () && Variable.user_visible fvar
  then
    let name = Variable.name fvar in
    let generated =
      (String.length name > 1 && Char.equal name.[0] '*')
      || match name with "param" | "switcher" | "tag" -> true | _ -> false
    in
    match Jsir.Var.get_name jvar with
    | Some _ when generated || not (Jsir.Var.generated_name jvar) -> ()
    | Some _ | None -> Jsir.Var.set_name jvar ~generated name

let add_var t fvar jvar =
  name_var fvar jvar;
  { t with vars = Variable.Map.add fvar jvar t.vars }

let add_symbol_without_registering t symbol jvar =
  { t with symbols = Symbol.Map.add symbol jvar t.symbols }

let add_symbol t ~res symbol jvar =
  let t = add_symbol_without_registering t symbol jvar in
  t, To_jsir_result.register_symbol res symbol jvar

let add_code t code_id code =
  { t with code = Code_id.Map.add code_id code t.code }

let add_closure_slots t ~closure slots =
  { t with closure_slots = Jsir.Var.Map.add closure slots t.closure_slots }

let get_continuation_exn t cont = Continuation.Map.find cont t.continuations

let get_exn_handler_exn t cont = Continuation.Map.find cont t.exn_handlers

let get_var_exn t fvar = Variable.Map.find fvar t.vars

let get_symbol t ~res symbol =
  match Symbol.compilation_unit symbol |> Current_unit.is_current with
  | true -> Option.map (fun v -> v, res) (Symbol.Map.find_opt symbol t.symbols)
  | false ->
    let res, var = To_jsir_result.external_symbol_var res symbol in
    Some (var, res)

let get_symbol_exn t ~res symbol =
  match get_symbol t ~res symbol with
  | Some (v, res) -> v, res
  | None -> raise Not_found

let register_symbol_exn t ~res ?loc symbol =
  (* Using the map directly rather than using [get_symbol], because we don't
     want to load external symbols *)
  To_jsir_result.register_symbol res ?loc symbol
    (Symbol.Map.find symbol t.symbols)

let get_code_exn t code_id =
  match Code_id.Map.find_opt code_id t.code with
  | Some code -> code
  | None ->
    Misc.fatal_errorf "Code ID %a not found in the environment" Code_id.print
      code_id

let get_closure_slots_exn t ~closure =
  match Jsir.Var.Map.find_opt closure t.closure_slots with
  | Some slots -> slots
  | None ->
    Misc.fatal_errorf
      "Cannot project from the closure %a: the set of closures that it belongs \
       to is not known"
      Jsir.Var.print closure

let get_function_slot_exn t ~closure slot =
  let slots = get_closure_slots_exn t ~closure in
  match Function_slot.Map.find_opt slot slots.function_slots with
  | Some var -> var
  | None ->
    Misc.fatal_errorf "Function slot %a not found in the set of closures of %a"
      Function_slot.print slot Jsir.Var.print closure

let get_value_slot_exn t ~closure slot =
  let slots = get_closure_slots_exn t ~closure in
  match Value_slot.Map.find_opt slot slots.value_slots with
  | Some var -> var
  | None ->
    Misc.fatal_errorf "Value slot %a not found in the set of closures of %a"
      Value_slot.print slot Jsir.Var.print closure

let add_var_alias_of_var_exn t ~var ~alias_of =
  let jvar = get_var_exn t alias_of in
  add_var t var jvar

let add_symbol_alias_of_var_exn t ~res ~symbol ~alias_of =
  let jvar = get_var_exn t alias_of in
  add_symbol t ~res symbol jvar

let add_var_alias_of_symbol_exn t ~res ~var ~alias_of =
  let jvar, res =
    match get_symbol t ~res alias_of with
    | None ->
      Misc.fatal_errorf "Symbol %a not found in the environment" Symbol.print
        alias_of
    | Some (v, res) -> v, res
  in
  add_var t var jvar, res
