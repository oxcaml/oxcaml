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

type continuation =
  { addr : Jsir.Addr.t;
    arity : int
  }

type exn_handler =
  { addr : Jsir.Addr.t;
    exn_param : Jsir.Var.t;
    extra_args : Jsir.Var.t list
  }

type code_id =
  { addr : Jsir.Addr.t;
    params : Jsir.Var.t list;
    closure : Jsir.Var.t
  }

type t =
  { module_symbol : Symbol.t;
    return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    continuations : continuation Continuation.Map.t;
    exn_handlers : exn_handler Continuation.Map.t;
    vars : Jsir.Var.t Variable.Map.t;
    symbols : Jsir.Var.t Symbol.Map.t;
    code_ids : code_id Code_id.Map.t;
    function_slots : Jsir.Var.t Function_slot.Map.t;
    value_slots : Jsir.Var.t Value_slot.Map.t;
    traps : Continuation.t list;
    my_closure : Variable.t option
  }

let create ~module_symbol ~return_continuation ~exn_continuation =
  { module_symbol;
    return_continuation;
    exn_continuation;
    continuations = Continuation.Map.empty;
    exn_handlers = Continuation.Map.empty;
    vars = Variable.Map.empty;
    symbols = Symbol.Map.empty;
    code_ids = Code_id.Map.empty;
    function_slots = Function_slot.Map.empty;
    value_slots = Value_slot.Map.empty;
    traps = [];
    my_closure = None
  }

let module_symbol t = t.module_symbol

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let enter_function_body t ~return_continuation ~exn_continuation =
  { t with return_continuation; exn_continuation }

let add_continuation t cont addr ~arity =
  { t with
    continuations = Continuation.Map.add cont { addr; arity } t.continuations
  }

let add_exn_handler t cont ~addr ~exn_param ~extra_args =
  { t with
    exn_handlers =
      Continuation.Map.add cont { addr; exn_param; extra_args } t.exn_handlers
  }

let add_var t fvar jvar = { t with vars = Variable.Map.add fvar jvar t.vars }

let symbol_to_native_strings symbol =
  ( Symbol.compilation_unit symbol
    |> Compilation_unit.name |> Compilation_unit.Name.to_string
    |> Jsir.Native_string.of_string,
    Symbol.linkage_name_as_string symbol |> Jsir.Native_string.of_string )

let symbol_is_for_compilation_unit symbol =
  let compilation_unit = Symbol.compilation_unit symbol in
  Symbol.equal (Symbol.for_compilation_unit compilation_unit) symbol

let register_symbol' ~res symbol var =
  match symbol_is_for_compilation_unit symbol with
  | true ->
    (* We don't need to add this symbol to our symbol table, because we will
       instead fetch it from jsoo's global data table (see
       [get_symbol_for_global_data]) *)
    res
  | false ->
    let compilation_unit_name, symbol_name = symbol_to_native_strings symbol in
    To_jsir_result.add_instr_exn res
      (Let
         ( Jsir.Var.fresh (),
           Prim
             ( Extern "caml_register_symbol",
               [ Pc (NativeString compilation_unit_name);
                 Pc (NativeString symbol_name);
                 Pv var ] ) ))

let add_symbol_without_registering t symbol jvar =
  { t with symbols = Symbol.Map.add symbol jvar t.symbols }

let add_symbol t ~res symbol jvar =
  let t = add_symbol_without_registering t symbol jvar in
  t, register_symbol' ~res symbol jvar

let add_code_id t code_id ~addr ~params ~closure =
  let code_ids = Code_id.Map.add code_id { addr; params; closure } t.code_ids in
  { t with code_ids }

let add_function_slot t fslot jvar =
  { t with function_slots = Function_slot.Map.add fslot jvar t.function_slots }

let add_value_slot t vslot jvar =
  { t with value_slots = Value_slot.Map.add vslot jvar t.value_slots }

let get_continuation_exn t cont = Continuation.Map.find cont t.continuations

let get_exn_handler_exn t cont = Continuation.Map.find cont t.exn_handlers

let get_var_exn t fvar = Variable.Map.find fvar t.vars

let get_symbol_from_global_data ~compilation_unit ~symbol_name ~res =
  let res = To_jsir_result.import_compilation_unit res compilation_unit in
  let res, global_data = To_jsir_result.global_data_var res in
  let symbol_name = Jsir.Native_string.of_string symbol_name in
  let var = Jsir.Var.fresh () in
  let expr : Jsir.expr =
    Prim (Extern "caml_js_get", [Pv global_data; Pc (NativeString symbol_name)])
  in
  var, To_jsir_result.add_instr_exn res (Let (var, expr))

let get_predef_exception ~res symbol =
  let symbol_name = Symbol.linkage_name_as_string symbol in
  (* Chop off caml_exn_ *)
  let caml_exn_ = "caml_exn_" in
  if not (String.starts_with ~prefix:caml_exn_ symbol_name)
  then
    Misc.fatal_errorf
      "Predefined exception symbol %a doesn't start with \"caml_exn_\""
      Symbol.print symbol;
  let symbol_name =
    String.sub symbol_name (String.length caml_exn_)
      (String.length symbol_name - String.length caml_exn_)
  in
  (* jsoo already registers these in global data *)
  get_symbol_from_global_data
    ~compilation_unit:(Symbol.compilation_unit symbol)
    ~symbol_name ~res

let get_external_symbol ~res symbol =
  match Symbol.is_predefined_exception symbol with
  | true -> get_predef_exception ~res symbol
  | false -> (
    match symbol_is_for_compilation_unit symbol with
    | true ->
      let compilation_unit = Symbol.compilation_unit symbol in
      let compilation_unit_name =
        Compilation_unit.name_as_string compilation_unit
      in
      get_symbol_from_global_data ~compilation_unit
        ~symbol_name:compilation_unit_name ~res
    | false ->
      let compilation_unit_name, symbol_name =
        symbol_to_native_strings symbol
      in
      let var = Jsir.Var.fresh () in
      let expr : Jsir.expr =
        Prim
          ( Extern "caml_get_symbol",
            [ Pc (NativeString compilation_unit_name);
              Pc (NativeString symbol_name) ] )
      in
      var, To_jsir_result.add_instr_exn res (Let (var, expr)))

let get_symbol t ~res symbol =
  match Symbol.compilation_unit symbol |> Compilation_unit.is_current with
  | true -> Option.map (fun v -> v, res) (Symbol.Map.find_opt symbol t.symbols)
  | false -> Some (get_external_symbol ~res symbol)

let get_symbol_exn t ~res symbol =
  match get_symbol t ~res symbol with
  | Some (v, res) -> v, res
  | None -> raise Not_found

let register_symbol_exn t ~res symbol =
  (* Using the map directly rather than using [get_symbol], because we don't
     want to load external symbols *)
  register_symbol' ~res symbol (Symbol.Map.find symbol t.symbols)

let get_code_id_exn t code_id = Code_id.Map.find code_id t.code_ids

let get_function_slot t fslot =
  Function_slot.Map.find_opt fslot t.function_slots

let get_function_slot_exn t fslot =
  Function_slot.Map.find fslot t.function_slots

let get_value_slot t vslot = Value_slot.Map.find_opt vslot t.value_slots

let get_value_slot_exn t vslot = Value_slot.Map.find vslot t.value_slots

let add_var_alias_of_var_exn t ~var ~alias_of =
  let jvar = get_var_exn t alias_of in
  { t with vars = Variable.Map.add var jvar t.vars }

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
  { t with vars = Variable.Map.add var jvar t.vars }, res

let add_if_not_found map item ~mem ~add =
  if mem item map
  then map
  else
    let var = Jsir.Var.fresh () in
    add item var map

let add_function_slot_if_not_found t slot =
  { t with
    function_slots =
      add_if_not_found t.function_slots slot ~mem:Function_slot.Map.mem
        ~add:Function_slot.Map.add
  }

let add_value_slot_if_not_found t slot =
  { t with
    value_slots =
      add_if_not_found t.value_slots slot ~mem:Value_slot.Map.mem
        ~add:Value_slot.Map.add
  }

let set_my_closure t my_closure var =
  let t = { t with my_closure = Some my_closure } in
  add_var t my_closure var

let is_my_closure t var =
  match t.my_closure with
  | None -> false
  | Some my_closure -> Variable.equal var my_closure
