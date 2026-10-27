(******************************************************************************
 *                                  OxCaml                                    *
 *                                Jane Street                                 *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

[@@@ocaml.warning "+a-30-40-41-42"]

module Apply = Flambda.Apply
module Ece = Effects_and_coeffects
module K = Flambda_kind
module Nonempty_list = Misc.Nonempty_list
module Abi = Extcall_abi

module C = struct
  include Cmm_helpers
  include Cmm_builtins
  include To_cmm_shared
end

(* See [Extcall_abi] for details about the ABI and how unboxed products are
   handled. *)

module Pack = struct
  (* In the low [8 * size] bits of a word, the rest unspecified. *)
  let component_bits ~dbg component cmm : Cmm.expression =
    let kind = Abi.Component.kind component in
    match K.With_subkind.kind kind with
    | Naked_number
        ( Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
        | Naked_nativeint | Naked_mask ) ->
      cmm
    | Naked_number Naked_float -> C.float_as_int64 ~dbg cmm
    | Naked_number Naked_float32 ->
      Cop (Creinterpret_cast Int32_of_float32, [cmm], dbg)
    | Value ->
      (* Values are word-sized, so never share a unit. *)
      Misc.fatal_error "To_cmm_extcall.Pack.component_bits: value component"
    | Naked_number (Naked_vec128 | Naked_vec256 | Naked_vec512)
    | Region | Rec_info ->
      Misc.fatal_errorf
        "To_cmm_extcall.Pack.component_bits: component of kind %a"
        K.With_subkind.print kind

  let with_exprs (components : Abi.Component.t list) exprs =
    if List.compare_lengths components exprs <> 0
    then
      Misc.fatal_errorf
        "To_cmm_extcall.Pack.with_exprs: %d components but %d expressions"
        (List.length components) (List.length exprs);
    List.combine components exprs

  (* Components sharing a unit, packed into one word as C would load them. *)
  let int_bits ~dbg
      (components_with_args :
        (Abi.Component.t * Cmm.expression) Nonempty_list.t) =
    let bits_of (component, arg) =
      let offset = Abi.Component.offset component in
      let size = Abi.Component.size component in
      let bits = component_bits ~dbg component arg in
      let bits =
        (* No mask needed: the shift (or the unit's width) drops the bits
           above. *)
        if offset + size >= 8
        then bits
        else C.zero_extend ~bits:(8 * size) ~dbg bits
      in
      if offset = 0
      then bits
      else C.lsl_int bits (Cconst_int (8 * offset, dbg)) dbg
    in
    let (first :: rest) = components_with_args in
    List.fold_left
      (fun acc component_with_arg ->
        C.or_int acc (bits_of component_with_arg) dbg)
      (bits_of first) rest

  let stack_unit ~dbg unit exprs : Cmm.expression =
    match
      ( Abi.Unit.single_component unit,
        with_exprs (Abi.Unit.components unit) exprs )
    with
    | Some _, [(_, expr)] -> expr
    | None, [] ->
      (* Padding, whose contents are unspecified *)
      Cconst_int (0, dbg)
    | None, first :: rest -> int_bits ~dbg (first :: rest)
    | Some _, ([] | _ :: _ :: _) ->
      Misc.fatal_error "To_cmm_extcall.Pack.unit: single component mismatch"

  let reg_unit ~dbg reg_unit exprs : Cmm.expression =
    let components_with_exprs =
      with_exprs
        (Nonempty_list.to_list (Abi.Reg_unit.components reg_unit))
        exprs
    in
    match Abi.Reg_unit.contents reg_unit, components_with_exprs with
    | Component _, [(_, expr)] -> expr
    | Int_bits, [(component, expr)] when Abi.Component.offset component = 0 ->
      (* The bits of a floating-point component (arm64); no mask needed. *)
      component_bits ~dbg component expr
    | Int_bits, first :: rest -> int_bits ~dbg (first :: rest)
    | Float_bits, first :: rest ->
      C.int64_as_float ~dbg (int_bits ~dbg (first :: rest))
    | Component _, ([] | _ :: _ :: _) | (Int_bits | Float_bits), [] ->
      Misc.fatal_error "To_cmm_extcall.Pack.reg_unit: component mismatch"

  let prepare_args ~dbg args arg_slots =
    let dummy : Cmm.expression = Ctuple [] in
    let padding : Cmm.expression = Cconst_int (0, dbg) in
    Abi.Arg_slots.pack arg_slots ~reg_unit:(reg_unit ~dbg)
      ~stack_unit:(stack_unit ~dbg) ~dummy ~padding args
    |> List.split
end

module Unpack = struct
  (* Returned small integer values need to be sign-extended because it's not
     clear whether C code that returns a small integer returns one that is sign
     extended or not. *)
  let sign_extend_small_int_result ~dbg ~builtin_sign_extends kind cmm =
    if builtin_sign_extends
    then cmm
    else
      match K.With_subkind.kind kind with
      | Naked_number Naked_int8 -> C.sign_extend ~bits:8 ~dbg cmm
      | Naked_number Naked_int16 -> C.sign_extend ~bits:16 ~dbg cmm
      | Naked_number Naked_int32 -> C.sign_extend ~bits:32 ~dbg cmm
      | Naked_number
          ( Naked_float | Naked_immediate | Naked_int64 | Naked_nativeint
          | Naked_vec128 | Naked_vec256 | Naked_vec512 | Naked_mask
          | Naked_float32 )
      | Value | Rec_info | Region ->
        cmm

  let component ~dbg ~builtin_sign_extends reg_unit component unit_value :
      Cmm.expression =
    let kind = Abi.Component.kind component in
    let offset = Abi.Component.offset component in
    let size = Abi.Component.size component in
    let shifted value =
      if offset = 0
      then value
      else C.asr_int value (Cconst_int (8 * offset, dbg)) dbg
    in
    match Abi.Reg_unit.contents reg_unit with
    | Component _ ->
      sign_extend_small_int_result ~dbg ~builtin_sign_extends kind unit_value
    | Float_bits ->
      if size <> 4
      then
        (* Only float32s share an SSE eightbyte. *)
        Misc.fatal_errorf
          "To_cmm_extcall.Unpack.component: component of size %d shares a \
           floating-point unit"
          size;
      Cop
        ( Creinterpret_cast Float32_of_int32,
          [shifted (C.float_as_int64 ~dbg unit_value)],
          dbg )
    | Int_bits -> (
      match Abi.Component.reg_class component with
      | Int_regs ->
        if size >= 8
        then shifted unit_value
        else C.sign_extend ~bits:(8 * size) ~dbg (shifted unit_value)
      | Float_regs -> (
        match size with
        | 8 -> C.int64_as_float ~dbg unit_value
        | 4 ->
          Cop (Creinterpret_cast Float32_of_int32, [shifted unit_value], dbg)
        | _ ->
          Misc.fatal_errorf
            "To_cmm_extcall.Unpack.component: floating-point component of size \
             %d in an integer unit"
            size))

  let prepare_return ~dbg ~builtin_sign_extends return_abi extcall_ident =
    Abi.Return.unpack return_abi
      ~scalar:(sign_extend_small_int_result ~dbg ~builtin_sign_extends)
      ~unit_value:(fun result ~component_tys index ->
        C.tuple_field result ~component_tys index dbg)
      ~component:(component ~dbg ~builtin_sign_extends)
      ~tuple:C.make_tuple (Cmm.Cvar extcall_ident)
end

(* Cmm arguments should be evaluated right to left. However, the way we
   implement the ABI means we reorder arguments when we actually generate the
   [Cextcall]. Here, we let-bind [args] in order to fix the evaluation order. *)
let bind_args_in_evaluation_order args =
  let is_trivial (expr : Cmm.expression) =
    match expr with
    | Cvar _ | Cconst_int _ | Cconst_natint _ | Cconst_float32 _
    | Cconst_float _ | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _
    | Cconst_vec512 _ | Cconst_mask _ ->
      true
    | Clet _ | Cphantom_let _ | Cname_for_debugger _ | Ctuple _ | Cop _
    | Csequence _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Cinvalid _
      ->
      false
  in
  List.fold_left
    (fun (bindings, args) arg ->
      if is_trivial arg
      then bindings, arg :: args
      else
        let ident = Ident.create_local "extcall_arg" in
        ( (Backend_var.With_provenance.create ident, arg) :: bindings,
          Cmm.Cvar ident :: args ))
    ([], []) (List.rev args)

let callee_symbol_name res apply callee_simple =
  match callee_simple with
  | None ->
    Misc.fatal_errorf "To_cmm_extcall.callee_symbol_name: no callee:@ %a"
      Apply.print apply
  | Some callee_simple -> (
    match Simple.must_be_symbol callee_simple with
    | Some (sym, _) -> (To_cmm_result.symbol res sym).sym_name
    | None ->
      Misc.fatal_errorf
        "To_cmm_extcall.callee_symbol_name: callee is not a symbol: %a"
        Simple.print callee_simple)

let translate_external_call env res ~free_vars apply ~callee_simple ~args
    ~return_arity ~return_ty dbg ~needs_caml_c_call ~is_c_builtin ~effects
    ~coeffects =
  C.fail_if_apply_with_probe apply;
  (* Prepare arguments according to the ABI *)
  let arg_slots = Abi.Arg_slots.create apply in
  let arg_bindings, args =
    if Abi.Arg_slots.in_argument_order arg_slots
    then [], args
    else bind_args_in_evaluation_order args
  in
  let args, ty_args = Pack.prepare_args ~dbg args arg_slots in
  (* Prepare the return value representation *)
  let return_abi = Abi.Return.create apply return_arity in
  let return_ty =
    Abi.Return.machtype return_abi
      ~return_ty:(C.Extended_machtype.to_machtype return_ty)
  in
  (* Make the actual call *)
  let callee = callee_symbol_name res apply callee_simple in
  let returns = Apply.returns apply in
  let effects = To_cmm_effects.transl_c_call_effects effects in
  let coeffects = To_cmm_effects.transl_c_call_coeffects coeffects in
  let { extcall; builtin_sign_extends } : Cmm_builtins.t =
    C.extcall ~dbg ~alloc:needs_caml_c_call ~is_c_builtin ~effects ~coeffects
      ~returns ~ty_args callee return_ty args
  in
  (* Unpack the returned values *)
  let extcall_ident = Ident.create_local "extcall" in
  let extcall_var = Backend_var.With_provenance.create extcall_ident in
  let body =
    Unpack.prepare_return ~dbg ~builtin_sign_extends return_abi extcall_ident
  in
  let extcall_unpacked = C.letin extcall_var ~defining_expr:extcall ~body in
  (* Add the let bindings for the arguments to the beginning *)
  let cmm =
    List.fold_left
      (fun body (var, defining_expr) -> C.letin var ~defining_expr ~body)
      extcall_unpacked arg_bindings
  in
  cmm, free_vars, env, res, Ece.all
