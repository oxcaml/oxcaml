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

module List = struct
  include List
  include Misc.Stdlib.List
end

module C = struct
  include Cmm_helpers
  include Cmm_builtins
  include To_cmm_shared
end

(* See [Extcall_abi] for details on how unboxed products of an external call are
   passed. *)

module Cmm_arg = struct
  type t = Cmm.expression * Cmm.exttype

  let dummy ~dbg (reg_class : Abi.Reg_class.t) : t =
    match reg_class with
    | Int_regs -> Cconst_int (0, dbg), XInt64
    | Float_regs -> Cconst_float (0., dbg), XFloat

  let padding ~dbg : t = Cconst_int (0, dbg), XInt64
end

module Pack = struct
  (* The bit pattern of a component, as an untagged integer in the low [8 *
     size] bits of a word (the bits above being unspecified). *)
  let component_bits ~dbg (component : Abi.Component.t) cmm : Cmm.expression =
    match K.With_subkind.kind component.kind with
    | Naked_number
        ( Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
        | Naked_nativeint | Naked_mask ) ->
      cmm
    | Naked_number Naked_float -> C.float_as_int64 ~dbg cmm
    | Naked_number Naked_float32 ->
      Cop (Creinterpret_cast Int32_of_float32, [cmm], dbg)
    | Value ->
      (* Values are word-sized, hence never share a unit, and are passed
         directly. *)
      Misc.fatal_error "To_cmm_extcall.Pack.component_bits: value component"
    | Naked_number (Naked_vec128 | Naked_vec256 | Naked_vec512)
    | Region | Rec_info ->
      Misc.fatal_errorf
        "To_cmm_extcall.Pack.component_bits: component of kind %a"
        K.With_subkind.print component.kind

  (* Pair the components of a unit with their expressions, one each. *)
  let with_exprs (components : Abi.Component.t list) exprs =
    if List.compare_lengths components exprs <> 0
    then
      Misc.fatal_errorf
        "To_cmm_extcall.Pack.with_exprs: %d components but %d expressions"
        (List.length components) (List.length exprs);
    List.combine components exprs

  (* Pack components sharing an 8-byte unit into one word, as if that part of
     the C struct had been loaded into an integer register. *)
  let int_bits ~dbg
      (components_with_args :
        (Abi.Component.t * Cmm.expression) Nonempty_list.t) =
    let bits_of ((component : Abi.Component.t), arg) =
      let bits = component_bits ~dbg component arg in
      let bits =
        (* No mask is needed when the component reaches the top of the unit: the
           left shift below (or the width of the unit itself) discards the
           unspecified bits above the component. *)
        if component.offset + component.size >= 8
        then bits
        else C.zero_extend ~bits:(8 * component.size) ~dbg bits
      in
      if component.offset = 0
      then bits
      else C.lsl_int bits (Cconst_int (8 * component.offset, dbg)) dbg
    in
    let (first :: rest) = components_with_args in
    List.fold_left
      (fun acc component_with_arg ->
        C.or_int acc (bits_of component_with_arg) dbg)
      (bits_of first) rest

  (* A unit on the stack is passed as its single component if it has one, and
     otherwise as the integer holding its bytes (see [Abi.Unit.exttype]). *)
  let unit ~dbg (unit : Abi.Unit.t) exprs : Cmm_arg.t =
    match Abi.Unit.single_component unit, with_exprs unit.components exprs with
    | Some _, [(_, expr)] -> expr, Abi.Unit.exttype unit
    | None, [] -> Cmm_arg.padding ~dbg
    | None, first :: rest ->
      int_bits ~dbg (first :: rest), Abi.Unit.exttype unit
    | Some _, ([] | _ :: _ :: _) ->
      Misc.fatal_error "To_cmm_extcall.Pack.unit: single component mismatch"

  (* A register unit is passed as the register's contents (see
     [Abi.Reg_unit.exttype]). *)
  let reg_unit ~dbg (reg_unit : Abi.Reg_unit.t) exprs : Cmm_arg.t =
    let components_with_exprs =
      with_exprs (Nonempty_list.to_list reg_unit.components) exprs
    in
    let expr : Cmm.expression =
      match reg_unit.reg_class, components_with_exprs with
      | Float_regs, [(_, expr)] -> expr
      | Float_regs, first :: rest ->
        C.int64_as_float ~dbg (int_bits ~dbg (first :: rest))
      | Int_regs, [((component : Abi.Component.t), expr)]
        when component.offset = 0 -> (
        match component.reg_class with
        | Int_regs -> expr
        | Float_regs -> component_bits ~dbg component expr)
      | Int_regs, first :: rest -> int_bits ~dbg (first :: rest)
      | (Int_regs | Float_regs), [] ->
        Misc.fatal_error "To_cmm_extcall.Pack.reg_unit: no components"
    in
    expr, Abi.Reg_unit.exttype reg_unit

  (* The Cmm arguments of one C argument, one per slot, consuming its component
     expressions in order. *)
  let arg ~dbg (arg : Abi.Arg.t) component_exprs : Cmm_arg.t list =
    let each ~num_components ~pack parts =
      let leftover_exprs, cmm_args =
        List.fold_left_map
          (fun exprs part ->
            let part_exprs, exprs = List.split_at (num_components part) exprs in
            exprs, pack part part_exprs)
          component_exprs parts
      in
      if not (List.is_empty leftover_exprs)
      then
        Misc.fatal_error
          "To_cmm_extcall.Pack.arg: component expressions left over";
      cmm_args
    in
    match arg, component_exprs with
    | Void, [] -> []
    | Scalar { component = _; exttype }, [expr] -> [expr, exttype]
    | Reg_units reg_units, _ ->
      each ~num_components:Abi.Reg_unit.num_components ~pack:(reg_unit ~dbg)
        reg_units
    | Units units, _ ->
      each ~num_components:Abi.Unit.num_components ~pack:(unit ~dbg) units
    | Void, _ :: _ | Scalar _, ([] | _ :: _ :: _) ->
      Misc.fatal_errorf
        "To_cmm_extcall.Pack.arg: %d component expressions for a scalar or \
         void argument"
        (List.length component_exprs)
end

(* Extracting the components of a return value from its register units. *)
module Unpack = struct
  (* Returned small integer values need to be sign-extended because it's not
     clear whether C code that returns a small integer returns one that is sign
     extended or not. *)
  let sign_extend_small_int_result ~builtin_sign_extends kind ~dbg cmm =
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

  (* The machtype in which the value of a unit of a return value is received. *)
  let reg_unit_machtype ({ reg_class; components } : Abi.Reg_unit.t) =
    match reg_class, components with
    | Float_regs, [component] -> C.machtype_of_kind component.kind
    | Float_regs, _ :: _ :: _ -> Cmm.typ_float
    | Int_regs, [component] -> (
      match component.reg_class with
      | Int_regs -> C.machtype_of_kind component.kind (* val / int / mask *)
      | Float_regs ->
        Cmm.typ_int (* float bits in an integer register (arm64) *))
    | Int_regs, _ :: _ :: _ -> Cmm.typ_int

  (* Extract one component of a return value from the value of its unit.
     Components that arrive in a register of the correct class without any
     packing get the usual sign extension of small integer results. *)
  let component ~dbg ~builtin_sign_extends (t : Abi.Reg_unit.t) unit_value
      (component : Abi.Component.t) : Cmm.expression =
    let shifted value =
      if component.offset = 0
      then value
      else C.asr_int value (Cconst_int (8 * component.offset, dbg)) dbg
    in
    let single = match t.components with [_] -> true | _ :: _ :: _ -> false in
    match t.reg_class, component.reg_class with
    | Float_regs, Float_regs ->
      if single
      then unit_value
      else if component.size <> 4
      then
        (* Only float32s share an SSE eightbyte. *)
        Misc.fatal_errorf
          "To_cmm_extcall.Unpack.component: component of size %d shares a \
           floating-point unit"
          component.size
      else
        Cop
          ( Creinterpret_cast Float32_of_int32,
            [shifted (C.float_as_int64 ~dbg unit_value)],
            dbg )
    | Int_regs, Float_regs -> (
      match component.size with
      | 8 -> C.int64_as_float ~dbg unit_value
      | 4 -> Cop (Creinterpret_cast Float32_of_int32, [shifted unit_value], dbg)
      | _ ->
        Misc.fatal_errorf
          "To_cmm_extcall.Unpack.component: floating-point component of size \
           %d in an integer unit"
          component.size)
    | Int_regs, Int_regs ->
      if single && component.offset = 0
      then
        sign_extend_small_int_result ~builtin_sign_extends component.kind ~dbg
          unit_value
      else if component.size >= 8
      then shifted unit_value
      else C.sign_extend ~bits:(8 * component.size) ~dbg (shifted unit_value)
    | Float_regs, Int_regs ->
      Misc.fatal_errorf
        "To_cmm_extcall.Unpack.component: integer component of kind %a in a \
         floating-point unit"
        K.With_subkind.print component.kind
end

(* Translation of the arguments of an external call. *)
module Args = struct
  (* Returns the Cmm arguments and the [exttype]s describing what is actually
     passed. [args] is the flat list of unarized Cmm arguments. *)
  let translate apply ~dbg args =
    let plan = Abi.Args_plan.create apply in
    let component_exprs_per_arg =
      Flambda_arity.group_by_parameter (Apply.args_arity apply) args
    in
    let cmm_args_per_arg =
      Array.of_list
        (List.map2
           (fun arg component_exprs ->
             Array.of_list (Pack.arg ~dbg arg component_exprs))
           (Array.to_list plan.args) component_exprs_per_arg)
    in
    List.map
      (fun (slot : Abi.Slot.t) ->
        match slot with
        | Arg { arg; slot } -> cmm_args_per_arg.(arg).(slot)
        | Dummy reg_class -> Cmm_arg.dummy ~dbg reg_class
        | Padding -> Cmm_arg.padding ~dbg)
      plan.slots
    |> List.split
end

(* Translation of the return value of an external call. *)
module Return = struct
  let machtype (t : Abi.Return.t) ~return_ty =
    match t with
    | No_value | Scalar _ -> C.Extended_machtype.to_machtype return_ty
    | Reg_units reg_units ->
      Array.concat (List.map Unpack.reg_unit_machtype reg_units)

  (* Rebuild the unarized return values from the received register units. *)
  let unpack_reg_units ~dbg ~builtin_sign_extends reg_units return_values =
    let unit_tys =
      Array.of_list (List.map Unpack.reg_unit_machtype reg_units)
    in
    let unit_value =
      match reg_units with
      | [_] ->
        (* A single unit is not wrapped in a [Ctuple]. *)
        fun _index -> return_values
      | [] | _ :: _ :: _ ->
        fun index ->
          C.tuple_field return_values ~component_tys:unit_tys index dbg
    in
    List.mapi
      (fun index (reg_unit : Abi.Reg_unit.t) ->
        List.map
          (fun component ->
            Unpack.component ~dbg ~builtin_sign_extends reg_unit
              (unit_value index) component)
          (Nonempty_list.to_list reg_unit.components))
      reg_units
    |> List.concat |> C.make_tuple

  let unpack ~dbg ~builtin_sign_extends (t : Abi.Return.t) return_values =
    match t with
    | No_value -> return_values
    | Scalar kind ->
      Unpack.sign_extend_small_int_result ~builtin_sign_extends kind ~dbg
        return_values
    | Reg_units reg_units ->
      unpack_reg_units ~dbg ~builtin_sign_extends reg_units return_values
end

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
  let callee = callee_symbol_name res apply callee_simple in
  let returns = Apply.returns apply in
  let return = Abi.Return.create apply return_arity in
  let args, ty_args = Args.translate apply ~dbg args in
  let effects = To_cmm_effects.transl_c_call_effects effects in
  let coeffects = To_cmm_effects.transl_c_call_coeffects coeffects in
  let { extcall; builtin_sign_extends } : Cmm_builtins.t =
    C.extcall ~dbg ~alloc:needs_caml_c_call ~is_c_builtin ~effects ~coeffects
      ~returns ~ty_args callee
      (Return.machtype return ~return_ty)
      args
  in
  let extcall_ident = Ident.create_local "extcall" in
  let extcall_var = Backend_var.With_provenance.create extcall_ident in
  let body =
    Return.unpack ~dbg ~builtin_sign_extends return (Cvar extcall_ident)
  in
  C.letin extcall_var ~defining_expr:extcall ~body, free_vars, env, res, Ece.all
