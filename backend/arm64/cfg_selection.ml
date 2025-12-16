(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*   Copyright 2025 Jane Street Group LLC.                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the ARM processor *)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-40-41-42"]

open Arch
module Validated_mem_offset = Arm64_ast.Ast.DSL.Validated_mem_offset

let scale_of_chunk : Cmm.memory_chunk -> int = function
  | Byte_unsigned | Byte_signed -> 1
  | Sixteen_unsigned | Sixteen_signed -> 2
  | Thirtytwo_unsigned | Thirtytwo_signed | Single { reg = Float64 | Float32 }
    ->
    4
  | Word_int | Word_val | Double -> 8
  | Onetwentyeight_aligned | Onetwentyeight_unaligned -> 16
  | Twofiftysix_aligned | Twofiftysix_unaligned | Fivetwelve_aligned
  | Fivetwelve_unaligned ->
    Misc.fatal_error "arm64: got 256/512 bit vector"

let is_offset chunk n =
  Validated_mem_offset.is_valid ~scale:(scale_of_chunk chunk) ~offset:n

let is_logical_immediate_int n =
  Arm64_ast.Logical_immediates.is_logical_immediate (Nativeint.of_int n)

(* Signed immediates are simpler *)

let int_is_immediate n =
  let mn = -n in
  n land 0xFFF = n
  || n land 0xFFF_000 = n
  || mn land 0xFFF = mn
  || mn land 0xFFF_000 = mn

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops = ["sqrt"]

let use_direct_addressing _symb = (not !Clflags.dlcode) && not Arch.macosx

let is_stack_slot rv =
  Reg.(match rv with [| { loc = Stack _; _ } |] -> true | _ -> false)

let select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth = function
  | Sixteen -> Sixteen
  | Thirtytwo -> Thirtytwo
  | Sixtyfour -> Sixtyfour

let specific x : Cfg.basic_or_terminator = Basic (Op (Specific x))

let is_immediate (op : Operation.integer_operation) n :
    Cfg_selectgen_target_intf.is_immediate_result =
  match op with
  | Iadd | Isub -> Is_immediate (n <= 0xFFF_FFF && n >= -0xFFF_FFF)
  | Iand | Ior | Ixor -> Is_immediate (is_logical_immediate_int n)
  | Icomp _ -> Is_immediate (int_is_immediate n)
  | _ -> Use_default

let is_immediate_test _cmp n : Cfg_selectgen_target_intf.is_immediate_result =
  Is_immediate (int_is_immediate n)

let is_simple_expr (expr : Cmm.expression) :
    Cfg_selectgen_target_intf.is_simple_expr_result =
  match expr with
  (* inlined floating-point ops are simple if their arguments are *)
  | Cop (Cextcall { func; _ }, args, _) when List.mem func inline_ops ->
    Simple_if_all_expressions_are args
  | _ -> Use_default

let effects_of (expr : Cmm.expression) :
    Cfg_selectgen_target_intf.effects_of_result =
  match expr with
  | Cop (Cextcall { func; _ }, args, _) when List.mem func inline_ops ->
    Effects_of_all_expressions args
  | _ -> Use_default

let asm_symbol_of_cmm (s : Cmm.symbol) =
  let visibility : Asm_targets.Asm_symbol.visibility =
    match s.sym_global with Cmm.Global -> Global | Cmm.Local -> Local
  in
  Asm_targets.Asm_symbol.create ~visibility s.sym_name

let validated_offset chunk n =
  let scale = scale_of_chunk chunk in
  match Validated_mem_offset.create ~scale ~offset:n with
  | Some v -> Iindexed v
  | None ->
    Misc.fatal_errorf "cfg_selection: offset %d invalid for chunk scale %d" n
      scale

let select_addressing' chunk (expr : Cmm.expression) :
    addressing_mode * Cmm.expression =
  match expr with
  | Cop ((Caddv | Cadda), [Cconst_symbol (s, _); Cconst_int (n, _)], _)
    when use_direct_addressing s ->
    Ibased (asm_symbol_of_cmm s, n), Ctuple []
  | Cop ((Caddv | Cadda), [arg; Cconst_int (n, _)], _) when is_offset chunk n ->
    validated_offset chunk n, arg
  | Cop
      ( ((Caddv | Cadda) as op),
        [arg1; Cop (Caddi, [arg2; Cconst_int (n, _)], _)],
        dbg )
    when is_offset chunk n ->
    validated_offset chunk n, Cop (op, [arg1; arg2], dbg)
  | Cconst_symbol (s, _) when use_direct_addressing s ->
    Ibased (asm_symbol_of_cmm s, 0), Ctuple []
  | arg -> validated_offset chunk 0, arg

let select_addressing chunk exp : addressing_mode * Cmm.expression =
  if !Clflags.llvm_backend (* Llvmize only expects [Iindexed] *)
  then validated_offset chunk 0, exp
  else select_addressing' chunk exp

let select_operation' ~generic_select_condition:_ (op : Cmm.operation)
    (args : Cmm.expression list) dbg ~label_after:_ :
    Cfg_selectgen_target_intf.select_operation_result =
  let[@inline] rewrite_multiply_add_or_sub shift_op mul_op ~arg1 ~args2 dbg :
      Cfg_selectgen_target_intf.select_operation_result =
    Select_operation_then_rewrite
      ( Cmuli,
        args2,
        dbg,
        fun (basic_or_terminator : Cfg.basic_or_terminator) ~args ->
          match basic_or_terminator, args with
          | Basic (Op (Intop_imm (Ilsl, l))), [arg3] ->
            Rewritten (specific (Ishiftarith (shift_op, l)), [arg1; arg3])
          | Basic (Op (Intop Imul)), [arg3; arg4] ->
            Rewritten (specific mul_op, [arg3; arg4; arg1])
          | _ -> Use_default )
  in
  match op with
  (* Integer addition *)
  | Caddi | Caddv | Cadda -> (
    match args with
    (* Shift-add *)
    | [arg1; Cop (Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, n)), [arg1; arg2])
    | [arg1; Cop (Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, -n)), [arg1; arg2])
    | [Cop (Clsl, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, n)), [arg2; arg1])
    | [Cop (Casr, [arg1; Cconst_int (n, _)], _); arg2] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftadd, -n)), [arg2; arg1])
    (* Multiply-add *)
    | [arg1; Cop (Cmuli, args2, dbg)] | [Cop (Cmuli, args2, dbg); arg1] ->
      rewrite_multiply_add_or_sub Ishiftadd Imuladd ~arg1 ~args2 dbg
    | _ -> Use_default)
  (* Integer subtraction *)
  | Csubi -> (
    match args with
    (* Shift-sub *)
    | [arg1; Cop (Clsl, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftsub, n)), [arg1; arg2])
    | [arg1; Cop (Casr, [arg2; Cconst_int (n, _)], _)] when n > 0 && n < 64 ->
      Rewritten (specific (Ishiftarith (Ishiftsub, -n)), [arg1; arg2])
    (* Multiply-sub *)
    | [arg1; Cop (Cmuli, args2, dbg)] ->
      rewrite_multiply_add_or_sub Ishiftsub Imulsub ~arg1 ~args2 dbg
    | _ -> Use_default)
  (* Recognize sign extension *)
  | Casr -> (
    match args with
    | [Cop (Clsl, [k; Cconst_int (n, _)], _); Cconst_int (n', _)]
      when n' = n && 0 < n && n < 64 ->
      Rewritten (specific (Isignext (64 - n)), [k])
    | _ -> Use_default)
  (* Use trivial addressing mode for atomic loads *)
  | Cload { memory_chunk; mutability; is_atomic = true } ->
    Rewritten
      ( Basic
          (Op
             (Load
                { memory_chunk;
                  addressing_mode = validated_offset memory_chunk 0;
                  mutability = Select_utils.select_mutable_flag mutability;
                  is_atomic = true
                })),
        args )
  (* Recognize floating-point negate and multiply *)
  | Cnegf Float64 -> (
    match args with
    | [Cop (Cmulf Float64, args, _)] -> Rewritten (specific Inegmulf, args)
    | _ -> Use_default)
  (* Recognize floating-point multiply and add/sub *)
  | Caddf Float64 -> (
    match args with
    | [arg; Cop (Cmulf Float64, args, _)] | [Cop (Cmulf Float64, args, _); arg]
      ->
      Rewritten (specific Imuladdf, arg :: args)
    | _ -> Use_default)
  | Csubf Float64 -> (
    match args with
    | [arg; Cop (Cmulf Float64, args, _)] ->
      Rewritten (specific Imulsubf, arg :: args)
    | [Cop (Cmulf Float64, args, _); arg] ->
      Rewritten (specific Inegmulsubf, arg :: args)
    | _ -> Use_default)
  | Cpackf32 -> Rewritten (specific (Isimd Zip1_f32), args)
  (* Recognize floating-point square root *)
  | Cextcall { func = "sqrt" | "sqrtf" | "caml_neon_float64_sqrt"; _ } ->
    Rewritten (specific Isqrtf, args)
  | Cextcall { func; builtin = true; _ } -> (
    match Simd_selection.select_operation_cfg func args dbg with
    | Some (op, args) -> Rewritten (Basic (Op op), args)
    | None -> Use_default)
  (* Recognize bswap instructions *)
  | Cbswap { bitwidth } ->
    let bitwidth = select_bitwidth bitwidth in
    Rewritten (specific (Ibswap { bitwidth }), args)
  (* Other operations are regular *)
  | _ -> Use_default

let select_operation
    ~(generic_select_condition :
       Cmm.expression -> Operation.test * Cmm.expression) (op : Cmm.operation)
    (args : Cmm.expression list) dbg ~label_after :
    Cfg_selectgen_target_intf.select_operation_result =
  if !Clflags.llvm_backend
  then Use_default
  else select_operation' ~generic_select_condition op args dbg ~label_after

let select_store ~is_assign:_ _addr _exp :
    Cfg_selectgen_target_intf.select_store_result =
  Maybe_out_of_range

let is_store_out_of_range kind ~byte_offset :
    Cfg_selectgen_target_intf.is_store_out_of_range_result =
  if is_offset kind byte_offset then Within_range else Out_of_range

let insert_move_extcall_arg (ty_arg : Cmm.exttype) src dst :
    Cfg_selectgen_target_intf.insert_move_extcall_arg_result =
  let ty_arg_is_small_int =
    match ty_arg with
    | XInt32 | XInt16 | XInt8 -> true
    | XInt | XInt64 | XFloat32 | XFloat | XVec128 -> false
    | XVec256 | XVec512 -> Misc.fatal_error "arm64: got 256/512 bit vector"
  in
  if macosx && ty_arg_is_small_int && is_stack_slot dst
  then Rewritten (Op (Specific Imove32), src, dst)
  else Use_default

exception Use_default_exn

let pseudoregs_for_operation op arg res =
  match (op : Operation.t) with
  | Specific (Isimd simd_op) ->
    Simd_selection.pseudoregs_for_operation simd_op arg res
  | Specific
      ( Ifar_poll | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf
      | Imulsubf | Inegmulsubf | Isqrtf | Imove32 | Ifar_alloc _
      | Ishiftarith (_, _)
      | Ibswap _ | Isignext _ )
  | Move | Spill | Reload | Opaque | Pause | Begin_region | End_region | Dls_get
  | Tls_get | Domain_index | Poll | Const_int _ | Const_float32 _
  | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
  | Const_vec512 _ | Stackoffset _ | Load _
  | Store (_, _, _)
  | Intop _ | Int128op _
  | Intop_imm (_, _)
  | Intop_atomic _
  | Floatop (_, _)
  | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
  | Name_for_debugger _ | Alloc _ ->
    raise Use_default_exn
  | Specific (Illvm_intrinsic intr) ->
    Misc.fatal_errorf
      "Cfg_selection.pseudoregs_for_operation: Unexpected llvm_intrinsic %s: \
       not using LLVM backend"
      intr

let insert_op_debug' env sub_cfg op dbg rs rd :
    Cfg_selectgen_target_intf.insert_op_debug_result =
  try
    let rsrc, rdst = pseudoregs_for_operation op rs rd in
    Select_utils.insert_moves env sub_cfg rs rsrc;
    Select_utils.insert_debug env sub_cfg (Op op) dbg rsrc rdst;
    Select_utils.insert_moves env sub_cfg rdst rd;
    Regs rd
  with Use_default_exn -> Use_default

let insert_op_debug env sub_cfg op dbg rs rd :
    Cfg_selectgen_target_intf.insert_op_debug_result =
  if !Clflags.llvm_backend
  then Use_default
  else insert_op_debug' env sub_cfg op dbg rs rd
