(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let subst_label = Cfg_equiv_subst.subst_label

let subst_symbol = Cfg_equiv_subst.subst_symbol

let subst_func_symbol = Cfg_equiv_subst.subst_func_symbol

let equiv_func_call_operation subst (left : Cfg.func_call_operation)
    (right : Cfg.func_call_operation) =
  match left, right with
  | Cfg.Indirect left_callees, Cfg.Indirect right_callees ->
    Option.equal
      (List.equal (fun ls rs -> Cmm.equal_symbol (subst_symbol subst ls) rs))
      left_callees right_callees
  | Cfg.Direct left_sym, Cfg.Direct right_sym ->
    Cmm.equal_symbol (subst_symbol subst left_sym) right_sym
  | (Cfg.Indirect _ | Cfg.Direct _), _ -> false

let equiv_external_call_operation subst (left : Cfg.external_call_operation)
    (right : Cfg.external_call_operation) =
  String.equal (subst_func_symbol subst left.func_symbol) right.func_symbol
  && Bool.equal left.alloc right.alloc
  && Cmm.equal_effects left.effects right.effects
  && Cmm.equal_machtype left.ty_res right.ty_res
  && List.equal Cmm.equal_exttype left.ty_args right.ty_args
  && Int.equal left.stack_ofs right.stack_ofs
  && Cmm.equal_stack_align left.stack_align right.stack_align

let equiv_prim_call_operation subst (left : Cfg.prim_call_operation)
    (right : Cfg.prim_call_operation) =
  match left, right with
  | Cfg.External left_ext, Cfg.External right_ext ->
    equiv_external_call_operation subst left_ext right_ext
  | ( Cfg.Probe
        { name = left_name;
          handler_code_sym = left_handler;
          enabled_at_init = left_enabled
        },
      Cfg.Probe
        { name = right_name;
          handler_code_sym = right_handler;
          enabled_at_init = right_enabled
        } ) ->
    String.equal left_name right_name
    (* CR xclerc: [handler_code_sym] is a code symbol; consider whether it
       should go through [subst_func_symbol]. *)
    && String.equal left_handler right_handler
    && Bool.equal left_enabled right_enabled
  | (Cfg.External _ | Cfg.Probe _), _ -> false

let equiv_bool_test subst
    ({ ifso = left_ifso; ifnot = left_ifnot } : Cfg.bool_test)
    ({ ifso = right_ifso; ifnot = right_ifnot } : Cfg.bool_test) =
  Label.equal (subst_label subst left_ifso) right_ifso
  && Label.equal (subst_label subst left_ifnot) right_ifnot

let equiv_int_test subst
    ({ lt = left_lt;
       eq = left_eq;
       gt = left_gt;
       is_signed = left_is_signed;
       imm = left_imm
     } :
      Cfg.int_test)
    ({ lt = right_lt;
       eq = right_eq;
       gt = right_gt;
       is_signed = right_is_signed;
       imm = right_imm
     } :
      Cfg.int_test) =
  Label.equal (subst_label subst left_lt) right_lt
  && Label.equal (subst_label subst left_eq) right_eq
  && Label.equal (subst_label subst left_gt) right_gt
  && Scalar.Signedness.equal left_is_signed right_is_signed
  && Option.equal Int.equal left_imm right_imm

let equiv_float_test subst
    ({ width = left_width;
       lt = left_lt;
       eq = left_eq;
       gt = left_gt;
       uo = left_uo
     } :
      Cfg.float_test)
    ({ width = right_width;
       lt = right_lt;
       eq = right_eq;
       gt = right_gt;
       uo = right_uo
     } :
      Cfg.float_test) =
  Cmm.equal_float_width left_width right_width
  && Label.equal (subst_label subst left_lt) right_lt
  && Label.equal (subst_label subst left_eq) right_eq
  && Label.equal (subst_label subst left_gt) right_gt
  && Label.equal (subst_label subst left_uo) right_uo

let equiv_test (left : Operation.test) (right : Operation.test) =
  match left, right with
  | Itruetest, Itruetest
  | Ifalsetest, Ifalsetest
  | Ioddtest, Ioddtest
  | Ieventest, Ieventest ->
    true
  | Iinttest left_cmp, Iinttest right_cmp ->
    Operation.equal_integer_comparison left_cmp right_cmp
  | Iinttest_imm (left_cmp, left_n), Iinttest_imm (right_cmp, right_n) ->
    Operation.equal_integer_comparison left_cmp right_cmp
    && Int.equal left_n right_n
  | Ifloattest (left_w, left_cmp), Ifloattest (right_w, right_cmp) ->
    Operation.equal_float_width left_w right_w
    && Operation.equal_float_comparison left_cmp right_cmp
  | ( ( Itruetest | Ifalsetest | Ioddtest | Ieventest | Iinttest _
      | Iinttest_imm _ | Ifloattest _ ),
      _ ) ->
    false

let equiv_operation subst (left : Operation.t) (right : Operation.t) =
  let equiv_addr = Cfg_equiv_target.equiv_addressing_mode subst in
  match left, right with
  | Move, Move | Spill, Spill | Reload, Reload | Dummy_use, Dummy_use -> true
  | Const_int left_n, Const_int right_n -> Nativeint.equal left_n right_n
  | Const_float32 left_f, Const_float32 right_f -> Int32.equal left_f right_f
  | Const_float left_f, Const_float right_f -> Int64.equal left_f right_f
  | Const_symbol left_sym, Const_symbol right_sym ->
    Cmm.equal_symbol (subst_symbol subst left_sym) right_sym
  | ( Const_vec128 { Cmm.word0 = left_w0; word1 = left_w1 },
      Const_vec128 { Cmm.word0 = right_w0; word1 = right_w1 } ) ->
    Int64.equal left_w0 right_w0 && Int64.equal left_w1 right_w1
  | ( Const_vec256
        { Cmm.word0 = left_w0;
          word1 = left_w1;
          word2 = left_w2;
          word3 = left_w3
        },
      Const_vec256
        { Cmm.word0 = right_w0;
          word1 = right_w1;
          word2 = right_w2;
          word3 = right_w3
        } ) ->
    Int64.equal left_w0 right_w0
    && Int64.equal left_w1 right_w1
    && Int64.equal left_w2 right_w2
    && Int64.equal left_w3 right_w3
  | ( Const_vec512
        { Cmm.word0 = left_w0;
          word1 = left_w1;
          word2 = left_w2;
          word3 = left_w3;
          word4 = left_w4;
          word5 = left_w5;
          word6 = left_w6;
          word7 = left_w7
        },
      Const_vec512
        { Cmm.word0 = right_w0;
          word1 = right_w1;
          word2 = right_w2;
          word3 = right_w3;
          word4 = right_w4;
          word5 = right_w5;
          word6 = right_w6;
          word7 = right_w7
        } ) ->
    Int64.equal left_w0 right_w0
    && Int64.equal left_w1 right_w1
    && Int64.equal left_w2 right_w2
    && Int64.equal left_w3 right_w3
    && Int64.equal left_w4 right_w4
    && Int64.equal left_w5 right_w5
    && Int64.equal left_w6 right_w6
    && Int64.equal left_w7 right_w7
  | Stackoffset left_n, Stackoffset right_n -> Int.equal left_n right_n
  | ( Load
        { memory_chunk = left_chunk;
          addressing_mode = left_addr;
          mutability = left_mut;
          is_atomic = left_atomic
        },
      Load
        { memory_chunk = right_chunk;
          addressing_mode = right_addr;
          mutability = right_mut;
          is_atomic = right_atomic
        } ) ->
    Cmm.equal_memory_chunk left_chunk right_chunk
    && equiv_addr left_addr right_addr
    && Operation.equal_mutable_flag left_mut right_mut
    && Bool.equal left_atomic right_atomic
  | ( Store (left_chunk, left_addr, left_assign),
      Store (right_chunk, right_addr, right_assign) ) ->
    Cmm.equal_memory_chunk left_chunk right_chunk
    && equiv_addr left_addr right_addr
    && Bool.equal left_assign right_assign
  | Intop left_op, Intop right_op ->
    Operation.equal_integer_operation left_op right_op
  | Int128op left_op, Int128op right_op ->
    Operation.equal_int128_operation left_op right_op
  | Intop_imm (left_op, left_n), Intop_imm (right_op, right_n) ->
    Operation.equal_integer_operation left_op right_op
    && Int.equal left_n right_n
  | ( Intop_atomic { op = left_op; size = left_size; addr = left_addr },
      Intop_atomic { op = right_op; size = right_size; addr = right_addr } ) ->
    Cmm.equal_atomic_op left_op right_op
    && Cmm.equal_atomic_bitwidth left_size right_size
    && equiv_addr left_addr right_addr
  | Floatop (left_w, left_op), Floatop (right_w, right_op) ->
    Operation.equal_float_width left_w right_w
    && Operation.equal_float_operation left_op right_op
  | Csel left_test, Csel right_test -> equiv_test left_test right_test
  | Reinterpret_cast left_c, Reinterpret_cast right_c ->
    Cmm.equal_reinterpret_cast left_c right_c
  | Static_cast left_c, Static_cast right_c ->
    Cmm.equal_static_cast left_c right_c
  | ( Probe_is_enabled { name = left_name; enabled_at_init = left_eai },
      Probe_is_enabled { name = right_name; enabled_at_init = right_eai } ) ->
    String.equal left_name right_name
    && Option.equal Bool.equal left_eai right_eai
  | Opaque, Opaque | Begin_region, Begin_region | End_region, End_region -> true
  | Specific left_spec, Specific right_spec ->
    Cfg_equiv_target.equiv_specific_operation subst left_spec right_spec
  | Name_for_debugger _, Name_for_debugger _ ->
    (* CR xclerc: we should ignore [Name_for_debugger] instructions when
       comparing lists of instructions. *)
    true
  | Dls_get, Dls_get | Tls_get, Tls_get -> true
  | Domain_index, Domain_index -> true
  | Poll, Poll | Pause, Pause -> true
  | ( Alloc { bytes = left_bytes; dbginfo = _; mode = left_mode },
      Alloc { bytes = right_bytes; dbginfo = _; mode = right_mode } ) ->
    (* CR xclerc: see the CR in [equiv_instruction] about [dbg]. *)
    Int.equal left_bytes right_bytes
    && Cmm.Alloc_mode.equal left_mode right_mode
  | ( ( Move | Spill | Reload | Dummy_use | Const_int _ | Const_float32 _
      | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
      | Const_vec512 _ | Stackoffset _ | Load _ | Store _ | Intop _ | Int128op _
      | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _
      | Static_cast _ | Probe_is_enabled _ | Opaque | Begin_region | End_region
      | Specific _ | Name_for_debugger _ | Dls_get | Tls_get | Domain_index
      | Poll | Pause | Alloc _ ),
      _ ) ->
    false

let equiv_basic subst (left : Cfg.basic) (right : Cfg.basic) =
  match left, right with
  | Cfg.Op left_op, Cfg.Op right_op -> equiv_operation subst left_op right_op
  | Cfg.Reloadretaddr, Cfg.Reloadretaddr -> true
  | ( Cfg.Pushtrap { lbl_handler = left_lbl },
      Cfg.Pushtrap { lbl_handler = right_lbl } ) ->
    Label.equal (subst_label subst left_lbl) right_lbl
  | ( Cfg.Poptrap { lbl_handler = left_lbl },
      Cfg.Poptrap { lbl_handler = right_lbl } ) ->
    Label.equal (subst_label subst left_lbl) right_lbl
  | Cfg.Prologue, Cfg.Prologue | Cfg.Epilogue, Cfg.Epilogue -> true
  | ( Cfg.Stack_check { max_frame_size_bytes = left_size },
      Cfg.Stack_check { max_frame_size_bytes = right_size } ) ->
    Int.equal left_size right_size
  | ( ( Cfg.Op _ | Cfg.Reloadretaddr | Cfg.Pushtrap _ | Cfg.Poptrap _
      | Cfg.Prologue | Cfg.Epilogue | Cfg.Stack_check _ ),
      _ ) ->
    false

let equiv_terminator subst (left : Cfg.terminator) (right : Cfg.terminator) =
  match left, right with
  | Cfg.Never, Cfg.Never -> true
  | Cfg.Always left_lbl, Cfg.Always right_lbl ->
    Label.equal (subst_label subst left_lbl) right_lbl
  | Cfg.Parity_test left_test, Cfg.Parity_test right_test
  | Cfg.Truth_test left_test, Cfg.Truth_test right_test ->
    equiv_bool_test subst left_test right_test
  | Cfg.Float_test left_test, Cfg.Float_test right_test ->
    equiv_float_test subst left_test right_test
  | Cfg.Int_test left_test, Cfg.Int_test right_test ->
    equiv_int_test subst left_test right_test
  | Cfg.Switch left_labels, Cfg.Switch right_labels ->
    Int.equal (Array.length left_labels) (Array.length right_labels)
    && Array.for_all2
         (fun left_lbl right_lbl ->
           Label.equal (subst_label subst left_lbl) right_lbl)
         left_labels right_labels
  | Cfg.Return, Cfg.Return -> true
  | Cfg.Raise left_kind, Cfg.Raise right_kind ->
    Lambda.equal_raise_kind left_kind right_kind
  | ( Cfg.Tailcall_self { destination = left_dest },
      Cfg.Tailcall_self { destination = right_dest } ) ->
    Label.equal (subst_label subst left_dest) right_dest
  | Cfg.Tailcall_func left_op, Cfg.Tailcall_func right_op ->
    equiv_func_call_operation subst left_op right_op
  | Cfg.Call_no_return left_ext, Cfg.Call_no_return right_ext ->
    equiv_external_call_operation subst left_ext right_ext
  | ( Cfg.Call { op = left_op; label_after = left_lbl },
      Cfg.Call { op = right_op; label_after = right_lbl } ) ->
    equiv_func_call_operation subst left_op right_op
    && Label.equal (subst_label subst left_lbl) right_lbl
  | ( Cfg.Prim { op = left_op; label_after = left_lbl },
      Cfg.Prim { op = right_op; label_after = right_lbl } ) ->
    equiv_prim_call_operation subst left_op right_op
    && Label.equal (subst_label subst left_lbl) right_lbl
  | ( Cfg.Invalid
        { message = left_msg;
          stack_ofs = left_ofs;
          stack_align = left_align;
          label_after = left_lbl
        },
      Cfg.Invalid
        { message = right_msg;
          stack_ofs = right_ofs;
          stack_align = right_align;
          label_after = right_lbl
        } ) ->
    String.equal left_msg right_msg
    && Int.equal left_ofs right_ofs
    && Cmm.equal_stack_align left_align right_align
    && Option.equal
         (fun l r -> Label.equal (subst_label subst l) r)
         left_lbl right_lbl
  | ( ( Cfg.Never | Cfg.Always _ | Cfg.Parity_test _ | Cfg.Truth_test _
      | Cfg.Float_test _ | Cfg.Int_test _ | Cfg.Switch _ | Cfg.Return
      | Cfg.Raise _ | Cfg.Tailcall_self _ | Cfg.Tailcall_func _
      | Cfg.Call_no_return _ | Cfg.Call _ | Cfg.Prim _ | Cfg.Invalid _ ),
      _ ) ->
    false

let same_loc r1 r2 =
  Reg.same_loc_fatal_on_unknown
    ~fatal_message:"Cfg_equiv: unknown register location." r1 r2

let equiv_instruction : type a.
    equiv_desc:(Cfg_equiv_subst.t -> a -> a -> bool) ->
    Cfg_equiv_subst.t ->
    a Cfg.instruction ->
    a Cfg.instruction ->
    bool =
 fun ~equiv_desc subst left right ->
  (* CR xclerc: we have to drop `dbg`, but that might be a problem;
   * as long as the feature using the function are opt-in that might
   * be OK. *)
  match left, right with
  | ( { Cfg.desc = left_desc;
        id = _;
        arg = left_arg;
        res = left_res;
        dbg = _;
        fdo = _;
        live = _;
        stack_offset = left_stack_offset;
        available_before = _;
        available_across = _
      },
      { Cfg.desc = right_desc;
        id = _;
        arg = right_arg;
        res = right_res;
        dbg = _;
        fdo = _;
        live = _;
        stack_offset = right_stack_offset;
        available_before = _;
        available_across = _
      } ) ->
    Int.equal left_stack_offset right_stack_offset
    && Misc.Stdlib.Array.equal same_loc left_arg right_arg
    && Misc.Stdlib.Array.equal same_loc left_res right_res
    && equiv_desc subst left_desc right_desc

let equiv_basic_block subst (left : Cfg.basic_block) (right : Cfg.basic_block) =
  match left, right with
  | ( { Cfg.start = left_start;
        body = left_body;
        terminator = left_terminator;
        predecessors = _;
        stack_offset = left_stack_offset;
        exn = left_exn;
        can_raise = _;
        is_trap_handler = left_is_trap_handler;
        cold = left_cold
      },
      { Cfg.start = right_start;
        body = right_body;
        terminator = right_terminator;
        predecessors = _;
        stack_offset = right_stack_offset;
        exn = right_exn;
        can_raise = _;
        is_trap_handler = right_is_trap_handler;
        cold = right_cold
      } ) ->
    Label.equal (subst_label subst left_start) right_start
    && Int.equal left_stack_offset right_stack_offset
    && Option.equal
         (fun l r -> Label.equal (subst_label subst l) r)
         left_exn right_exn
    && Bool.equal left_is_trap_handler right_is_trap_handler
    && Bool.equal left_cold right_cold
    && DLL.equal
         (equiv_instruction ~equiv_desc:equiv_basic subst)
         left_body right_body
    && equiv_instruction ~equiv_desc:equiv_terminator subst left_terminator
         right_terminator

let equiv_cfg_with_layout ~symbols ~func_symbols (left : Cfg_with_layout.t)
    (right : Cfg_with_layout.t) =
  let left_layout = Cfg_with_layout.layout left in
  let right_layout = Cfg_with_layout.layout right in
  let left_len = DLL.length left_layout in
  let right_len = DLL.length right_layout in
  if not (Int.equal left_len right_len)
  then false
  else begin
    let labels = Label.Tbl.create left_len in
    DLL.iter2 left_layout right_layout ~f:(fun left_lbl right_lbl ->
        Label.Tbl.add labels left_lbl right_lbl);
    let subst = { Cfg_equiv_subst.labels; symbols; func_symbols } in
    let left_cfg = Cfg_with_layout.cfg left in
    let right_cfg = Cfg_with_layout.cfg right in
    DLL.for_all left_layout ~f:(fun left_lbl ->
        let left_block = Cfg.get_block_exn left_cfg left_lbl in
        let right_lbl = Cfg_equiv_subst.subst_label subst left_lbl in
        let right_block = Cfg.get_block_exn right_cfg right_lbl in
        equiv_basic_block subst left_block right_block)
  end
