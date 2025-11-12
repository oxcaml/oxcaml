(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
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

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

type trap_stack =
  | Uncaught
  | Specific_trap of Cmm.trywith_shared_label * trap_stack

let rec equal_trap_stack ts1 ts2 =
  match ts1, ts2 with
  | Uncaught, Uncaught -> true
  | Specific_trap (lbl1, ts1), Specific_trap (lbl2, ts2) ->
    Static_label.equal lbl1 lbl2 && equal_trap_stack ts1 ts2
  | Uncaught, Specific_trap _ | Specific_trap _, Uncaught -> false

type integer_comparison = Cmm.integer_comparison

let string_of_integer_comparison c =
  Printf.sprintf " %s " (Printcmm.integer_comparison c)

let equal_integer_comparison = Scalar.Integer_comparison.equal

let invert_integer_comparison = Scalar.Integer_comparison.negate

type integer_operation =
  | Iadd
  | Isub
  | Imul
  | Imulh of { signed : bool }
  | Idiv
  | Imod
  | Iand
  | Ior
  | Ixor
  | Ilsl
  | Ilsr
  | Iasr
  | Iclz of { arg_is_non_zero : bool }
  | Ictz of { arg_is_non_zero : bool }
  | Ipopcnt
  | Icomp of integer_comparison

let string_of_integer_operation = function
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh { signed } -> " *h " ^ if signed then "" else "u"
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior -> " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Iclz { arg_is_non_zero } -> Printf.sprintf "clz %B " arg_is_non_zero
  | Ictz { arg_is_non_zero } -> Printf.sprintf "ctz %B " arg_is_non_zero
  | Ipopcnt -> "popcnt "
  | Icomp cmp -> string_of_integer_comparison cmp

let is_unary_integer_operation = function
  | Iclz _ | Ictz _ | Ipopcnt -> true
  | Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
  | Iasr | Icomp _ ->
    false

let equal_integer_operation left right =
  match left, right with
  | Iadd, Iadd -> true
  | Isub, Isub -> true
  | Imul, Imul -> true
  | Imulh { signed = left }, Imulh { signed = right } -> Bool.equal left right
  | Idiv, Idiv -> true
  | Imod, Imod -> true
  | Iand, Iand -> true
  | Ior, Ior -> true
  | Ixor, Ixor -> true
  | Ilsl, Ilsl -> true
  | Ilsr, Ilsr -> true
  | Iasr, Iasr -> true
  | ( Iclz { arg_is_non_zero = left_arg_is_non_zero },
      Iclz { arg_is_non_zero = right_arg_is_non_zero } ) ->
    Bool.equal left_arg_is_non_zero right_arg_is_non_zero
  | ( Ictz { arg_is_non_zero = left_arg_is_non_zero },
      Ictz { arg_is_non_zero = right_arg_is_non_zero } ) ->
    Bool.equal left_arg_is_non_zero right_arg_is_non_zero
  | Ipopcnt, Ipopcnt -> true
  | Icomp left, Icomp right -> equal_integer_comparison left right
  | ( Iadd,
      ( Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Isub,
      ( Iadd | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Imul,
      ( Iadd | Isub | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Imulh _,
      ( Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Idiv,
      ( Iadd | Isub | Imul | Imulh _ | Imod | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Imod,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Iand | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Iand,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Ior | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ior,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ixor | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ixor,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ilsl | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ilsl,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsr
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ilsr,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Iasr,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Iclz _,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Ictz _ | Ipopcnt | Icomp _ ) )
  | ( Ictz _,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Iclz _ | Ipopcnt | Icomp _ ) )
  | ( Ipopcnt,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Iclz _ | Ictz _ | Icomp _ ) )
  | ( Icomp _,
      ( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
      | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt ) ) ->
    false

type float_width = Cmm.float_width

let equal_float_width = Cmm.equal_float_width

type float_comparison = Cmm.float_comparison

let equal_float_comparison = Cmm.equal_float_comparison

type float_operation =
  | Inegf
  | Iabsf
  | Iaddf
  | Isubf
  | Imulf
  | Idivf
  | Icompf of float_comparison

let string_of_float_operation = function
  | Iaddf -> "+."
  | Isubf -> "-."
  | Imulf -> "*."
  | Idivf -> "/."
  | Iabsf -> "abs"
  | Inegf -> "neg"
  | Icompf cmp -> Printcmm.float_comparison cmp

let format_float_operation ppf op =
  Format.fprintf ppf "%s" (string_of_float_operation op)

let equal_float_operation left right =
  match left, right with
  | Inegf, Inegf -> true
  | Iabsf, Iabsf -> true
  | Iaddf, Iaddf -> true
  | Isubf, Isubf -> true
  | Imulf, Imulf -> true
  | Idivf, Idivf -> true
  | Icompf left, Icompf right -> equal_float_comparison left right
  | Inegf, (Iabsf | Iaddf | Isubf | Imulf | Idivf | Icompf _)
  | Iabsf, (Inegf | Iaddf | Isubf | Imulf | Idivf | Icompf _)
  | Iaddf, (Inegf | Iabsf | Isubf | Imulf | Idivf | Icompf _)
  | Isubf, (Inegf | Iabsf | Iaddf | Imulf | Idivf | Icompf _)
  | Imulf, (Inegf | Iabsf | Iaddf | Isubf | Idivf | Icompf _)
  | Idivf, (Inegf | Iabsf | Iaddf | Isubf | Imulf | Icompf _)
  | Icompf _, (Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf) ->
    false

type mutable_flag =
  | Immutable
  | Mutable

let equal_mutable_flag left right =
  match left, right with
  | Immutable, Immutable | Mutable, Mutable -> true
  | (Immutable | Mutable), _ -> false

let of_ast_mutable_flag : Asttypes.mutable_flag -> mutable_flag = function
  | Immutable -> Immutable
  | Mutable -> Mutable

let to_ast_mutable_flag : mutable_flag -> Asttypes.mutable_flag = function
  | Immutable -> Immutable
  | Mutable -> Mutable

type test =
  | Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of float_width * float_comparison
  | Ioddtest
  | Ieventest

let format_test ~print_reg tst ppf arg =
  let reg = print_reg in
  match tst with
  | Itruetest -> reg ppf arg.(0)
  | Ifalsetest -> Format.fprintf ppf "not %a" reg arg.(0)
  | Iinttest cmp ->
    Format.fprintf ppf "%a%s%a" reg arg.(0)
      (string_of_integer_comparison cmp)
      reg arg.(1)
  | Iinttest_imm (cmp, n) ->
    Format.fprintf ppf "%a%s%i" reg arg.(0) (string_of_integer_comparison cmp) n
  | Ifloattest (_, cmp) ->
    Format.fprintf ppf "%a %s %a" reg arg.(0)
      (Printcmm.float_comparison cmp)
      reg arg.(1)
  | Ieventest -> Format.fprintf ppf "%a & 1 == 0" reg arg.(0)
  | Ioddtest -> Format.fprintf ppf "%a & 1 == 1" reg arg.(0)

let invert_test = function
  | Itruetest -> Ifalsetest
  | Ifalsetest -> Itruetest
  | Iinttest cmp -> Iinttest (invert_integer_comparison cmp)
  | Iinttest_imm (cmp, n) -> Iinttest_imm (invert_integer_comparison cmp, n)
  | Ifloattest (w, cmp) -> Ifloattest (w, Cmm.negate_float_comparison cmp)
  | Ieventest -> Ioddtest
  | Ioddtest -> Ieventest

type t =
  | Move
  | Spill
  | Reload
  | Const_int of nativeint (* CR-someday xclerc: change to `Targetint.t` *)
  | Const_float32 of int32
  | Const_float of int64
  | Const_symbol of Cmm.symbol
  | Const_vec128 of Cmm.vec128_bits
  | Const_vec256 of Cmm.vec256_bits
  | Const_vec512 of Cmm.vec512_bits
  | Stackoffset of int
  | Load of
      { memory_chunk : Cmm.memory_chunk;
        addressing_mode : Arch.addressing_mode;
        mutability : mutable_flag;
        is_atomic : bool
      }
  | Store of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Intop of integer_operation
  | Intop_imm of integer_operation * int
  | Intop_atomic of
      { op : Cmm.atomic_op;
        size : Cmm.atomic_bitwidth;
        addr : Arch.addressing_mode
      }
  | Floatop of float_width * float_operation
  | Csel of test
  | Reinterpret_cast of Cmm.reinterpret_cast
  | Static_cast of Cmm.static_cast
  | Probe_is_enabled of { name : string }
  | Opaque
  | Begin_region
  | End_region
  | Specific of Arch.specific_operation
  | Name_for_debugger of
      { ident : Ident.t;
        which_parameter : int option;
        provenance : Backend_var.Provenance.t option;
        regs : Reg.t array
      }
  | Dls_get
  | Tls_get
  | Poll
  | Pause
  | Alloc of
      { bytes : int;
        dbginfo : Cmm.alloc_dbginfo;
        mode : Cmm.Alloc_mode.t
      }

let is_pure = function
  | Move -> true
  | Spill -> true
  | Reload -> true
  | Const_int _ -> true
  | Const_float32 _ -> true
  | Const_float _ -> true
  | Const_symbol _ -> true
  | Const_vec128 _ -> true
  | Const_vec256 _ -> true
  | Const_vec512 _ -> true
  | Stackoffset _ -> false
  | Load _ -> true
  | Store _ -> false
  | Intop _ -> true
  | Intop_imm _ -> true
  | Intop_atomic _ -> false
  | Floatop _ -> true
  | Csel _ -> true
  | Reinterpret_cast
      ( V128_of_vec _ | V256_of_vec _ | V512_of_vec _ | Float32_of_float
      | Float32_of_int32 | Float_of_float32 | Float_of_int64 | Int64_of_float
      | Int32_of_float32 ) ->
    true
  | Static_cast _ -> true
  (* Conservative to ensure valueofint/intofvalue are not eliminated before
     emit. *)
  | Reinterpret_cast (Value_of_int | Int_of_value) -> false
  | Probe_is_enabled _ -> true
  | Opaque -> false
  | Begin_region -> false
  | End_region -> false
  | Specific s -> Arch.operation_is_pure s
  | Name_for_debugger _ -> false
  | Dls_get -> true
  | Tls_get -> true
  | Poll -> false
  | Pause -> false
  | Alloc _ -> false

(* The next 2 functions are copied almost as is from asmcomp/printmach.ml
   because there is no interface to call them. Eventually this won't be needed
   when we change cfg to have its own types rather than referring back to mach
   and cmm. *)
(* CR-someday gyorsh: implement desc printing, and args/res/dbg, etc, properly,
   with regs, use the dreaded Format. *)

let intcomp (comp : integer_comparison) =
  Printf.sprintf " %s " (Printcmm.integer_comparison comp)

let intop (op : integer_operation) =
  match op with
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh { signed : bool } -> " *h" ^ if signed then " " else "u "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior -> " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Ipopcnt -> " pop "
  | Iclz _ -> " clz "
  | Ictz _ -> " ctz "
  | Icomp cmp -> intcomp cmp

let floatop ppf (op : float_operation) =
  match op with
  | Iaddf -> Format.fprintf ppf "+."
  | Isubf -> Format.fprintf ppf "-."
  | Imulf -> Format.fprintf ppf "*."
  | Idivf -> Format.fprintf ppf "/."
  | Iabsf -> Format.fprintf ppf "abs"
  | Inegf -> Format.fprintf ppf "neg"
  | Icompf cmp -> Format.fprintf ppf "%s" (Printcmm.float_comparison cmp)

let dump ppf op =
  match op with
  | Move -> Format.fprintf ppf "mov"
  | Spill -> Format.fprintf ppf "spill"
  | Reload -> Format.fprintf ppf "reload"
  | Const_int n -> Format.fprintf ppf "const_int %nd" n
  | Const_float32 f ->
    Format.fprintf ppf "const_float32 %Fs" (Int32.float_of_bits f)
  | Const_float f -> Format.fprintf ppf "const_float %F" (Int64.float_of_bits f)
  | Const_symbol s -> Format.fprintf ppf "const_symbol %s" s.sym_name
  | Const_vec128 { word0; word1 } ->
    Format.fprintf ppf "const vec128 %016Lx:%016Lx" word0 word1
  | Const_vec256 { word0; word1; word2; word3 } ->
    Format.fprintf ppf "const vec256 %016Lx:%016Lx:%016Lx:%016Lx" word0 word1
      word2 word3
  | Const_vec512 { word0; word1; word2; word3; word4; word5; word6; word7 } ->
    Format.fprintf ppf
      "const vec512 %016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx"
      word0 word1 word2 word3 word4 word5 word6 word7
  | Stackoffset n -> Format.fprintf ppf "stackoffset %d" n
  | Load _ -> Format.fprintf ppf "load"
  | Store _ -> Format.fprintf ppf "store"
  | Intop op -> Format.fprintf ppf "intop %s" (intop op)
  | Intop_imm (op, n) -> Format.fprintf ppf "intop %s %d" (intop op) n
  | Intop_atomic { op; size = _; addr = _ } ->
    Format.fprintf ppf "intop atomic %s" (Printcmm.atomic_op op)
  | Floatop (Float64, op) -> Format.fprintf ppf "floatop %a" floatop op
  | Floatop (Float32, op) -> Format.fprintf ppf "float32op %a" floatop op
  | Csel _ -> Format.fprintf ppf "csel"
  | Reinterpret_cast cast ->
    Format.fprintf ppf "%s" (Printcmm.reinterpret_cast cast)
  | Static_cast cast -> Format.fprintf ppf "%s" (Printcmm.static_cast cast)
  | Specific specific ->
    Format.fprintf ppf "specific %s" (Arch.specific_operation_name specific)
  | Probe_is_enabled { name } -> Format.fprintf ppf "probe_is_enabled %s" name
  | Opaque -> Format.fprintf ppf "opaque"
  | Begin_region -> Format.fprintf ppf "beginregion"
  | End_region -> Format.fprintf ppf "endregion"
  | Name_for_debugger _ -> Format.fprintf ppf "name_for_debugger"
  | Dls_get -> Format.fprintf ppf "dls_get"
  | Tls_get -> Format.fprintf ppf "tls_get"
  | Poll -> Format.fprintf ppf "poll"
  | Pause -> Format.fprintf ppf "pause"
  | Alloc { bytes; dbginfo = _; mode = Heap } ->
    Format.fprintf ppf "alloc %i" bytes
  | Alloc { bytes; dbginfo = _; mode = Local } ->
    Format.fprintf ppf "alloc_local %i" bytes

let equal_alloc_dbginfo_item left right =
  Int.equal left.Cmm.alloc_words right.Cmm.alloc_words
  && Cmm.equal_alloc_block_kind left.Cmm.alloc_block_kind
       right.Cmm.alloc_block_kind
  && Debuginfo.compare left.Cmm.alloc_dbg right.Cmm.alloc_dbg = 0

let equal_alloc_dbginfo left right =
  List.equal equal_alloc_dbginfo_item left right

let equal_test left right =
  match left, right with
  | Itruetest, Itruetest
  | Ifalsetest, Ifalsetest
  | Ioddtest, Ioddtest
  | Ieventest, Ieventest ->
    true
  | Iinttest left_cmp, Iinttest right_cmp ->
    equal_integer_comparison left_cmp right_cmp
  | Iinttest_imm (left_cmp, left_n), Iinttest_imm (right_cmp, right_n) ->
    equal_integer_comparison left_cmp right_cmp && Int.equal left_n right_n
  | Ifloattest (left_w, left_cmp), Ifloattest (right_w, right_cmp) ->
    equal_float_width left_w right_w
    && equal_float_comparison left_cmp right_cmp
  | ( ( Itruetest | Ifalsetest | Iinttest _ | Iinttest_imm _ | Ifloattest _
      | Ioddtest | Ieventest ),
      _ ) ->
    false

let equal left right =
  match left, right with
  | Move, Move | Spill, Spill | Reload, Reload -> true
  | Const_int left_n, Const_int right_n -> Nativeint.equal left_n right_n
  | Const_float32 left_f, Const_float32 right_f -> Int32.equal left_f right_f
  | Const_float left_f, Const_float right_f -> Int64.equal left_f right_f
  | Const_symbol left_s, Const_symbol right_s -> Cmm.equal_symbol left_s right_s
  | Const_vec128 left_v, Const_vec128 right_v ->
    Int64.equal left_v.Cmm.word0 right_v.Cmm.word0
    && Int64.equal left_v.Cmm.word1 right_v.Cmm.word1
  | Const_vec256 left_v, Const_vec256 right_v ->
    Int64.equal left_v.Cmm.word0 right_v.Cmm.word0
    && Int64.equal left_v.Cmm.word1 right_v.Cmm.word1
    && Int64.equal left_v.Cmm.word2 right_v.Cmm.word2
    && Int64.equal left_v.Cmm.word3 right_v.Cmm.word3
  | Const_vec512 left_v, Const_vec512 right_v ->
    Int64.equal left_v.Cmm.word0 right_v.Cmm.word0
    && Int64.equal left_v.Cmm.word1 right_v.Cmm.word1
    && Int64.equal left_v.Cmm.word2 right_v.Cmm.word2
    && Int64.equal left_v.Cmm.word3 right_v.Cmm.word3
    && Int64.equal left_v.Cmm.word4 right_v.Cmm.word4
    && Int64.equal left_v.Cmm.word5 right_v.Cmm.word5
    && Int64.equal left_v.Cmm.word6 right_v.Cmm.word6
    && Int64.equal left_v.Cmm.word7 right_v.Cmm.word7
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
    && Arch.equal_addressing_mode left_addr right_addr
    && equal_mutable_flag left_mut right_mut
    && Bool.equal left_atomic right_atomic
  | ( Store (left_chunk, left_addr, left_assign),
      Store (right_chunk, right_addr, right_assign) ) ->
    Cmm.equal_memory_chunk left_chunk right_chunk
    && Arch.equal_addressing_mode left_addr right_addr
    && Bool.equal left_assign right_assign
  | Intop left_op, Intop right_op -> equal_integer_operation left_op right_op
  | Intop_imm (left_op, left_n), Intop_imm (right_op, right_n) ->
    equal_integer_operation left_op right_op && Int.equal left_n right_n
  | ( Intop_atomic { op = left_op; size = left_size; addr = left_addr },
      Intop_atomic { op = right_op; size = right_size; addr = right_addr } ) ->
    Cmm.equal_atomic_op left_op right_op
    && Cmm.equal_atomic_bitwidth left_size right_size
    && Arch.equal_addressing_mode left_addr right_addr
  | Floatop (left_w, left_op), Floatop (right_w, right_op) ->
    equal_float_width left_w right_w && equal_float_operation left_op right_op
  | Csel left_test, Csel right_test -> equal_test left_test right_test
  | Reinterpret_cast left_c, Reinterpret_cast right_c ->
    Cmm.equal_reinterpret_cast left_c right_c
  | Static_cast left_c, Static_cast right_c ->
    Cmm.equal_static_cast left_c right_c
  | ( Probe_is_enabled { name = left_name },
      Probe_is_enabled { name = right_name } ) ->
    String.equal left_name right_name
  | Opaque, Opaque | Begin_region, Begin_region | End_region, End_region -> true
  | Specific left_op, Specific right_op ->
    Arch.equal_specific_operation left_op right_op
  | ( Name_for_debugger
        { ident = left_ident;
          which_parameter = left_wp;
          provenance = left_prov;
          regs = _
        },
      Name_for_debugger
        { ident = right_ident;
          which_parameter = right_wp;
          provenance = right_prov;
          regs = _
        } ) ->
    Ident.same left_ident right_ident
    && Option.equal Int.equal left_wp right_wp
    && Option.equal Backend_var.Provenance.equal left_prov right_prov
  | Dls_get, Dls_get | Tls_get, Tls_get | Poll, Poll | Pause, Pause -> true
  | ( Alloc { bytes = left_bytes; dbginfo = left_dbg; mode = left_mode },
      Alloc { bytes = right_bytes; dbginfo = right_dbg; mode = right_mode } ) ->
    Int.equal left_bytes right_bytes
    && equal_alloc_dbginfo left_dbg right_dbg
    && Cmm.Alloc_mode.equal left_mode right_mode
  | ( ( Move | Spill | Reload | Const_int _ | Const_float32 _ | Const_float _
      | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
      | Stackoffset _ | Load _ | Store _ | Intop _ | Intop_imm _
      | Intop_atomic _ | Floatop _ | Csel _ | Reinterpret_cast _ | Static_cast _
      | Probe_is_enabled _ | Opaque | Begin_region | End_region | Specific _
      | Name_for_debugger _ | Dls_get | Tls_get | Poll | Pause | Alloc _ ),
      _ ) ->
    false
