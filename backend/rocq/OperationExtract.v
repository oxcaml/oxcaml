(******************************************************************************
 *                                  OxCaml                                    *
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

(* Extraction directives for Operation.v, trusted by inspection:
   [integer_operation] and [operation] are [Operation.integer_operation] and
   [Operation.t], whose constructors must be listed in the same order as in
   backend/operation.mli; the abstract payload types are the compiler's,
   inlined so that this module extracts to nothing. *)

From Stdlib Require Import Extraction.
From OxCamlBackend Require Import Operation.

Extract Inlined Constant symbol => "Cmm.symbol".
Extract Inlined Constant vec128_bits => "Cmm.vec128_bits".
Extract Inlined Constant vec256_bits => "Cmm.vec256_bits".
Extract Inlined Constant vec512_bits => "Cmm.vec512_bits".
Extract Inlined Constant memory_chunk => "Cmm.memory_chunk".
Extract Inlined Constant addressing_mode => "Arch.addressing_mode".
Extract Inlined Constant reinterpret_cast => "Cmm.reinterpret_cast".
Extract Inlined Constant static_cast => "Cmm.static_cast".
Extract Inlined Constant specific_operation => "Arch.specific_operation".
Extract Inlined Constant signedness => "Operation.signedness".
Extract Inlined Constant load => "Operation.load".
Extract Inlined Constant intop_atomic => "Operation.intop_atomic".
Extract Inlined Constant probe_is_enabled => "Operation.probe_is_enabled".
Extract Inlined Constant name_for_debugger => "Operation.name_for_debugger".
Extract Inlined Constant alloc => "Operation.alloc".
Extract Inlined Constant int128_operation => "Operation.int128_operation".
Extract Inlined Constant float_width => "Operation.float_width".
Extract Inlined Constant float_operation => "Operation.float_operation".
Extract Inlined Constant test => "Operation.test".

Extract Inductive integer_operation => "Operation.integer_operation"
  [ "Operation.Iadd" "Operation.Isub" "Operation.Imul" "Operation.Imulh"
    "Operation.Idiv" "Operation.Imod" "Operation.Iand" "Operation.Ior"
    "Operation.Ixor" "Operation.Inot" "Operation.Ilsl" "Operation.Ilsr"
    "Operation.Iasr" "Operation.Iclz" "Operation.Ictz" "Operation.Ipopcnt"
    "Operation.Icomp" ].
Extract Inductive operation => "Operation.t"
  [ "Operation.Move" "Operation.Spill" "Operation.Reload"
    "Operation.Const_int" "Operation.Const_float32" "Operation.Const_float"
    "Operation.Const_symbol" "Operation.Const_vec128"
    "Operation.Const_vec256" "Operation.Const_vec512" "Operation.Const_mask"
    "Operation.Stackoffset" "Operation.Load" "Operation.Store"
    "Operation.Intop" "Operation.Int128op" "Operation.Intop_imm"
    "Operation.Intop_atomic" "Operation.Floatop" "Operation.Csel"
    "Operation.Reinterpret_cast" "Operation.Static_cast"
    "Operation.Probe_is_enabled" "Operation.Opaque" "Operation.Begin_region"
    "Operation.End_region" "Operation.Specific" "Operation.Name_for_debugger"
    "Operation.Dls_get" "Operation.Tls_get" "Operation.Domain_index"
    "Operation.Poll" "Operation.Pause" "Operation.Alloc" ].
