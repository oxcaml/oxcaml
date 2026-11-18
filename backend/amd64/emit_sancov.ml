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

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open X86_dsl
module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label
module S = Asm_targets.Asm_symbol

module Common = Emit_sancov_common

let reset_unit = Common.reset_unit

let reset_function = Common.reset_function

(* The gate is defined hidden in this unit (see [emit_data]), so it can be
   addressed PC-relatively, even in position-independent code. *)
let emit_gate ~res =
  let gate = S.create_global Cfg_trace_pc_guard.gate in
  I.mov (mem64_rip QWORD (S.encode gate)) res

(* Call [__sanitizer_cov_trace_pc_guard(&guard)] on the C stack (or, without
   stack checks, on the current stack, once aligned), as for a C call that does
   not allocate. Unlike such a call, it preserves every register: those that are
   live and that C functions do not preserve are saved in a frame on the stack
   that the callback runs on.

   [r13] holds the original stack pointer across the call and is the CFA
   register meanwhile. It is first pushed on the current stack. Like the return
   address pushed by a call to [caml_call_gc], this word lies within the red
   zone that the runtime keeps below the stack frames of OCaml code
   ([Stack_threshold]). *)
let emit_call ~live ~move ~emit_call ~index =
  let guard = Common.guard_of_index index in
  (* The frame is 16-byte aligned, and so is every vector slot, since [move]
     uses aligned moves for [Vec128] and [Valx2] stack slots. *)
  let saved, frame_size = Common.registers_to_save ~live in
  I.push r13;
  D.cfi_adjust_cfa_offset ~bytes:8;
  I.mov rsp r13;
  D.cfi_remember_state ();
  D.cfi_def_cfa_register ~reg:"r13";
  if Config.no_stack_checks
  then I.and_ (int (-16)) rsp
  else
    I.mov
      (mem64 QWORD
         (Domainstate.idx_of_field Domain_c_stack * 8)
         (Scalar R14))
      rsp;
  if frame_size > 0 then I.sub (int frame_size) rsp;
  List.iter (fun (r, slot) -> move r slot) saved;
  I.lea (mem64_rip NONE (L.encode guard)) rdi;
  emit_call (Cmm.global_symbol Cfg_trace_pc_guard.callback);
  List.iter (fun (r, slot) -> move slot r) saved;
  I.mov r13 rsp;
  D.cfi_restore_state ();
  I.pop r13;
  D.cfi_adjust_cfa_offset ~bytes:(-8)

let emit_constructor_body ~emit_call ~start ~stop ~init:_ =
  (* Align the stack. *)
  I.push rax;
  D.cfi_adjust_cfa_offset ~bytes:8;
  I.lea (mem64_rip NONE (S.encode start)) rdi;
  I.lea (mem64_rip NONE (S.encode stop)) (Reg64 RSI);
  emit_call (Cmm.global_symbol Cfg_trace_pc_guard.init);
  I.pop rax;
  D.cfi_adjust_cfa_offset ~bytes:(-8);
  I.ret ()

let emit_data ~add_def_symbol ~emit_call =
  if Common.unit_has_guards () then add_def_symbol Cfg_trace_pc_guard.gate;
  Common.emit_data ~code_alignment:16
    ~emit_constructor_body:(emit_constructor_body ~emit_call)
