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

(* The guards of the current unit, one per call to the callback, most recent
   first. *)
let guards = ref ([] : L.t list)

let reset_unit () = guards := []

(* The gate is defined hidden in this unit (see [emit_data]), so it can be
   addressed PC-relatively, even in position-independent code. *)
let emit_gate ~res =
  let gate = S.create_global Cfg_trace_pc_guard.gate in
  I.mov (mem64_rip QWORD (S.encode gate)) res

(* Call [__sanitizer_cov_trace_pc_guard(&guard)] on the C stack
   (or, without stack checks, on the current stack, once aligned), as for a C
   call that does not allocate. Unlike such a call, it preserves every register:
   those that are live and that C functions do not preserve are saved in a frame
   on the stack that the callback runs on.

   [r13] holds the original stack pointer across the call and is the CFA
   register meanwhile. It is first pushed on the current stack. Like the return
   address pushed by a call to [caml_call_gc], this word lies within the red
   zone that the runtime keeps below the stack frames of OCaml code
   ([Stack_threshold]). *)
let emit_call ~live ~move ~emit_call =
  let guard = L.create Trace_pc_guards in
  guards := guard :: !guards;
  (* The frame is 16-byte aligned, and so is every vector slot, since [move]
     uses aligned moves for [Vec128] and [Valx2] stack slots. [Outgoing] slots
     are addressed relative to the stack pointer, independently of the frame of
     the function. *)
  let saved, size =
    Reg.Set.fold
      (fun (r : Reg.t) (saved, offset) ->
        if Proc.may_be_destroyed_by_c_function r
        then
          let size = Select_utils.size_component r.typ in
          let offset = Misc.align offset (Int.min size 16) in
          let slot = Reg.create_at_location r.typ (Stack (Outgoing offset)) in
          (r, slot) :: saved, offset + size
        else saved, offset)
      live ([], 0)
  in
  let frame_size = Misc.align size 16 in
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

(* The constructor of the unit, as emitted by clang: it tells the coverage
   runtime where the guards of this executable or shared object are. All copies
   are in one COMDAT group, together with their [.init_array] entries, so the
   linker keeps a single one. The bounds are hidden, so that they are those of
   the object being linked, and weak, so that linking does not fail if the
   linker discards all the guards. *)
let emit_constructor ~emit_call =
  let name = Cfg_trace_pc_guard.constructor in
  let ctor = S.create_global name in
  let start = S.create_global Cfg_trace_pc_guard.guards_start in
  let stop = S.create_global Cfg_trace_pc_guard.guards_stop in
  let text : Asm_targets.Asm_section.t =
    Custom
      { names = [".text." ^ name];
        flags = Some "axG";
        args = ["@progbits"; name; "comdat"];
        is_delayed = false
      }
  in
  D.switch_to_section text;
  D.weak start;
  D.hidden start;
  D.weak stop;
  D.hidden stop;
  D.weak ctor;
  D.hidden ctor;
  D.type_symbol ctor ~ty:Function;
  D.align ~fill:Nop ~bytes:16;
  D.define_symbol_label ~section:Text ctor;
  D.cfi_startproc ();
  (* Align the stack. *)
  I.push rax;
  D.cfi_adjust_cfa_offset ~bytes:8;
  I.lea (mem64_rip NONE (S.encode start)) rdi;
  I.lea (mem64_rip NONE (S.encode stop)) (Reg64 RSI);
  emit_call (Cmm.global_symbol Cfg_trace_pc_guard.init);
  I.pop rax;
  D.cfi_adjust_cfa_offset ~bytes:(-8);
  I.ret ();
  D.cfi_endproc ();
  D.size ctor;
  D.switch_to_section_raw ~names:[".init_array.2"] ~flags:(Some "awG")
    ~args:["@init_array"; name; "comdat"]
    ~is_delayed:false;
  D.align ~fill:Zero ~bytes:8;
  D.symbol ctor

let emit_data ~add_def_symbol ~emit_call =
  if not (Misc.Stdlib.List.is_empty !guards)
  then (
    D.switch_to_section Trace_pc_guards;
    D.align ~fill:Zero ~bytes:4;
    List.iter
      (fun lbl ->
        D.define_label lbl;
        D.int32 0l)
      (List.rev !guards);
    (* The gate, defined as by clang: every instrumented unit has a zero
       default, which a strong definition elsewhere (e.g. in the coverage
       runtime) overrides. Being hidden, it is resolved within the linked
       object, which lets probes address it PC-relatively. *)
    let gate = S.create_global Cfg_trace_pc_guard.gate in
    add_def_symbol Cfg_trace_pc_guard.gate;
    D.hidden gate;
    D.type_symbol gate ~ty:Object;
    D.switch_to_section Trace_pc_guard_gate;
    D.weak gate;
    D.align ~fill:Zero ~bytes:8;
    D.define_symbol_label ~section:Trace_pc_guard_gate gate;
    D.int64 0L;
    D.size gate;
    emit_constructor ~emit_call)
