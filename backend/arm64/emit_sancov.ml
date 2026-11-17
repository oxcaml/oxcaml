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
module Ast = Arm64_ast.Ast
module A = Ast.DSL.Acc
module D = Asm_targets.Asm_directives
module H = Dsl_helpers
module L = Asm_targets.Asm_label
module O = Ast.DSL
module R = Ast.Reg
module S = Asm_targets.Asm_symbol

(* The guards of the current unit, one per call to the callback, most recent
   first. *)
let guards = ref ([] : L.t list)

(* The guards of the current function, by probe index. [Emit] emits each
   instruction several times, to measure it, and a call must then keep its
   guard. *)
let guards_of_function : (int, L.t) Hashtbl.t = Hashtbl.create 16

let reset_function () = Hashtbl.reset guards_of_function

let reset_unit () =
  reset_function ();
  guards := []

let guard_of_index index =
  match Hashtbl.find_opt guards_of_function index with
  | Some guard -> guard
  | None ->
    let guard = L.create Trace_pc_guards in
    Hashtbl.add guards_of_function index guard;
    guards := guard :: !guards;
    guard

let macosx = String.equal Config.system "macosx"

(* Scratch registers of the emitter, which the register allocator does not
   use. *)
let reg_tmp1 = Proc.phys_reg Int X16

let reg_x_tmp1 = H.reg_x reg_tmp1

(* Callee-saved register that holds the original stack pointer during the
   call, as for external calls that do not allocate. *)
let reg_saved_sp = Proc.phys_reg Int X19

let reg_x_saved_sp = H.reg_x reg_saved_sp

(* The gate is defined hidden in this unit (see [emit_data]), so it is addressed
   directly. *)
let emit_gate ~res =
  let gate = S.create_global Cfg_trace_pc_guard.gate in
  let reloc : [`Twelve] Ast.Symbol.same_unit_or_reloc =
    if macosx then Needs_reloc PAGE_OFF else Needs_reloc LOWER_TWELVE
  in
  let res_x = H.reg_x res in
  A.ins2 ADRP res_x
    (O.symbol (Ast.Symbol.create_symbol (Needs_reloc PAGE) gate));
  A.ins2 LDR res_x (H.mem_symbol_reg (H.gp_reg_of_reg res) ~reloc gate)

(* Call [__sanitizer_cov_trace_pc_guard(&guard)] on the C stack,
   as for a C call that does not allocate. Unlike such a call, it preserves
   every register: those that are live and that C functions do not preserve are
   saved in a frame on the C stack.

   [x19] holds the original stack pointer across the call and is the CFA
   register meanwhile. It is first pushed on the current stack, together with
   the link register, which holds the return address in functions without a
   frame. Like the words pushed before a call to [caml_call_realloc_stack],
   these lie within the red zone that the runtime keeps below the stack frames
   of OCaml code ([Stack_threshold]). *)
let emit_call ~live ~move ~index =
  let guard = guard_of_index index in
  (* [Outgoing] slots are addressed relative to the stack pointer, independently
     of the frame of the function. *)
  let saved, size =
    Reg.Set.fold
      (fun (r : Reg.t) (saved, offset) ->
        if Proc.may_be_destroyed_by_c_function r
        then
          let size = Select_utils.size_component r.typ in
          let offset = Misc.align offset size in
          let slot = Reg.create_at_location r.typ (Stack (Outgoing offset)) in
          (r, slot) :: saved, offset + size
        else saved, offset)
      live ([], 0)
  in
  let frame_size = Misc.align size 16 in
  A.ins3 (STP X) reg_x_saved_sp O.lr (O.mem_pre_pair ~base:R.sp ~offset:(-16));
  D.cfi_adjust_cfa_offset ~bytes:16;
  D.cfi_remember_state ();
  (* This is where the link register was just saved if the function has no
     frame; otherwise it is the slot of the frame that holds the return
     address, if the function saves it. *)
  D.cfi_offset ~reg:(R.gp_encoding R.lr) ~offset:(-8);
  A.ins_mov_from_sp ~dst:reg_x_saved_sp;
  D.cfi_def_cfa_register
    ~reg:(Int.to_string (H.gp_x_dwarf_encoding reg_x_saved_sp));
  A.ins2 LDR reg_x_tmp1 (H.domainstate_field Domain_c_stack);
  if frame_size > 0
  then
    A.ins4 SUB_immediate reg_x_tmp1 reg_x_tmp1 (O.imm frame_size)
      O.optional_none;
  A.ins_mov_to_sp ~src:reg_x_tmp1;
  List.iter (fun (r, slot) -> move r slot) saved;
  let reg_x0 = H.reg_x (Proc.phys_reg Int X0) in
  A.ins2 ADRP reg_x0
    (O.symbol (Ast.Symbol.create_label (Needs_reloc PAGE) guard));
  A.ins4 ADD_immediate reg_x0 reg_x0
    (O.symbol (Ast.Symbol.create_label (Needs_reloc PAGE_OFF) guard))
    O.optional_none;
  A.ins1 BL
    (O.symbol
       (Ast.Symbol.create_symbol (Needs_reloc CALL26)
          (S.create_global Cfg_trace_pc_guard.callback)));
  List.iter (fun (r, slot) -> move slot r) saved;
  A.ins_mov_to_sp ~src:reg_x_saved_sp;
  D.cfi_restore_state ();
  A.ins3 (LDP X) reg_x_saved_sp O.lr (O.mem_post_pair ~base:R.sp ~offset:16);
  D.cfi_adjust_cfa_offset ~bytes:(-16)

(* The constructor of the unit, as emitted by clang: it tells the coverage
   runtime where the guards of this executable or shared object are. All copies
   are in one COMDAT group, together with their [.init_array] entries, so the
   linker keeps a single one. The bounds are hidden, so that they are those of
   the object being linked, and weak, so that linking does not fail if the
   linker discards all the guards. *)
let emit_constructor () =
  let name = Cfg_trace_pc_guard.constructor in
  let ctor = S.create_global name in
  let start = S.create_global Cfg_trace_pc_guard.guards_start in
  let stop = S.create_global Cfg_trace_pc_guard.guards_stop in
  let text : Asm_targets.Asm_section.t =
    Custom
      { names = [".text." ^ name];
        flags = Some "axG";
        args = ["%progbits"; name; "comdat"];
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
  D.align ~fill:Nop ~bytes:8;
  D.define_symbol_label ~section:Text ctor;
  D.cfi_startproc ();
  A.ins3 (STP X) reg_x_tmp1 O.lr (O.mem_pre_pair ~base:R.sp ~offset:(-16));
  D.cfi_adjust_cfa_offset ~bytes:16;
  D.cfi_offset ~reg:(R.gp_encoding R.lr) ~offset:(-8);
  let load_address reg sym =
    A.ins2 ADRP reg
      (O.symbol (Ast.Symbol.create_symbol (Needs_reloc PAGE) sym));
    A.ins4 ADD_immediate reg reg
      (O.symbol (Ast.Symbol.create_symbol (Needs_reloc PAGE_OFF) sym))
      O.optional_none
  in
  load_address (H.reg_x (Proc.phys_reg Int X0)) start;
  load_address (H.reg_x (Proc.phys_reg Int X1)) stop;
  A.ins1 BL
    (O.symbol
       (Ast.Symbol.create_symbol (Needs_reloc CALL26)
          (S.create_global Cfg_trace_pc_guard.init)));
  A.ins3 (LDP X) reg_x_tmp1 O.lr (O.mem_post_pair ~base:R.sp ~offset:16);
  A.ins0 RET;
  D.cfi_endproc ();
  D.size ctor;
  D.switch_to_section_raw ~names:[".init_array.2"] ~flags:(Some "awG")
    ~args:["%init_array"; name; "comdat"]
    ~is_delayed:false;
  D.align ~fill:Zero ~bytes:8;
  D.symbol ctor

let emit_data () =
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
       object, which lets probes address it directly. *)
    let gate = S.create_global Cfg_trace_pc_guard.gate in
    D.hidden gate;
    D.type_symbol gate ~ty:Object;
    D.switch_to_section Trace_pc_guard_gate;
    D.weak gate;
    D.align ~fill:Zero ~bytes:8;
    D.define_symbol_label ~section:Trace_pc_guard_gate gate;
    D.int64 0L;
    D.size gate;
    emit_constructor ())
