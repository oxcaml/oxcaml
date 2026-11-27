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
module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label
module S = Asm_targets.Asm_symbol

type t =
  { mutable guards : L.t list;  (** Those of the unit, most recent first. *)
    guards_of_function : (int, L.t) Hashtbl.t
        (** Those of the current function, by probe index. *)
  }

let create () = { guards = []; guards_of_function = Hashtbl.create 16 }

let start_function t = Hashtbl.reset t.guards_of_function

let guard_of_index t index =
  match Hashtbl.find_opt t.guards_of_function index with
  | Some guard -> guard
  | None ->
    let guard = L.create Trace_pc_guards in
    Hashtbl.add t.guards_of_function index guard;
    t.guards <- guard :: t.guards;
    guard

let unit_has_guards t = not (Misc.Stdlib.List.is_empty t.guards)

(* [Outgoing] slots are addressed relative to the stack pointer, independently
   of the frame of the function. Every slot is aligned to its size, up to 16
   bytes. *)
let registers_to_save ~live =
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
  saved, Misc.align size 16

(* With the weak definition, every instrumented unit has a zero default, which a
   definition that is not weak (e.g. in the coverage runtime) overrides. Being
   hidden, it is resolved within the linked object, which lets probes address it
   directly. *)
let emit_gate_definition ~weak =
  let gate = S.create_global Cfg_trace_pc_guard.gate in
  if weak then D.hidden gate;
  D.type_symbol gate ~ty:Object;
  D.switch_to_section Trace_pc_guard_gate;
  if weak then D.weak gate else D.global gate;
  D.align ~fill:Zero ~bytes:8;
  D.define_symbol_label ~section:Trace_pc_guard_gate gate;
  D.int64 0L;
  D.size gate

(* The constructor of the unit: it tells the coverage runtime where the guards
   of this executable or shared object are. All copies are in one COMDAT group,
   together with their [.init_array] entries, so the linker keeps a single one.
   The bounds are hidden, so that they are those of the object being linked, and
   weak, so that linking does not fail if the linker discards all the guards. *)
let emit_constructor ~code_alignment ~emit_constructor_body =
  let ctor = S.create_global Cfg_trace_pc_guard.constructor in
  let start = S.create_global Cfg_trace_pc_guard.guards_start in
  let stop = S.create_global Cfg_trace_pc_guard.guards_stop in
  let init = S.create_global Cfg_trace_pc_guard.init in
  D.switch_to_section Trace_pc_guard_constructor;
  D.weak start;
  D.hidden start;
  D.weak stop;
  D.hidden stop;
  D.weak ctor;
  D.hidden ctor;
  D.type_symbol ctor ~ty:Function;
  D.align ~fill:Nop ~bytes:code_alignment;
  D.define_symbol_label ~section:Trace_pc_guard_constructor ctor;
  D.cfi_startproc ();
  emit_constructor_body ~start ~stop ~init;
  D.cfi_endproc ();
  D.size ctor;
  D.switch_to_section Trace_pc_guard_init_array;
  D.align ~fill:Zero ~bytes:8;
  D.symbol ctor

let emit_data t ~code_alignment ~emit_constructor_body =
  if unit_has_guards t
  then (
    D.switch_to_section Trace_pc_guards;
    D.align ~fill:Zero ~bytes:4;
    List.iter
      (fun lbl ->
        D.define_label lbl;
        D.int32 0l)
      (List.rev t.guards);
    emit_gate_definition ~weak:true;
    emit_constructor ~code_alignment ~emit_constructor_body)
