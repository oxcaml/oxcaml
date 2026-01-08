(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

module D = Asm_targets.Asm_directives
module S = Asm_targets.Asm_symbol
module L = Asm_targets.Asm_label
module String = Misc.Stdlib.String

type probe =
  { stack_offset : int;
    num_stack_slots : int Stack_class.Tbl.t;
    probe_name : string;
    probe_enabled_at_init : bool;
    probe_handler_code_sym : string;
    (* Record frame info held in the corresponding mutable variables. *)
    probe_label : Label.t;
    (* Probe site, recorded in .note.stapsdt section for enabling and disabling
       the probes *)
    probe_insn : Linear.instruction
        (* Iprobe instruction, recorded at probe site and used for emitting the
           notes and the wrapper code at the end of the compilation unit. *)
  }

let probes = ref []

let get_probes () = !probes

let add_probe ~stack_offset ~num_stack_slots ~probe_name ~probe_enabled_at_init
    ~probe_handler_code_sym ~probe_label ~probe_insn =
  let probe =
    { stack_offset;
      num_stack_slots;
      probe_name;
      probe_enabled_at_init;
      probe_handler_code_sym;
      probe_label;
      probe_insn
    }
  in
  probes := probe :: !probes

type semaphore_data = string * Asm_targets.Asm_symbol.t * bool option

let probe_semaphores = ref String.Map.empty

let find_or_add_semaphore name enabled_at_init dbg =
  match String.Map.find_opt name !probe_semaphores with
  | Some (label, symbol, e) ->
    (match e, enabled_at_init with
    | None, None -> ()
    | None, Some _ ->
      let d = label, symbol, enabled_at_init in
      probe_semaphores
        := String.Map.remove name !probe_semaphores |> String.Map.add name d
    | Some _, None ->
      (* [find_or_add_semaphore] is called with None for Iprobe_is_enabled
         during code emission only. [find_or_add_semaphore] is called with Some
         to emit probe notes only after all code is emitted. *)
      assert (not !Clflags.emit_optimized_probes)
    | Some b, Some b' ->
      if not (Bool.equal b b')
      then raise (Emitaux.Error (Inconsistent_probe_init (name, dbg))));
    label
  | None ->
    let symbol = S.Predef.caml_probes_semaphore ~name in
    let sym = S.to_raw_string symbol in
    let d = sym, symbol, enabled_at_init in
    probe_semaphores := String.Map.add name d !probe_semaphores;
    sym

let emit_probe_note_desc ~probe_name ~probe_label ~semaphore_label ~probe_args =
  (match probe_label with
  | Some probe_label ->
    let lbl = L.create_int Stapsdt_note (Label.to_int probe_label) in
    D.label lbl
  | None -> D.int64 0L);
  (match Target_system.is_macos () with
  | false -> D.symbol S.Predef.stapsdt_base
  | true -> D.int64 0L);
  D.symbol semaphore_label;
  D.string "ocaml_2\000";
  D.string (probe_name ^ "\000");
  D.string (probe_args ^ "\000")

let emit_probe_notes0 ~slot_offset =
  D.switch_to_section Stapsdt_note;
  let stap_arg (arg : Reg.t) =
    let arg_name =
      match arg.loc with
      | Stack s ->
        Printf.sprintf "%d(%%rsp)"
          (slot_offset s (Stack_class.of_machtype arg.Reg.typ))
      | Reg reg -> Reg_class.register_name arg.Reg.typ reg
      | Unknown ->
        Misc.fatal_errorf "Cannot create probe: illegal argument: %a"
          Printreg.reg arg
    in
    Printf.sprintf "%d@%s" (Select_utils.size_component arg.Reg.typ) arg_name
  in
  let describe_one_probe p =
    let probe_name = p.probe_name in
    let enabled_at_init = p.probe_enabled_at_init in
    let args =
      Array.fold_right (fun arg acc -> stap_arg arg :: acc) p.probe_insn.arg []
      |> String.concat " "
    in
    let semsym =
      find_or_add_semaphore probe_name (Some enabled_at_init) p.probe_insn.dbg
    in
    let semaphore_label = S.create_global semsym in
    let emit_desc () =
      emit_probe_note_desc ~probe_label:(Some p.probe_label) ~semaphore_label
        ~probe_name ~probe_args:args
    in
    Emitaux.emit_elf_note ~section:Stapsdt_note ~owner:"stapsdt" ~typ:3l
      ~emit_desc
  in
  List.iter describe_one_probe !probes

let emit_dummy_probe_notes () =
  (* A semaphore may be used via [%probe_is_enabled] without a corresponding
     probe site in the same compilation unit or even the entire program due to
     inlining or optimizations or user defined special cases. Emit dummy probe
     notes to correctly toggle such semaphores. A dummy note has no associated
     probe site or probe handler. *)
  let describe_dummy_probe ~probe_name sym =
    let semaphore_label = S.create_global sym in
    let emit_desc () =
      emit_probe_note_desc ~probe_name ~probe_label:None ~semaphore_label
        ~probe_args:""
    in
    Emitaux.emit_elf_note ~section:Stapsdt_note ~owner:"stapsdt" ~typ:3l
      ~emit_desc
  in
  let semaphores_without_probes =
    List.fold_left
      (fun acc probe -> String.Map.remove probe.probe_name acc)
      !probe_semaphores !probes
  in
  if not (String.Map.is_empty semaphores_without_probes)
  then (
    D.switch_to_section Stapsdt_note;
    String.Map.iter
      (fun probe_name (sym, _, _) -> describe_dummy_probe ~probe_name sym)
      semaphores_without_probes)

let emit_probe_semaphores ~add_def_symbol =
  (match Target_system.is_macos () with
  | false ->
    Emitaux.emit_stapsdt_base_section ();
    D.switch_to_section Probes
  | true -> D.switch_to_section Probes);
  D.align ~fill_x86_bin_emitter:Zero
    ~bytes:(if Target_system.is_macos () then 8 else 2);
  String.Map.iter
    (fun _ (label, label_sym, enabled_at_init) ->
      (* Unresolved weak symbols have a zero value regardless of the following
         initialization. *)
      let enabled_at_init = Option.value enabled_at_init ~default:false in
      if not (Target_system.is_macos ())
      then (
        D.weak label_sym;
        D.hidden label_sym);
      D.define_symbol_label ~section:Probes label_sym;
      D.int16 (Numbers.Int16.of_int_exn 0);
      (* for systemtap probes *)
      D.int16 (Numbers.Int16.of_int_exn (Bool.to_int enabled_at_init));
      (* for ocaml probes *)
      add_def_symbol label)
    !probe_semaphores

let emit_probe_notes ~slot_offset ~add_def_symbol =
  (match !probes with [] -> () | _ -> emit_probe_notes0 ~slot_offset);
  if not (String.Map.is_empty !probe_semaphores)
  then (
    emit_dummy_probe_notes ();
    emit_probe_semaphores ~add_def_symbol)

let reset () =
  probes := [];
  probe_semaphores := String.Map.empty
