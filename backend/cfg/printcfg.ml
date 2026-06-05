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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
module Array = ArrayLabels
module DLL = Oxcaml_utils.Doubly_linked_list
module List = ListLabels

(* Compose [print_reg] with a [Cfg_colours.argument] colour wrap, so that
   register arguments stand out from the surrounding op-body colour. *)
let with_arg_colour print_reg ppf r =
  Cfg_colours.argument ppf;
  print_reg ppf r;
  Cfg_colours.pop ppf

let basic_desc ppf (basic : Cfg.basic) =
  let open Format in
  match basic with
  | Op op -> Operation.dump ppf op
  | Reloadretaddr -> fprintf ppf "reloadretaddr"
  | Pushtrap { lbl_handler } ->
    fprintf ppf "pushtrap handler=%a" Label.format lbl_handler
  | Poptrap { lbl_handler } ->
    fprintf ppf "poptrap handler=%a" Label.format lbl_handler
  | Prologue -> fprintf ppf "prologue"
  | Epilogue -> fprintf ppf "epilogue"
  | Stack_check { max_frame_size_bytes } ->
    fprintf ppf "stack_check size=%d" max_frame_size_bytes

(* Shared implementation between [terminator_desc] (no args, desc-only) and
   [print_terminator_body] (args drawn from the instruction). *)
let terminator_body_with_args ?(print_reg = Printreg.reg) ?(args = [||])
    ?(sep = "\n") ppf (terminator : Cfg.terminator) =
  let first_arg =
    if Array.length args >= 1
    then Format.fprintf Format.str_formatter " %a" print_reg args.(0);
    Format.flush_str_formatter ()
  in
  let second_arg =
    if Array.length args >= 2
    then Format.fprintf Format.str_formatter " %a" print_reg args.(1);
    Format.flush_str_formatter ()
  in
  let print_args ppf args =
    if Array.length args = 0
    then ()
    else Format.fprintf ppf " %a" (Printreg.regs' ~print_reg) args
  in
  let dump_linear_call_op ppf op =
    Printlinear.call_operation ~print_reg ppf op args
  in
  let open Format in
  match terminator with
  | Never -> fprintf ppf "deadend"
  | Always l -> fprintf ppf "goto %a" Label.format l
  | Parity_test { ifso; ifnot } ->
    fprintf ppf "if even%s goto %a%selse goto %a" first_arg Label.format ifso
      sep Label.format ifnot
  | Truth_test { ifso; ifnot } ->
    fprintf ppf "if true%s goto %a%selse goto %a" first_arg Label.format ifso
      sep Label.format ifnot
  | Float_test { width = _; lt; eq; gt; uo } ->
    fprintf ppf "if%s <%s goto %a%s" first_arg second_arg Label.format lt sep;
    fprintf ppf "if%s =%s goto %a%s" first_arg second_arg Label.format eq sep;
    fprintf ppf "if%s >%s goto %a%s" first_arg second_arg Label.format gt sep;
    fprintf ppf "else goto %a" Label.format uo
  | Int_test { lt; eq; gt; is_signed; imm } ->
    let cmp =
      Printf.sprintf " %s%s"
        (match is_signed with Signed -> "s" | Unsigned -> "u")
        (match imm with None -> second_arg | Some i -> " " ^ Int.to_string i)
    in
    fprintf ppf "if%s <%s goto %a%s" first_arg cmp Label.format lt sep;
    fprintf ppf "if%s =%s goto %a%s" first_arg cmp Label.format eq sep;
    fprintf ppf "if%s >%s goto %a" first_arg cmp Label.format gt
  | Switch labels ->
    fprintf ppf "switch%s%s" first_arg sep;
    let label_count = Array.length labels in
    if label_count >= 1
    then (
      for i = 0 to label_count - 2 do
        fprintf ppf "case %d: goto %a%s" i Label.format labels.(i) sep
      done;
      let i = label_count - 1 in
      fprintf ppf "case %d: goto %a" i Label.format labels.(i))
  | Call_no_return { func_symbol; _ } ->
    fprintf ppf "call_no_return %s%a" func_symbol print_args args
  | Return -> fprintf ppf "return%a" print_args args
  | Raise _ -> fprintf ppf "raise%a" print_args args
  | Tailcall_self { destination } ->
    dump_linear_call_op ppf
      (Linear.Ltailcall_imm
         { func =
             { sym_name =
                 Printf.sprintf "self(%s)" (Label.to_string destination);
               sym_global = Local
             }
         })
  | Tailcall_func call ->
    (* CR ncourant: here and below, maybe the callees should be printed when
       they are known *)
    dump_linear_call_op ppf
      (match call with
      | Indirect _callees -> Linear.Ltailcall_ind
      | Direct func -> Linear.Ltailcall_imm { func })
  | Call { op = call; label_after } ->
    dump_linear_call_op ppf
      (match call with
      | Indirect _callees -> Linear.Lcall_ind
      | Direct func -> Linear.Lcall_imm { func });
    Format.fprintf ppf "%s\n           goto %a" sep Label.format label_after
  | Prim { op = prim; label_after } ->
    dump_linear_call_op ppf
      (match prim with
      | External
          { func_symbol = func;
            ty_res;
            ty_args;
            alloc;
            stack_ofs;
            stack_align;
            effects = _
          } ->
        Linear.Lextcall
          { func;
            ty_res;
            ty_args;
            returns = true;
            alloc;
            stack_ofs;
            stack_align
          }
      | Probe { name; handler_code_sym; enabled_at_init } ->
        Linear.Lprobe { name; handler_code_sym; enabled_at_init });
    Format.fprintf ppf "%sgoto %a" sep Label.format label_after
  | Invalid { message; label_after; _ } ->
    Format.fprintf ppf "Invalid %S" message;
    Option.iter (Format.fprintf ppf "%sgoto %a" sep Label.format) label_after

let terminator_desc ?sep ppf terminator =
  terminator_body_with_args ?sep ppf terminator

let basic_body ?(print_reg = Printreg.reg) ppf
    (instr : Cfg.basic Cfg.instruction) =
  match instr.desc with
  | Op op -> Printoperation.operation_body ~print_reg op instr.arg ppf
  | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Epilogue | Stack_check _
    ->
    basic_desc ppf instr.desc

let terminator_body ?(print_reg = Printreg.reg) ?(sep = "\n") ppf
    (ti : Cfg.terminator Cfg.instruction) =
  terminator_body_with_args ~print_reg ~args:ti.arg ~sep ppf ti.desc

(* Common wrapping for [basic] and [terminator]: prints [<colour>res := <pop>]
   if [res] is non-empty, followed by the body printed via [print_body] with
   argument registers wrapped in [Cfg_colours.argument] on top of [print_reg].
   The whole thing is wrapped in [outer_colour]. *)
let instruction_with_colours ~outer_colour ~print_reg ~res ~print_body ppf =
  outer_colour ppf;
  if Array.length res > 0
  then (
    Cfg_colours.result ppf;
    Printoperation.result_prefix ~print_reg ppf res;
    Cfg_colours.pop ppf);
  print_body ~print_reg:(with_arg_colour print_reg) ppf;
  Cfg_colours.pop ppf

let basic_with_print_reg ~print_reg ppf (instr : Cfg.basic Cfg.instruction) =
  instruction_with_colours ~outer_colour:Cfg_colours.basic ~print_reg
    ~res:instr.res
    ~print_body:(fun ~print_reg ppf -> basic_body ~print_reg ppf instr)
    ppf

let basic ppf instr = basic_with_print_reg ~print_reg:Printreg.reg ppf instr

(* Internal: full-options terminator printer, used by [terminator],
   [terminator_with_print_reg], and the block printer (which needs [~sep:",
   "]). *)
let terminator_full ~print_reg ?(sep = "") ppf
    (ti : Cfg.terminator Cfg.instruction) =
  instruction_with_colours ~outer_colour:Cfg_colours.terminator ~print_reg
    ~res:ti.res
    ~print_body:(fun ~print_reg ppf -> terminator_body ~print_reg ~sep ppf ti)
    ppf

let terminator_with_print_reg ~print_reg ppf ti =
  terminator_full ~print_reg ppf ti

let terminator ppf ti = terminator_full ~print_reg:Printreg.reg ppf ti

let instruction_with_print_reg ~print_reg ppf = function
  | `Basic i -> basic_with_print_reg ~print_reg ppf i
  | `Terminator i -> terminator_with_print_reg ~print_reg ppf i

let instruction ppf i = instruction_with_print_reg ~print_reg:Printreg.reg ppf i

(* Helpers for the rich block printer below. *)

let print_id : type a. Format.formatter -> a Cfg.instruction -> unit =
 fun fmt instr ->
  Format.fprintf fmt "%t%a%t " Cfg_colours.instr_id InstructionId.format_padded
    instr.id Cfg_colours.pop

let print_liveness : type a.
    Format.formatter ->
    a Cfg.instruction * Cfg_liveness.domain InstructionId.Tbl.t option ->
    unit =
 fun fmt (instr, liveness) ->
  match liveness with
  | None -> ()
  | Some liveness -> (
    match InstructionId.Tbl.find_opt liveness instr.id with
    | None ->
      Format.fprintf fmt " %t(no liveness found)%t" Cfg_colours.liveness
        Cfg_colours.pop
    | Some { before = _; across = live } -> (
      match Reg.Set.is_empty live with
      | true -> ()
      | false ->
        Format.fprintf fmt " %tlive:[|%a|]%t" Cfg_colours.liveness
          Printreg.regset live Cfg_colours.pop))

let label_set : Format.formatter -> Label.Set.t -> unit =
 fun fmt set ->
  Format.fprintf fmt "{";
  Label.Set.iter (fun label -> Format.fprintf fmt " %a" Label.print label) set;
  Format.fprintf fmt " }"

let block :
    Format.formatter ->
    Cfg.basic_block ->
    Cfg_liveness.domain InstructionId.Tbl.t option ->
    unit =
 fun fmt block liveness ->
  Format.fprintf fmt "block %t%a%t%s%s -"
    (if block.is_trap_handler
     then Cfg_colours.block_label_exn
     else Cfg_colours.block_label)
    Label.format block.start Cfg_colours.pop
    (match block.is_trap_handler with false -> "" | true -> " [handler]")
    (match block.cold with false -> "" | true -> " [cold]");
  Format.fprintf fmt " %tpredecessors: %a%t" Cfg_colours.pred_succ label_set
    block.predecessors Cfg_colours.pop;
  Format.fprintf fmt " %tnormal successors: %a%t" Cfg_colours.pred_succ
    label_set
    (Cfg.successor_labels block ~normal:true ~exn:false)
    Cfg_colours.pop;
  (match block.exn with
  | None -> ()
  | Some exn_label ->
    Format.fprintf fmt " %texceptional successor: %a%t" Cfg_colours.pred_succ
      Label.format exn_label Cfg_colours.pop);
  Format.fprintf fmt "\n";
  DLL.iter block.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
      print_id fmt instr;
      basic fmt instr;
      print_liveness fmt (instr, liveness);
      Format.fprintf fmt "\n");
  print_id fmt block.terminator;
  terminator_full ~print_reg:Printreg.reg ~sep:", " fmt block.terminator;
  print_liveness fmt (block.terminator, liveness);
  Format.fprintf fmt "\n\n"

let format :
    Format.formatter ->
    Cfg.t ->
    Label.t array ->
    Cfg_liveness.domain InstructionId.Tbl.t option ->
    unit =
 fun fmt cfg labels liveness ->
  Format.fprintf fmt "cfg for %t%s%t\n" Cfg_colours.function_name cfg.fun_name
    Cfg_colours.pop;
  Format.fprintf fmt "  args: %a\n" Printreg.regs cfg.fun_args;
  Format.fprintf fmt "  ret_type: %a\n" Printcmm.machtype cfg.fun_ret_type;
  Format.fprintf fmt "  entry_label: %a\n" Label.format cfg.entry_label;
  let regalloc =
    List.find_map cfg.fun_codegen_options ~f:(function[@ocaml.warning "-4"]
      | Cfg.Use_regalloc regalloc -> Some regalloc
      | _ -> None)
  in
  (match regalloc with
  | None -> ()
  | Some regalloc ->
    Format.fprintf fmt "  use_regalloc: %a\n" Clflags.Register_allocator.format
      regalloc);
  let regalloc_params =
    List.find_map cfg.fun_codegen_options ~f:(function[@ocaml.warning "-4"]
      | Cfg.Use_regalloc_param params -> Some params
      | _ -> None)
  in
  (match regalloc_params with
  | None -> ()
  | Some regalloc_params ->
    Format.fprintf fmt "  regalloc_params: %s\n"
      (String.concat ", " regalloc_params));
  Format.fprintf fmt "\n";
  Array.iter labels ~f:(fun label ->
      block fmt (Cfg.get_block_exn cfg label) liveness);
  Format.fprintf fmt "%!"

let cfg : Format.formatter -> Cfg.t -> unit =
 fun fmt cfg ->
  let labels = cfg.blocks |> Label.Tbl.to_seq_keys |> Array.of_seq in
  Array.sort ~cmp:Label.compare labels;
  format fmt cfg labels None

let cfg_with_layout : Format.formatter -> Cfg_with_layout.t -> unit =
 fun fmt cfg_with_layout ->
  let labels = cfg_with_layout |> Cfg_with_layout.layout |> DLL.to_array in
  format fmt (Cfg_with_layout.cfg cfg_with_layout) labels None
