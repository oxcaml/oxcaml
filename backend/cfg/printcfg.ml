[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
module Array = ArrayLabels
module DLL = Oxcaml_utils.Doubly_linked_list
module List = ListLabels

let instr_prefix : type a. Format.formatter -> a Cfg.instruction -> unit =
 fun fmt instr ->
  Format.fprintf fmt "%t%a%t " Cfg_colours.instr_id InstructionId.format_padded
    instr.id Cfg_colours.pop;
  if Array.length instr.res > 0
  then Format.fprintf fmt "%a := " Printreg.regs instr.res

let instr_suffix : type a.
    Format.formatter ->
    a Cfg.instruction * Cfg_with_infos.liveness option ->
    unit =
 fun fmt (instr, liveness) ->
  if Array.length instr.arg > 0
  then Format.fprintf fmt " %a" Printreg.regs instr.arg;
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
    Cfg_with_infos.liveness option ->
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
      Format.fprintf fmt "%a%t%a%t%a\n" instr_prefix instr Cfg_colours.basic
        Cfg.dump_basic instr.desc Cfg_colours.pop instr_suffix (instr, liveness));
  Format.fprintf fmt "%a%t%a%t%a\n\n" instr_prefix block.terminator
    Cfg_colours.terminator
    (Cfg.dump_terminator ~sep:", ")
    block.terminator.desc Cfg_colours.pop instr_suffix
    (block.terminator, liveness)

let format :
    Format.formatter ->
    Cfg.t ->
    Label.t array ->
    Cfg_with_infos.liveness option ->
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

let cfg_with_infos : Format.formatter -> Cfg_with_infos.t -> unit =
 fun fmt cfg_with_infos ->
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let labels = cfg_with_layout |> Cfg_with_layout.layout |> DLL.to_array in
  format fmt
    (Cfg_with_layout.cfg cfg_with_layout)
    labels
    (Cfg_with_infos.liveness_if_available cfg_with_infos)
