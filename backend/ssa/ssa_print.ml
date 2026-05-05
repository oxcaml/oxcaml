[@@@ocaml.warning "+a-40-41-42"]

(** Pretty-printer for {!Ssa.Finished_graph} instances.

    Format conventions:
    - Op result names are [v<id>] (or [<name>/v<id>] when the op carries a name
      from a Cmm let-binding), block ids are [B<id>], block params are
      [B<id>.<index>].
    - References to ops use the result name, references to block params use the
      [B<id>.<index>] form, projections render as nested [v<i>.<index>].
    - Each block prints its header (id, kind, typed params, label hint, idom,
      depth, predecessors), then its body one instruction per line, then its
      terminator with the implicit {!Ssa.trap_successor} appended as
      [trap_successor=B<id>] when applicable. *)

module Make (S : Ssa.Finished_graph) = struct
  let print_block_id ppf (b : S.Block.t) =
    Format.fprintf ppf "B%d" (b.id :> int)

  let print_block_param ppf ((b : S.Block.t), index) =
    let p : Ssa_intf.block_param = b.params.(index) in
    match p.name with
    | None -> Format.fprintf ppf "%a.%d" print_block_id b index
    | Some n -> Format.fprintf ppf "%s/%a.%d" n print_block_id b index

  let print_op_id ppf (od : S.Instruction.op_data) =
    match od.name with
    | None -> Format.fprintf ppf "v%d" (od.id :> int)
    | Some n -> Format.fprintf ppf "%s/v%d" n (od.id :> int)

  let rec print_instruction ppf (i : S.Instruction.t) =
    match i with
    | Op ({ op; args; _ } as od) ->
      let op_str = Format.asprintf "%a" Operation.dump op in
      if Array.length args = 0
      then Format.fprintf ppf "%a = %s" print_op_id od op_str
      else
        let formatted_op =
          if String.contains op_str ' ' then "(" ^ op_str ^ ")" else op_str
        in
        Format.fprintf ppf "%a = %s(%a)" print_op_id od formatted_op print_args
          args
    | Push_trap { handler } ->
      Format.fprintf ppf "push_trap %a"
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "<invalid>")
           print_block_id)
        handler
    | Pop_trap { handler } ->
      Format.fprintf ppf "pop_trap %a"
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "<invalid>")
           print_block_id)
        handler
    | Stack_check { max_frame_size_bytes } ->
      Format.fprintf ppf "stack_check %d" max_frame_size_bytes
    | Name_for_debugger { ident; _ } ->
      Format.fprintf ppf "name_for_debugger %a" Ident.print ident
    | Block_param _ | Tuple _ | Proj _ -> assert false

  and print_instr_ref ppf (i : S.Instruction.t) =
    match i with
    | Op od -> print_op_id ppf od
    | Block_param { block; index; _ } -> print_block_param ppf (block, index)
    | Proj { index; src } ->
      Format.fprintf ppf "%d.%a" index print_instr_ref src
    | Tuple elems -> Format.fprintf ppf "tuple(%a)" print_args elems
    | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ ->
      assert false

  and print_args ppf args =
    Array.iteri
      (fun i arg ->
        if i > 0 then Format.fprintf ppf ", ";
        print_instr_ref ppf arg)
      args

  let print_terminator ppf (t : S.Terminator.t) =
    match t with
    | Goto { goto; args } ->
      Format.fprintf ppf "goto %a(%a)" print_block_id goto print_args args
    | Branch { cond; ifso; ifnot } ->
      Format.fprintf ppf "if %a then goto %a else goto %a" print_instr_ref cond
        print_block_id ifso print_block_id ifnot
    | Switch { index; targets } ->
      Format.fprintf ppf "switch(%a) [" print_instr_ref index;
      Array.iteri
        (fun i tgt ->
          if i > 0 then Format.fprintf ppf ", ";
          print_block_id ppf tgt)
        targets;
      Format.fprintf ppf "]"
    | Return { args } -> Format.fprintf ppf "return(%a)" print_args args
    | Raise { args; _ } -> Format.fprintf ppf "raise(%a)" print_args args
    | Tailcall_self { destination; args } ->
      Format.fprintf ppf "tailcall_self %a(%a)" print_block_id destination
        print_args args
    | Tailcall_func { args; _ } ->
      Format.fprintf ppf "tailcall_func(%a)" print_args args
    | Call { op = Func (Direct sym); args; continuation; may_raise; nontail } ->
      Format.fprintf ppf "call %s(%a) -> %a" sym.sym_name print_args args
        print_block_id continuation;
      if may_raise then Format.fprintf ppf " may_raise";
      if nontail then Format.fprintf ppf " nontail"
    | Call { op = Func (Indirect _); args; continuation; may_raise; nontail } ->
      Format.fprintf ppf "call_indirect(%a) -> %a" print_args args
        print_block_id continuation;
      if may_raise then Format.fprintf ppf " may_raise";
      if nontail then Format.fprintf ppf " nontail"
    | Call
        { op = Prim (External { func_symbol; _ });
          args;
          continuation;
          may_raise;
          nontail = _
        } ->
      Format.fprintf ppf "prim %s(%a) -> %a" func_symbol print_args args
        print_block_id continuation;
      if may_raise then Format.fprintf ppf " may_raise"
    | Call { op = Prim (Probe { name; _ }); args; continuation; _ } ->
      Format.fprintf ppf "probe %s(%a) -> %a" name print_args args
        print_block_id continuation
    | Invalid { message = _; args; continuation } -> (
      Format.fprintf ppf "invalid(%a)" print_args args;
      match continuation with
      | Some l -> Format.fprintf ppf " -> %a" print_block_id l
      | None -> ())

  let print_typed_params ppf (blk : S.Block.t) =
    Array.iteri
      (fun i (p : Ssa_intf.block_param) ->
        if i > 0 then Format.fprintf ppf ", ";
        Format.fprintf ppf "%a : %a" print_block_param (blk, i)
          Printcmm.machtype_component p.typ)
      blk.params

  let print_block_header ppf (blk : S.Block.t) =
    let name = if blk.is_function_start then "FUNCTION_START" else "BLOCK" in
    Format.fprintf ppf "%a: %s(%a)" print_block_id blk name print_typed_params
      blk;
    Format.fprintf ppf " [idom=%a depth=%d]" print_block_id
      blk.dominator_info.dominator blk.dominator_info.depth;
    match S.predecessors blk with
    | [] -> ()
    | preds ->
      Format.fprintf ppf " <- %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           print_block_id)
        preds

  let print_block ppf (blk : S.Block.t) =
    Format.fprintf ppf "%a@." print_block_header blk;
    Array.iter
      (fun bi -> Format.fprintf ppf "  %a@." print_instruction bi)
      blk.body;
    Format.fprintf ppf "  %a" print_terminator blk.terminator;
    (match S.trap_successor blk with
    | None -> ()
    | Some h -> Format.fprintf ppf " trap_successor=%a" print_block_id h);
    Format.fprintf ppf "@.@."

  let print ppf =
    Format.fprintf ppf "ssa %s(%a)@." S.function_info.fun_name Printcmm.machtype
      S.function_info.fun_args;
    Format.fprintf ppf "  entry = %a@.@." print_block_id S.entry;
    List.iter (print_block ppf) S.blocks;
    Format.fprintf ppf "@."
end

let print ppf (m : (module Ssa.Finished_graph)) =
  let module S = (val m : Ssa.Finished_graph) in
  let module P = Make (S) in
  P.print ppf
