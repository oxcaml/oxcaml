open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(** Post-finish SSA invariants. Most structural invariants are enforced by the
    graph builder. Here, we just verify that SSA definitions dominate their uses
    and that trap stacks are consistent. *)

module Make (S : Ssa.Finished_graph) = struct
  let error fmt =
    Format.kasprintf
      (fun s ->
        Misc.fatal_errorf "SSA validation (%s): %s" S.function_info.fun_name s)
      fmt

  let pb = S.print_block_id

  (* The trap stack at entry to [bl] must agree across all predecessors: on a
     normal edge it's the predecessor's [block_end_trap_stack]; on an trap edge
     the runtime pops the handler, so the successor sees [List.tl] of the
     predecessor's stack. *)
  let check_trap_stacks (bl : S.Block.t) =
    let trap_stack_at_entry_from (pred : S.Block.t) : S.Block.t list =
      let via_exception_edge =
        match S.trap_successor pred with
        | Some h -> S.Block.equal h bl
        | None -> false
      in
      if via_exception_edge
      then List.tl pred.block_end_trap_stack
      else pred.block_end_trap_stack
    in
    let print_trap_stack ppf stack =
      Format.fprintf ppf "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
           pb)
        stack
    in
    match S.predecessors bl |> S.Block.Set.elements with
    | [] -> ()
    | first_pred :: rest ->
      let expected = trap_stack_at_entry_from first_pred in
      rest
      |> List.iter (fun pred ->
          let actual = trap_stack_at_entry_from pred in
          if not (List.equal S.Block.equal expected actual)
          then
            error
              "block %a: trap-stack mismatch at entry — predecessor %a brings \
               %a, predecessor %a brings %a"
              pb bl pb first_pred print_trap_stack expected pb pred
              print_trap_stack actual)

  let validate () =
    let block_set = S.Block.Tbl.create 16 in
    List.iter (fun bl -> S.Block.Tbl.replace block_set bl ()) S.blocks;
    let block_exists b = S.Block.Tbl.mem block_set b in
    let defined_ops = S.Instruction_id.Tbl.create 64 in
    let rec check_arg (bl : S.Block.t) (i : S.Instruction.t) =
      match i with
      | Op { id; _ } -> (
        match S.Instruction_id.Tbl.find_opt defined_ops id with
        | None -> error "block %a: v%d used but not defined" pb bl (id :> int)
        | Some def_block ->
          if not (S.dominates def_block bl)
          then
            error "block %a: v%d defined in non-dominating block %a" pb bl
              (id :> int)
              pb def_block)
      | Block_param { block; _ } ->
        if not (block_exists block)
        then
          error "block %a: Block_param references non-existent block %a" pb bl
            pb block;
        if not (S.dominates block bl)
        then
          error "block %a: Block_param of non-dominating block %a" pb bl pb
            block
      | Proj { src; _ } -> (
        match src with
        | Op _ -> check_arg bl src
        | Tuple _ | Block_param _ | Proj _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ->
          error "block %a: Proj source must be an Op" pb bl)
      | Tuple _ ->
        error "block %a: Tuple must be short-circuited by Proj, not used as arg"
          pb bl
      | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ ->
        error "block %a: non-value instruction used as argument" pb bl
    in
    let check_args bl args = Array.iter (check_arg bl) args in
    let visit_block (bl : S.Block.t) =
      Array.iter
        (fun (i : S.Instruction.t) ->
          match i with
          | Op { id; args; _ } ->
            check_args bl args;
            if S.Instruction_id.Tbl.mem defined_ops id
            then error "block %a: duplicate Op id v%d" pb bl (id :> int);
            S.Instruction_id.Tbl.replace defined_ops id bl
          | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ -> ()
          | Block_param _ | Proj _ | Tuple _ ->
            error
              "block %a: virtual instruction (Block_param/Proj/Tuple) cannot \
               appear in a block body"
              pb bl)
        bl.body;
      (match bl.terminator with
      | Goto { args; _ } -> check_args bl args
      | Branch { cond; _ } -> check_arg bl cond
      | Switch { index; _ } -> check_arg bl index
      | Return { args } -> check_args bl args
      | Raise { args; _ } -> check_args bl args
      | Tailcall_self { args; _ } -> check_args bl args
      | Tailcall_func { args; _ } -> check_args bl args
      | Call { args; _ } -> check_args bl args
      | Invalid { args; _ } -> check_args bl args);
      check_trap_stacks bl
    in
    List.iter visit_block S.blocks
end

let validate (m : (module Ssa.Finished_graph)) =
  let module S = (val m : Ssa.Finished_graph) in
  let module V = Make (S) in
  try V.validate ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "*** SSA validation failed for %s: %s@.*** SSA:@.%a@."
      S.function_info.fun_name (Printexc.to_string exn) Ssa_print.print m;
    Format.pp_print_flush Format.err_formatter ();
    Printexc.raise_with_backtrace exn bt
