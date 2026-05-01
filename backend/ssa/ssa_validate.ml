[@@@ocaml.warning "+a-40-41-42"]

(** Post-finish SSA invariants. Most structural invariants are enforced by the
    graph builder. Here, we just that SSA definitions dominate their uses. *)

module Make (S : Ssa.Finished_graph) = struct
  module P = Ssa_print.Make (S)

  let validate () =
    let error fmt =
      Format.kasprintf
        (fun s ->
          Misc.fatal_errorf "SSA validation (%s): %s" S.function_info.fun_name s)
        fmt
    in
    let pb = P.print_block_id in
    let block_set = S.Block.Tbl.create 16 in
    List.iter (fun bl -> S.Block.Tbl.replace block_set bl ()) S.blocks;
    let block_exists b = S.Block.Tbl.mem block_set b in
    let defined_ops = S.Instruction_id.Tbl.create 64 in
    let rec check_arg (bl : S.Block.t) (i : S.Instruction.t) =
      match i with
      | Op { id; _ } -> (
        match S.Instruction_id.Tbl.find_opt defined_ops id with
        | None ->
          error "block %a: Op v%d used but not defined" pb bl
            (S.Instruction_id.hash id)
        | Some def_block ->
          if not (S.dominates def_block bl)
          then
            error "block %a: Op v%d defined in non-dominating block %a" pb bl
              (S.Instruction_id.hash id) pb def_block)
      | Block_param { block; _ } ->
        if not (block_exists block)
        then
          error "block %a: BlockParam references non-existent block %a" pb bl pb
            block;
        if not (S.dominates block bl)
        then
          error "block %a: BlockParam of non-dominating block %a" pb bl pb block
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
            then
              error "block %a: duplicate Op id v%d" pb bl
                (S.Instruction_id.hash id);
            S.Instruction_id.Tbl.replace defined_ops id bl
          | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ -> ()
          | Block_param _ | Proj _ | Tuple _ ->
            error
              "block %a: virtual instruction (Block_param/Proj/Tuple) cannot \
               appear in a block body"
              pb bl)
        bl.body;
      match bl.terminator with
      | Goto { args; _ } -> check_args bl args
      | Branch { cond; _ } -> check_arg bl cond
      | Switch { index; _ } -> check_arg bl index
      | Return { args } -> check_args bl args
      | Raise { args; _ } -> check_args bl args
      | Tailcall_self { args; _ } -> check_args bl args
      | Tailcall_func { args; _ } -> check_args bl args
      | Call { args; _ } -> check_args bl args
      | Invalid { args; _ } -> check_args bl args
    in
    List.iter visit_block S.blocks
end

let validate (m : (module Ssa.Finished_graph)) =
  let module S = (val m : Ssa.Finished_graph) in
  let module V = Make (S) in
  V.validate ()
