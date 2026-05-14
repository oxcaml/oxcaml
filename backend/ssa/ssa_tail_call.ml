open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa_reducer

(** Detects [Call] terminators whose continuation does nothing but [Return] the
    call's results, and whose enclosing block has an empty trap stack. Rewrites
    them as [Tailcall_self] (when the callee is the current function by name) or
    [Tailcall_func]. The matching predicates are equivalent to the checks in
    [Cfg_selectgen.emit_tail_apply]. *)
module Tail_call_reducer (C : Context) = struct
  include Default (C)

  let returns_args_unchanged (block : C.In.Block.t) : bool =
    Array.for_all
      (fun (instr : C.In.Instruction.t) ->
        match[@warning "-fragile-match"] instr with
        | Name_for_debugger _ -> true
        | _ -> false)
      block.body
    &&
    match[@warning "-fragile-match"] block.terminator with
    | Return { args } ->
      Array.length args = Array.length block.params
      &&
      let n = Array.length args in
      let rec loop i =
        i >= n
        ||
        match[@warning "-fragile-match"] args.(i) with
        | Block_param { block = param_block; param_index } ->
          C.In.Block.equal param_block block && param_index = i && loop (i + 1)
        | _ -> false
      in
      loop 0
    | _ -> false

  let stack_offsets_zero (call_op : Cfg_intf.S.func_call_operation)
      (args : C.In.Instruction.t array) (ret_ty : Cmm.machtype) : bool =
    let real_args =
      match call_op with
      | Indirect _ -> Array.sub args 1 (Array.length args - 1)
      | Direct _ -> args
    in
    let arg_types = Array.map C.In.Instruction.arg_type real_args in
    let _, stack_ofs_args = Proc.loc_arguments arg_types in
    let _, stack_ofs_res = Proc.loc_results_call ret_ty in
    stack_ofs_args = 0 && stack_ofs_res = 0

  let visit_terminator (block : C.In.Block.t) (c : C.Cursor.t) =
    match[@warning "-fragile-match"] block.terminator with
    | Call
        { op = Func call_op;
          args;
          continuation;
          may_raise = _;
          nontail = false
        }
      when List.is_empty block.block_end_trap_stack
           && returns_args_unchanged continuation
           && stack_offsets_zero call_op args
                (C.In.Block.params_machtype continuation) ->
      let mapped_args = Array.map C.map_arg args in
      let term : C.Terminator.t =
        match call_op with
        | Direct func when String.equal func.sym_name C.In.function_info.name ->
          Tailcall_self
            { destination = C.map_block C.In.entry; args = mapped_args }
        | Direct _ | Indirect _ ->
          Tailcall_func { op = call_op; args = mapped_args }
      in
      C.finish_block c ~dbg:block.terminator_dbg term;
      Replaced ()
    | _ -> Unchanged
end

let run ~keep_unused_ops ssa =
  run ~keep_unused_ops (module Tail_call_reducer : Reducer) ssa
