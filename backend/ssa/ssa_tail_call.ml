[@@@ocaml.warning "+a-40-41-42"]

open Ssa_reducer

(* Detects [Call] terminators whose continuation does nothing but [Return] the
   call's results, and whose enclosing block has an empty trap stack. Rewrites
   them as [Tailcall_self] (when the callee is the current function by name) or
   [Tailcall_func]. The matching predicates are the SSA-level analogue of the
   eager checks that used to live in [Ssa_of_cmm.emit_tail_apply]. *)
module Detect (C : Context) = struct
  include Default (C)

  let returns_args_unchanged (k : C.In.Block.t) : bool =
    Array.for_all
      (fun (i : C.In.Instruction.t) ->
        match[@warning "-fragile-match"] i with
        | Name_for_debugger _ -> true
        | _ -> false)
      k.body
    &&
    match[@warning "-fragile-match"] k.terminator with
    | Return { args } ->
      Array.length args = Array.length k.params
      &&
      let n = Array.length args in
      let rec loop i =
        i >= n
        ||
        match[@warning "-fragile-match"] args.(i) with
        | Block_param { block; index } ->
          C.In.Block.equal block k && index = i && loop (i + 1)
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

  let visit_terminator (blk : C.In.Block.t) (c : C.cursor) =
    match[@warning "-fragile-match"] blk.terminator with
    | Call
        { op = Func call_op;
          args;
          continuation;
          may_raise = _;
          nontail = false
        }
      when List.is_empty blk.block_end_trap_stack
           && returns_args_unchanged continuation
           && stack_offsets_zero call_op args
                (C.In.params_machtype continuation) ->
      let mapped_args = Array.map C.map_arg args in
      let term : C.Terminator.t =
        match call_op with
        | Direct func
          when String.equal func.sym_name C.In.function_info.fun_name ->
          Tailcall_self
            { destination = C.map_block C.In.entry; args = mapped_args }
        | Direct _ | Indirect _ ->
          Tailcall_func { op = call_op; args = mapped_args }
      in
      C.finish_block c ~dbg:blk.terminator_dbg term;
      Replaced ()
    | _ -> Unchanged
end

let run ssa = run ~keep_unused_ops:true (module Detect : Reducer) ssa
