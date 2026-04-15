[@@@ocaml.warning "+a-40-41-42"]

let print_block_id ppf (b : Ssa.block) = Format.fprintf ppf "%d" b.id

let rec print_instruction ppf (i : Ssa.instruction) =
  match i with
  | Op { id; op; args; _ } ->
    Format.fprintf ppf "v%d = %a(%a)"
      (Ssa.InstructionId.hash id)
      Operation.dump op print_args args
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" print_block_id block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref src
  | Pushtrap { handler } ->
    Format.fprintf ppf "pushtrap %a" print_block_id handler
  | Poptrap { handler } ->
    Format.fprintf ppf "poptrap %a" print_block_id handler
  | Stack_check { max_frame_size_bytes } ->
    Format.fprintf ppf "stack_check %d" max_frame_size_bytes
  | Name_for_debugger { ident; _ } ->
    Format.fprintf ppf "name_for_debugger %a" Ident.print ident

and print_instr_ref ppf (i : Ssa.instruction) =
  match i with
  | Op { id; _ } -> Format.fprintf ppf "v%d" (Ssa.InstructionId.hash id)
  | Block_param { block; index; _ } ->
    Format.fprintf ppf "%a.%d" print_block_id block index
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref src
  | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ ->
    print_instruction ppf i

and print_args ppf args =
  Array.iteri
    (fun i arg ->
      if i > 0 then Format.fprintf ppf ", ";
      print_instr_ref ppf arg)
    args

let print_instr_array ppf arr =
  Array.iteri
    (fun i arg ->
      if i > 0 then Format.fprintf ppf ", ";
      print_instr_ref ppf arg)
    arr

let print_terminator ppf (t : Ssa.terminator) =
  match t with
  | Never -> Format.fprintf ppf "never"
  | Goto { goto; args } ->
    Format.fprintf ppf "goto %a(%a)" print_block_id goto print_instr_array args
  | Branch { conditions; else_goto } ->
    Array.iter
      (fun (cond, target) ->
        Format.fprintf ppf "if %a then goto %a; " print_instr_ref cond
          print_block_id target)
      conditions;
    Format.fprintf ppf "else goto %a" print_block_id else_goto
  | Switch (targets, arg) ->
    Format.fprintf ppf "switch(%a) [%a]" print_instr_array arg
      (Format.pp_print_array
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         print_block_id)
      targets
  | Return args -> Format.fprintf ppf "return(%a)" print_instr_array args
  | Raise (_, args, _) -> Format.fprintf ppf "raise(%a)" print_instr_array args
  | Tailcall_self { destination; args } ->
    Format.fprintf ppf "tailcall_self %a(%a)" print_block_id destination
      print_instr_array args
  | Tailcall_func (_, args) ->
    Format.fprintf ppf "tailcall_func(%a)" print_instr_array args
  | Call { op = Direct sym; args; continuation; exn_continuation } -> (
    Format.fprintf ppf "call %s(%a) -> %a" sym.sym_name print_instr_array args
      print_block_id continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" print_block_id l
    | None -> ())
  | Call { op = Indirect _; args; continuation; exn_continuation } -> (
    Format.fprintf ppf "call_indirect(%a) -> %a" print_instr_array args
      print_block_id continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" print_block_id l
    | None -> ())
  | Prim
      { op = External { func_symbol; _ }; args; continuation; exn_continuation }
    -> (
    Format.fprintf ppf "prim %s(%a) -> %a" func_symbol print_instr_array args
      print_block_id continuation;
    match exn_continuation with
    | Some l -> Format.fprintf ppf " exn %a" print_block_id l
    | None -> ())
  | Prim { op = Probe { name; _ }; args; continuation; _ } ->
    Format.fprintf ppf "probe %s(%a) -> %a" name print_instr_array args
      print_block_id continuation
  | Invalid { message = _; args; continuation } -> (
    Format.fprintf ppf "invalid(%a)" print_instr_array args;
    match continuation with
    | Some l -> Format.fprintf ppf " -> %a" print_block_id l
    | None -> ())

let print_preds ppf predecessors =
  Format.fprintf ppf "preds=[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
       print_block_id)
    predecessors

let print_block_desc ppf (desc : Ssa.block_desc) =
  match desc with
  | Merge { predecessors } ->
    Format.fprintf ppf "merge %a" print_preds predecessors
  | BranchTarget { predecessor } ->
    Format.fprintf ppf "branch_target pred=%a" print_block_id predecessor
  | FunctionStart -> Format.fprintf ppf "function_start"
  | CallContinuation { predecessor } ->
    Format.fprintf ppf "call_cont pred=%a" print_block_id predecessor
  | TrapHandler { predecessors } ->
    Format.fprintf ppf "trap_handler %a" print_preds predecessors

let print_block ppf (blk : Ssa.block) =
  Format.fprintf ppf "%a: %a(%a)@." print_block_id blk print_block_desc blk.desc
    Printcmm.machtype blk.params;
  Array.iter
    (fun bi -> Format.fprintf ppf "  %a@." print_instruction bi)
    blk.body;
  Format.fprintf ppf "  %a@." print_terminator blk.terminator

let print ppf (t : Ssa.t) =
  Format.fprintf ppf "ssa %s(%a)@." t.fun_name Printcmm.machtype t.fun_args;
  Format.fprintf ppf "  entry = %a@.@." print_block_id t.entry;
  List.iter (print_block ppf) t.blocks;
  Format.fprintf ppf "@."
