[@@@ocaml.warning "+a-40-41-42"]

let print_block_id ppf (b : Ssa.block) = Format.fprintf ppf "B%d" (b.id :> int)

let print_block_param ppf ((b : Ssa.block), index) =
  Format.fprintf ppf "%a.%d" print_block_id b index

let rec print_instruction ppf (i : Ssa.instruction) =
  match i with
  | Op { id; op; args; _ } ->
    let op_str = Format.asprintf "%a" Operation.dump op in
    (* When the op has arguments, wrap its name in parens if it contains spaces,
       so that [v2 = (intop + -2)(v3)] is unambiguous. When there are no
       arguments, we omit both the op parens and the empty argument list. *)
    if Array.length args = 0
    then Format.fprintf ppf "v%d = %s" (id :> int) op_str
    else
      let formatted_op =
        if String.contains op_str ' ' then "(" ^ op_str ^ ")" else op_str
      in
      Format.fprintf ppf "v%d = %s(%a)" (id :> int) formatted_op print_args args
  | Block_param { block; index; _ } -> print_block_param ppf (block, index)
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref src
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

and print_instr_ref ppf (i : Ssa.instruction) =
  match i with
  | Op { id; _ } -> Format.fprintf ppf "v%d" (id :> int)
  | Block_param { block; index; _ } -> print_block_param ppf (block, index)
  | Proj { index; src } ->
    Format.fprintf ppf "proj(%d, %a)" index print_instr_ref src
  | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _ ->
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
  | Pending_construction -> Format.fprintf ppf "pending_construction"
  | Goto { goto; args } ->
    Format.fprintf ppf "goto %a(%a)" print_block_id goto print_instr_array args
  | Branch { cond; ifso; ifnot } ->
    Format.fprintf ppf "if %a then goto %a else goto %a" print_instr_ref cond
      print_block_id ifso print_block_id ifnot
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

let print_typed_params ppf (blk : Ssa.block) =
  Array.iteri
    (fun i typ ->
      if i > 0 then Format.fprintf ppf ", ";
      Format.fprintf ppf "%a : %a" print_block_param (blk, i)
        Printcmm.machtype_component typ)
    blk.params

let print_block_header ppf (blk : Ssa.block) =
  let name =
    match blk.desc with
    | Function_start -> "FUNCTION_START"
    | Branch_target -> "BRANCH_TARGET"
    | Call_continuation -> "CALL_CONT"
    | Merge -> "MERGE"
    | Trap_handler -> "TRAP_HANDLER"
  in
  Format.fprintf ppf "%a: %s(%a)" print_block_id blk name print_typed_params blk;
  (match blk.label_hint with
  | None -> ()
  | Some l -> Format.fprintf ppf " [label=%a]" Label.format l);
  match Ssa.predecessors blk with
  | [] -> ()
  | preds ->
    Format.fprintf ppf " <- %a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         print_block_id)
      preds

let print_block ppf (blk : Ssa.block) =
  Format.fprintf ppf "%a@." print_block_header blk;
  Array.iter
    (fun bi -> Format.fprintf ppf "  %a@." print_instruction bi)
    blk.body;
  Format.fprintf ppf "  %a@.@." print_terminator blk.terminator

let print ppf (t : Ssa.t) =
  Format.fprintf ppf "ssa %s(%a)@." t.fun_name Printcmm.machtype t.fun_args;
  Format.fprintf ppf "  entry = %a@.@." print_block_id t.entry;
  List.iter (print_block ppf) t.blocks;
  Format.fprintf ppf "@."
