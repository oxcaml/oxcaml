open SsaGraph
open SsaModel

(** val supported_operation : Operation.t -> 'a1 list -> bool **)

let supported_operation op args =
  match op with
  | Operation.Const_int _ -> ( match args with [] -> true | _ :: _ -> false)
  | Operation.Intop op0 -> (
    match op0 with
    | Operation.Iadd -> (
      match args with
      | [] -> false
      | _ :: l -> (
        match l with
        | [] -> false
        | _ :: l0 -> ( match l0 with [] -> true | _ :: _ -> false)))
    | Operation.Isub -> (
      match args with
      | [] -> false
      | _ :: l -> (
        match l with
        | [] -> false
        | _ :: l0 -> ( match l0 with [] -> true | _ :: _ -> false)))
    | Operation.Icomp _ -> (
      match args with
      | [] -> false
      | _ :: l -> (
        match l with
        | [] -> false
        | _ :: l0 -> ( match l0 with [] -> true | _ :: _ -> false)))
    | _ -> false)
  | Operation.Intop_imm (op0, _) -> (
    match op0 with
    | Operation.Iadd -> (
      match args with
      | [] -> false
      | _ :: l -> ( match l with [] -> true | _ :: _ -> false))
    | Operation.Isub -> (
      match args with
      | [] -> false
      | _ :: l -> ( match l with [] -> true | _ :: _ -> false))
    | Operation.Icomp _ -> (
      match args with
      | [] -> false
      | _ :: l -> ( match l with [] -> true | _ :: _ -> false))
    | _ -> false)
  | _ -> false

(** val supported_instruction : Ssa.finished Ssa.Instruction.t -> bool **)

let supported_instruction insn =
  (fun f_op f_push_trap f_pop_trap (i : Ssa.finished Ssa.Instruction.t) ->
    match i with
    | Ssa.Instruction.Op { Ssa.Instruction.id; op; args; typ; _ } ->
      f_op id op (Array.to_list args) (Array.length typ)
    | Ssa.Instruction.Push_trap _ -> f_push_trap ()
    | Ssa.Instruction.Pop_trap _ -> f_pop_trap ())
    (fun _ op args results ->
      Int.equal results 1 && supported_operation op args)
    (fun _ -> false)
    (fun _ -> false)
    insn

(** val supported_block : block -> bool **)

let supported_block b =
  List.for_all supported_instruction (block_body b)
  && terminator_pure_control (Ssa.Block.terminator b)

(** val context_instruction_supported : Ssa.finished Ssa.Instruction.t -> bool **)

let context_instruction_supported insn =
  (fun f_op f_push_trap f_pop_trap (i : Ssa.finished Ssa.Instruction.t) ->
    match i with
    | Ssa.Instruction.Op { Ssa.Instruction.id; op; args; typ; _ } ->
      f_op id op (Array.to_list args) (Array.length typ)
    | Ssa.Instruction.Push_trap _ -> f_push_trap ()
    | Ssa.Instruction.Pop_trap _ -> f_pop_trap ())
    (fun _ op _ count ->
      match op with
      | Operation.Const_int _ -> Int.equal count 1
      | Operation.Intop op0 -> (
        match op0 with
        | Operation.Iadd -> Int.equal count 1
        | Operation.Isub -> Int.equal count 1
        | Operation.Icomp _ -> Int.equal count 1
        | _ -> true)
      | Operation.Intop_imm (op0, _) -> (
        match op0 with
        | Operation.Iadd -> Int.equal count 1
        | Operation.Isub -> Int.equal count 1
        | Operation.Ior -> Int.equal count 1
        | Operation.Ilsl -> Int.equal count 1
        | Operation.Ilsr -> Int.equal count 1
        | Operation.Iasr -> Int.equal count 1
        | Operation.Icomp _ -> Int.equal count 1
        | _ -> true)
      | _ -> true)
    (fun _ -> true)
    (fun _ -> true)
    insn
