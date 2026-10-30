open OxCamlList
open SsaModel

(** val terminator_pure_control : Ssa.finished Ssa.Terminator.t -> bool **)

let terminator_pure_control t =
  (fun f_continue
    f_switch
    f_call
    f_invalid
    (t : Ssa.finished Ssa.Terminator.t)
  ->
    match t with
    | Ssa.Terminator.Continue { continuation; args } ->
      f_continue continuation (Array.to_list args)
    | Ssa.Terminator.Switch { index; targets } ->
      f_switch index (Array.to_list targets)
    | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
    | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))
    (fun continuation _ ->
      match continuation with Ssa.Goto _ -> true | _ -> false)
    (fun _ _ -> true)
    (fun _ -> false)
    (fun _ -> false)
    t

(** val is_param : block -> int -> Ssa.finished Ssa.Value.t -> bool **)

let is_param block0 index v = Ssa.Value.equal v (Ssa.Block.param block0 index)

(** val instr_defines_value : Ssa.finished Ssa.Instruction.t -> Ssa.finished
    Ssa.Value.t -> bool **)

let instr_defines_value instr v =
  (fun f_op f_push_trap f_pop_trap (i : Ssa.finished Ssa.Instruction.t) ->
    match i with
    | Ssa.Instruction.Op { Ssa.Instruction.id; op; args; typ; _ } ->
      f_op id op (Array.to_list args) (Array.length typ)
    | Ssa.Instruction.Push_trap _ -> f_push_trap ()
    | Ssa.Instruction.Pop_trap _ -> f_pop_trap ())
    (fun id _ _ _ ->
      (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
        match v with
        | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
          f_res id op (Array.to_list args) i
        | Ssa.Instruction.Block_param (b, i) -> f_param b i)
        (fun id' _ _ _ -> Ssa.Instruction.Id.equal id id')
        (fun _ _ -> false)
        v)
    (fun _ -> false)
    (fun _ -> false)
    instr

(** val args_of_instr : Ssa.finished Ssa.Instruction.t -> Ssa.finished
    Ssa.Value.t list **)

let args_of_instr instr =
  (fun f_op f_push_trap f_pop_trap (i : Ssa.finished Ssa.Instruction.t) ->
    match i with
    | Ssa.Instruction.Op { Ssa.Instruction.id; op; args; typ; _ } ->
      f_op id op (Array.to_list args) (Array.length typ)
    | Ssa.Instruction.Push_trap _ -> f_push_trap ()
    | Ssa.Instruction.Pop_trap _ -> f_pop_trap ())
    (fun _ _ args _ -> args)
    (fun _ -> [])
    (fun _ -> [])
    instr

(** val terminator_args : Ssa.finished Ssa.Terminator.t -> Ssa.finished
    Ssa.Value.t list list **)

let terminator_args term =
  (fun f_continue
    f_switch
    f_call
    f_invalid
    (t : Ssa.finished Ssa.Terminator.t)
  ->
    match t with
    | Ssa.Terminator.Continue { continuation; args } ->
      f_continue continuation (Array.to_list args)
    | Ssa.Terminator.Switch { index; targets } ->
      f_switch index (Array.to_list targets)
    | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
    | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))
    (fun _ args ->
      List.filter_map
        (fun arg ->
          match arg with
          | Ssa.Terminator.Arg v -> Some v
          | Ssa.Terminator.Omitted_since_unused -> None)
        args
      :: [])
    (fun index _ -> (index :: []) :: [])
    (fun args -> args :: [])
    (fun args -> args :: [])
    term

(** val arg_is : Ssa.finished Ssa.Value.t -> Ssa.finished Ssa.Terminator.arg ->
    bool **)

let arg_is v = function
  | Ssa.Terminator.Arg w -> Ssa.Value.equal w v
  | Ssa.Terminator.Omitted_since_unused -> false

(** val arg_value : Ssa.finished Ssa.Terminator.arg list -> int -> Ssa.finished
    Ssa.Value.t option **)

let arg_value args index =
  match nth_int args index with
  | Some a -> (
    match a with
    | Ssa.Terminator.Arg v -> Some v
    | Ssa.Terminator.Omitted_since_unused -> None)
  | None -> None

(** val goto_args : block -> block -> Ssa.finished Ssa.Terminator.arg list
    option **)

let goto_args target pred =
  (fun f_continue
    f_switch
    f_call
    f_invalid
    (t : Ssa.finished Ssa.Terminator.t)
  ->
    match t with
    | Ssa.Terminator.Continue { continuation; args } ->
      f_continue continuation (Array.to_list args)
    | Ssa.Terminator.Switch { index; targets } ->
      f_switch index (Array.to_list targets)
    | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
    | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))
    (fun continuation args ->
      match continuation with
      | Ssa.Goto goto ->
        if Ssa.Block.equal goto target && same_length args (params target)
        then Some args
        else None
      | _ -> None)
    (fun _ _ -> None)
    (fun _ -> None)
    (fun _ -> None)
    (Ssa.Block.terminator pred)

(** val goto_passes_only_at : block -> int -> Ssa.finished Ssa.Value.t -> block
    -> bool **)

let goto_passes_only_at target argument_index argument_value block0 =
  (fun f_continue
    f_switch
    f_call
    f_invalid
    (t : Ssa.finished Ssa.Terminator.t)
  ->
    match t with
    | Ssa.Terminator.Continue { continuation; args } ->
      f_continue continuation (Array.to_list args)
    | Ssa.Terminator.Switch { index; targets } ->
      f_switch index (Array.to_list targets)
    | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
    | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))
    (fun continuation args ->
      match continuation with
      | Ssa.Goto goto ->
        (Ssa.Block.equal goto target
        &&
        match arg_value args argument_index with
        | Some w -> Ssa.Value.equal w argument_value
        | None -> false)
        && Int.equal (count_occurrences (arg_is argument_value) args) 1
      | _ -> false)
    (fun _ _ -> false)
    (fun _ -> false)
    (fun _ -> false)
    (Ssa.Block.terminator block0)

(** val all_uses_ok : block list -> (Ssa.finished Ssa.Value.t -> bool) ->
    (Ssa.finished Ssa.Instruction.t -> bool) -> (block -> Ssa.finished
    Ssa.Value.t list -> bool) -> bool **)

let all_uses_ok blocks same approve_instr approve_term =
  List.for_all
    (fun block0 ->
      List.for_all
        (fun instr ->
          (not (List.exists same (args_of_instr instr))) || approve_instr instr)
        (block_body block0)
      && List.for_all
           (fun operands ->
             (not (List.exists same operands)) || approve_term block0 operands)
           (terminator_args (Ssa.Block.terminator block0)))
    blocks

(** val visit_all : (blockset -> block -> blockset option) -> blockset -> block
    list -> blockset option **)

let rec visit_all visit seen = function
  | [] -> Some seen
  | block0 :: roots0 -> (
    match visit seen block0 with
    | Some seen0 -> visit_all visit seen0 roots0
    | None -> None)

(** val visit_acyclic : block list -> blockset -> block -> blockset -> blockset
    -> block -> blockset option **)

let rec visit_acyclic fuel region cut active seen block0 =
  if
    (Ssa.Block.equal block0 cut || not (Ssa.Block.Set.mem block0 region))
    || Ssa.Block.Set.mem block0 seen
  then Some seen
  else if Ssa.Block.Set.mem block0 active
  then None
  else
    match fuel with
    | [] -> None
    | _ :: fuel0 -> (
      match
        visit_all
          (visit_acyclic fuel0 region cut (Ssa.Block.Set.add block0 active))
          seen
          (Ssa.Block.all_successors block0)
      with
      | Some seen0 -> Some (Ssa.Block.Set.add block0 seen0)
      | None -> None)

(** val acyclic_except : block list -> blockset -> block -> bool **)

let acyclic_except blocks region cut =
  match
    visit_all
      (visit_acyclic blocks region cut Ssa.Block.Set.empty)
      Ssa.Block.Set.empty blocks
  with
  | Some seen ->
    Ssa.Block.Set.for_all
      (fun block0 ->
        Ssa.Block.equal block0 cut || Ssa.Block.Set.mem block0 seen)
      region
  | None -> false
