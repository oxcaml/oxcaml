open AffineSsa
open DeleteEmptyLoops
open FourierMotzkin
open InductionRange
open OxCamlList
open SsaModel

type limit =
  | Value_limit of Ssa.finished Ssa.Value.t
  | Const_limit of int

(** val unsigned_check : Ssa.finished Ssa.Value.t -> ((Cmm.integer_comparison *
    Ssa.finished Ssa.Value.t) * limit) option **)

let unsigned_check cond =
  (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
    match v with
    | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
      f_res id op (Array.to_list args) i
    | Ssa.Instruction.Block_param (b, i) -> f_param b i)
    (fun _ op args _ ->
      match op with
      | Operation.Intop op0 -> (
        match op0 with
        | Operation.Icomp cmp -> (
          match args with
          | [] -> None
          | a :: l -> (
            match l with
            | [] -> None
            | b :: l0 -> (
              match l0 with
              | [] -> (
                match cmp with
                | Cmm.Cult -> Some ((cmp, a), Value_limit b)
                | Cmm.Cugt ->
                  Some ((Cmm.swap_integer_comparison cmp, b), Value_limit a)
                | Cmm.Cule -> Some ((cmp, a), Value_limit b)
                | Cmm.Cuge ->
                  Some ((Cmm.swap_integer_comparison cmp, b), Value_limit a)
                | _ -> None)
              | _ :: _ -> None)))
        | _ -> None)
      | Operation.Intop_imm (op0, k) -> (
        match op0 with
        | Operation.Icomp cmp -> (
          match args with
          | [] -> None
          | a :: l -> (
            match l with
            | [] -> (
              match cmp with
              | Cmm.Cult -> Some ((cmp, a), Const_limit k)
              | Cmm.Cule -> Some ((cmp, a), Const_limit k)
              | _ -> None)
            | _ :: _ -> None))
        | _ -> None)
      | _ -> None)
    (fun _ _ -> None)
    cond

(** val check_goals : Cmm.integer_comparison -> Affine.t -> Affine.t -> Affine.t
    list option **)

let check_goals cmp index limit0 =
  match Affine.sub_checked limit0 index with
  | Some room -> (
    match cmp with
    | Cmm.Cult -> (
      match Affine.add_const_checked room (-1) with
      | Some room0 -> Some [index; room0]
      | None -> None)
    | Cmm.Cule -> Some [index; room]
    | _ -> None)
  | None -> None

(** val limit_form : ctx -> Affine.t list -> limit -> (Affine.t * Affine.t list)
    * ctx **)

let limit_form c side = function
  | Value_limit v -> linearize c side v
  | Const_limit k -> (Affine.constant k, side), c

(** val check_passes : range_certificate list -> block -> Ssa.finished
    Ssa.Value.t -> bool **)

let check_passes certificates b cond =
  match unsigned_check cond with
  | Some p -> (
    let p0, limit0 = p in
    let cmp, index = p0 in
    let index_form, c = linearize_goal new_ctx index in
    let p1, c0 = limit_form c [] limit0 in
    let limit_form0, side = p1 in
    let p2, c1 = guards_at c0 side b in
    let guards, side0 = p2 in
    let ranges, _ = range_facts certificates b c1 in
    match check_goals cmp index_form limit_form0 with
    | Some goals -> List.for_all (entails (ranges @ guards @ side0)) goals
    | None -> false)
  | None -> false

(** val eliminate_check : range_certificate list -> block -> (block * block)
    option **)

let eliminate_check certificates b =
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
    (fun _ _ -> None)
    (fun cond targets ->
      match targets with
      | [] -> None
      | _ :: l -> (
        match l with
        | [] -> None
        | ifso :: l0 -> (
          match l0 with
          | [] ->
            if check_passes certificates b cond then Some (b, ifso) else None
          | _ :: _ -> None)))
    (fun _ -> None)
    (fun _ -> None)
    (Ssa.Block.terminator b)

(** val find_eliminations : op_def -> block -> block list -> (block * block)
    list **)

let find_eliminations op_def0 entry blocks =
  if graph_supported blocks
  then
    let certificates = find_certificates op_def0 entry blocks in
    let certificates0 =
      if length_at_most 4_294_967_296 certificates then certificates else []
    in
    List.filter_map (eliminate_check certificates0) blocks
  else []
