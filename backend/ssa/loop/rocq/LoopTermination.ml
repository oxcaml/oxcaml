open AffineSsa
open FourierMotzkin
open InductionVar
open LoopComparisons
open NaturalLoop
open SsaGraph
open SsaModel

type t =
  | Terminates
  | Unknown

type bound =
  | Value_bound of Ssa.finished Ssa.Value.t
  | Const_bound of int

(** val bound_in_range : int -> block -> bound -> bool **)

let bound_in_range step header0 = function
  | Value_bound v -> (
    let p, c = linearize new_ctx [] v in
    let form, side = p in
    let p0, _ = guards_at c side header0 in
    let guards, side0 = p0 in
    let facts = guards @ side0 in
    let goal =
      if 0 < step
      then
        match Affine.scale_checked (-1) form with
        | Some f -> Affine.add_const_checked f Int.max_int
        | None -> None
      else Affine.add_const_checked form Int.max_int
    in
    match goal with Some goal0 -> entails facts goal0 | None -> false)
  | Const_bound k ->
    if 0 < step then k <= Int.max_int else Int.neg Int.max_int <= k

(** val extract_comparison : (Ssa.finished Ssa.Value.t -> bool) -> (Ssa.finished
    Ssa.Value.t -> bool) -> Ssa.finished Ssa.Value.t -> ((Cmm.integer_comparison
    * bool) * bound) option **)

let extract_comparison is_counter_operand is_bound comparison_result =
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
          | x :: l -> (
            match l with
            | [] -> None
            | y :: l0 -> (
              match l0 with
              | [] ->
                if is_counter_operand x && is_bound y
                then Some ((cmp, true), Value_bound y)
                else if is_counter_operand y && is_bound x
                then Some ((cmp, false), Value_bound x)
                else None
              | _ :: _ -> None)))
        | _ -> None)
      | Operation.Intop_imm (op0, k) -> (
        match op0 with
        | Operation.Icomp cmp -> (
          match args with
          | [] -> None
          | x :: l -> (
            match l with
            | [] ->
              if is_counter_operand x
              then Some ((cmp, true), Const_bound k)
              else None
            | _ :: _ -> None))
        | _ -> None)
      | _ -> None)
    (fun _ _ -> None)
    comparison_result

(** val biv_implies_termination : op_def -> biv -> bool **)

let biv_implies_termination op_def0 biv0 =
  match find_exit_branch biv0.loop with
  | Some exit_info -> (
    match signed_step biv0 with
    | Some step -> (
      let header0 = biv0.loop.header in
      let is_self = is_param header0 biv0.param_index in
      let is_iv_val =
       fun v -> is_self v || List.exists (Ssa.Value.equal v) biv0.update
      in
      let is_bound = fun v -> is_loop_invariant op_def0 header0 v in
      match extract_comparison is_iv_val is_bound exit_info.condition with
      | Some p -> (
        let p0, bound0 = p in
        let cmp, iv_is_left = p0 in
        let continue_cmp =
          oriented_continue_comparison iv_is_left exit_info.continue_when_true
            cmp
        in
        match continue_terminates step continue_cmp with
        | LoopComparisons.Terminates -> true
        | Terminates_if_bound_in_range -> bound_in_range step header0 bound0
        | LoopComparisons.Unknown -> false)
      | None -> false)
    | None -> false)
  | None -> false

(** val analyze : op_def -> biv list -> t **)

let analyze op_def0 bivs =
  if List.exists (biv_implies_termination op_def0) bivs
  then Terminates
  else Unknown
