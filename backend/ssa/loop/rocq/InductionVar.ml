open NaturalLoop
open SsaDominance
open SsaGraph
open SsaModel

(** val is_loop_invariant : op_def -> block -> Ssa.finished Ssa.Value.t -> bool
    **)

let is_loop_invariant op_def0 header0 v =
  (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
    match v with
    | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
      f_res id op (Array.to_list args) i
    | Ssa.Instruction.Block_param (b, i) -> f_param b i)
    (fun id op _ _ ->
      match op with
      | Operation.Const_int _ -> true
      | Operation.Const_float32 _ -> true
      | Operation.Const_float _ -> true
      | Operation.Const_symbol _ -> true
      | Operation.Const_vec128 _ -> true
      | Operation.Const_vec256 _ -> true
      | Operation.Const_vec512 _ -> true
      | Operation.Const_mask _ -> true
      | _ -> (
        match Ssa.Instruction.Id.Tbl.find_opt op_def0 id with
        | Some block0 -> strictly_dominates block0 header0
        | None -> false))
    (fun block0 _ -> strictly_dominates block0 header0)
    v

type biv_step =
  | Step_const of int
  | Step_var of Ssa.finished Ssa.Value.t

type biv =
  { loop : natural_loop;
    param_index : int;
    update : Ssa.finished Ssa.Value.t list;
    step : biv_step;
    sign : [`Add | `Sub]
  }

(** val step_same : biv_step -> biv_step -> bool **)

let step_same a b =
  match a with
  | Step_const x -> (
    match b with Step_const y -> Int.equal x y | Step_var _ -> false)
  | Step_var x -> (
    match b with Step_const _ -> false | Step_var y -> Ssa.Value.equal x y)

(** val sign_same : [ `Add | `Sub ] -> [ `Add | `Sub ] -> bool **)

let sign_same a b =
  match a with
  | `Add -> ( match b with `Add -> true | `Sub -> false)
  | `Sub -> ( match b with `Add -> false | `Sub -> true)

(** val signed_step : biv -> int option **)

let signed_step biv0 =
  match biv0.step with
  | Step_const c -> (
    match biv0.sign with
    | `Add -> Some c
    | `Sub -> if Int.equal c Int.min_int then None else Some (Int.neg c))
  | Step_var _ -> None

(** val classify_update : (Ssa.finished Ssa.Value.t -> bool) -> op_def -> block
    -> Ssa.finished Ssa.Value.t -> (biv_step * [ `Add | `Sub ]) option **)

let classify_update is_self op_def0 header0 v =
  (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
    match v with
    | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
      f_res id op (Array.to_list args) i
    | Ssa.Instruction.Block_param (b, i) -> f_param b i)
    (fun _ op args _ ->
      match op with
      | Operation.Intop op0 -> (
        match op0 with
        | Operation.Iadd -> (
          match args with
          | [] -> None
          | x :: l -> (
            match l with
            | [] -> None
            | y :: l0 -> (
              match l0 with
              | [] ->
                if is_self x && is_loop_invariant op_def0 header0 y
                then Some (Step_var y, `Add)
                else if is_self y && is_loop_invariant op_def0 header0 x
                then Some (Step_var x, `Add)
                else None
              | _ :: _ -> None)))
        | Operation.Isub -> (
          match args with
          | [] -> None
          | x :: l -> (
            match l with
            | [] -> None
            | y :: l0 -> (
              match l0 with
              | [] ->
                if is_self x && is_loop_invariant op_def0 header0 y
                then Some (Step_var y, `Sub)
                else None
              | _ :: _ -> None)))
        | _ -> None)
      | Operation.Intop_imm (op0, c) -> (
        match op0 with
        | Operation.Iadd -> (
          match args with
          | [] -> None
          | x :: l -> (
            match l with
            | [] -> if is_self x then Some (Step_const c, `Add) else None
            | _ :: _ -> None))
        | Operation.Isub -> (
          match args with
          | [] -> None
          | x :: l -> (
            match l with
            | [] -> if is_self x then Some (Step_const c, `Sub) else None
            | _ :: _ -> None))
        | _ -> None)
      | _ -> None)
    (fun _ _ -> None)
    v

(** val back_edge_set : natural_loop -> blockset **)

let back_edge_set loop0 =
  List.fold_left
    (fun set block0 -> Ssa.Block.Set.add block0 set)
    Ssa.Block.Set.empty loop0.back_edge_sources

(** val classify_param : natural_loop -> op_def -> (block * Ssa.finished
    Ssa.Terminator.arg list) list -> int -> biv option **)

let classify_param loop0 op_def0 source_args index =
  let is_self = is_param loop0.header index in
  let back_vals_opt =
    List.map
      (fun pat ->
        let _, args = pat in
        arg_value args index)
      source_args
  in
  if List.for_all Option.is_some back_vals_opt
  then
    let back_vals = List.filter_map (fun v -> v) back_vals_opt in
    match back_vals with
    | [] -> None
    | first :: rest -> (
      match classify_update is_self op_def0 loop0.header first with
      | Some p ->
        let step0, sign0 = p in
        let agrees =
          List.for_all
            (fun v ->
              match classify_update is_self op_def0 loop0.header v with
              | Some p0 ->
                let step', sign' = p0 in
                step_same step0 step' && sign_same sign0 sign'
              | None -> false)
            rest
        in
        if agrees
        then
          Some
            { loop = loop0;
              param_index = index;
              update = back_vals;
              step = step0;
              sign = sign0
            }
        else None
      | None -> None)
  else None

(** val params_bivs : natural_loop -> op_def -> (block * Ssa.finished
    Ssa.Terminator.arg list) list -> int -> Ssa.finished Ssa.Value.t list -> biv
    list **)

let rec params_bivs loop0 op_def0 source_args index = function
  | [] -> []
  | _ :: params1 -> (
    let rest = params_bivs loop0 op_def0 source_args (index + 1) params1 in
    match classify_param loop0 op_def0 source_args index with
    | Some biv0 -> biv0 :: rest
    | None -> rest)

(** val analyze_loop : op_def -> natural_loop -> biv list **)

let analyze_loop op_def0 loop0 =
  let header0 = loop0.header in
  let source_set = back_edge_set loop0 in
  let pred_args =
    List.map
      (fun pred -> pred, goto_args header0 pred)
      (Ssa.Block.predecessors header0)
  in
  let all_back_edges_have_args =
    List.for_all
      (fun pat ->
        let pred, args = pat in
        (not (Ssa.Block.Set.mem pred source_set)) || Option.is_some args)
      pred_args
  in
  if not all_back_edges_have_args
  then []
  else
    let source_args =
      List.filter_map
        (fun pat ->
          let pred, args = pat in
          if Ssa.Block.Set.mem pred source_set
          then Option.map (fun args0 -> pred, args0) args
          else None)
        pred_args
    in
    params_bivs loop0 op_def0 source_args 0 (params header0)

(** val analyze : op_def -> block list -> (natural_loop * biv list) list **)

let analyze op_def0 blocks =
  List.map (fun loop0 -> loop0, analyze_loop op_def0 loop0) (find_loops blocks)
