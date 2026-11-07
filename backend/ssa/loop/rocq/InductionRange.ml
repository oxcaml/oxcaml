open AffineSsa
open FourierMotzkin
open InductionVar
open IntArith
open NaturalLoop
open OxCamlList
open SsaDominance
open SsaGraph
open SsaModel

type range_certificate =
  { biv : biv;
    lower : int;
    upper : Ssa.finished Ssa.Value.t option
  }

(** val literal_argument : block -> int -> block -> int option **)

let literal_argument header0 index pred =
  match goto_args header0 pred with
  | Some args -> (
    match arg_value args index with
    | Some argument ->
      (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
        match v with
        | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
          f_res id op (Array.to_list args) i
        | Ssa.Instruction.Block_param (b, i) -> f_param b i)
        (fun _ op _ _ ->
          match op with
          | Operation.Const_int n ->
            let n0 = Int64.of_nativeint n in
            if int64_is_int n0 then Some (Int64.to_int n0) else None
          | _ -> None)
        (fun _ _ -> None)
        argument
    | None -> None)
  | None -> None

(** val entry_predecessors : natural_loop -> block list **)

let entry_predecessors loop0 =
  let sources = back_edge_set loop0 in
  List.filter
    (fun pred -> not (Ssa.Block.Set.mem pred sources))
    (Ssa.Block.predecessors loop0.header)

(** val least_literal_argument : natural_loop -> int -> int option **)

let least_literal_argument loop0 index =
  match
    map_opt (literal_argument loop0.header index) (entry_predecessors loop0)
  with
  | Some literals -> (
    match literals with
    | [] -> None
    | first :: rest ->
      Some
        (List.fold_left
           (fun least k -> if k < least then k else least)
           first rest))
  | None -> None

(** val argument_at_most : Ssa.finished Ssa.Value.t -> block -> int -> block ->
    bool **)

let argument_at_most u header0 index pred =
  match goto_args header0 pred with
  | Some args -> (
    match arg_value args index with
    | Some argument -> (
      let p, c = linearize new_ctx [] u in
      let bound, side = p in
      let p0, c0 = linearize c side argument in
      let passed, side0 = p0 in
      let p1, _ = guards_at c0 side0 pred in
      let guards, side1 = p1 in
      match Affine.sub_checked bound passed with
      | Some goal -> entails (guards @ side1) goal
      | None -> false)
    | None -> false)
  | None -> false

(** val compared_with : block list -> (Ssa.finished Ssa.Value.t -> bool) ->
    Ssa.finished Ssa.Value.t list **)

let compared_with blocks is_counter =
  List.concat_map
    (fun b ->
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
        (fun _ _ -> [])
        (fun index _ ->
          (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
            match v with
            | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
              f_res id op (Array.to_list args) i
            | Ssa.Instruction.Block_param (b, i) -> f_param b i)
            (fun _ op args _ ->
              match op with
              | Operation.Intop op0 -> (
                match op0 with
                | Operation.Icomp _ -> (
                  match args with
                  | [] -> []
                  | x :: l -> (
                    match l with
                    | [] -> []
                    | y :: l0 -> (
                      match l0 with
                      | [] ->
                        if is_counter x
                        then y :: []
                        else if is_counter y
                        then x :: []
                        else []
                      | _ :: _ -> [])))
                | _ -> [])
              | _ -> [])
            (fun _ _ -> [])
            index)
        (fun _ -> [])
        (fun _ -> [])
        (Ssa.Block.terminator b))
    blocks

(** val upper_candidates : block list -> block -> int -> Ssa.finished
    Ssa.Value.t list -> Ssa.finished Ssa.Value.t list **)

let upper_candidates blocks header0 index updates =
  compared_with blocks (fun v -> List.exists (Ssa.Value.equal v) updates)
  @ compared_with blocks (fun v ->
      Ssa.Value.equal v (Ssa.Block.param header0 index))

(** val defined_above : op_def -> block -> Ssa.finished Ssa.Value.t -> bool **)

let defined_above op_def0 header0 v =
  (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
    match v with
    | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
      f_res id op (Array.to_list args) i
    | Ssa.Instruction.Block_param (b, i) -> f_param b i)
    (fun id _ _ _ ->
      match Ssa.Instruction.Id.Tbl.find_opt op_def0 id with
      | Some d -> strictly_dominates d header0
      | None -> false)
    (fun d _ -> strictly_dominates d header0)
    v

(** val verify_upper : op_def -> natural_loop -> int -> Ssa.finished Ssa.Value.t
    -> bool **)

let verify_upper op_def0 loop0 index u =
  defined_above op_def0 loop0.header u
  && List.for_all
       (argument_at_most u loop0.header index)
       (Ssa.Block.predecessors loop0.header)

(** val find_upper : op_def -> block list -> biv -> Ssa.finished Ssa.Value.t
    option **)

let find_upper op_def0 blocks biv0 =
  let loop0 = biv0.loop in
  List.find_opt
    (verify_upper op_def0 loop0 biv0.param_index)
    (upper_candidates blocks loop0.header biv0.param_index biv0.update)

(** val lower_facts : int -> Affine.t -> Affine.t list **)

let lower_facts lower0 counter =
  match Affine.sub_checked counter (Affine.constant lower0) with
  | Some fact -> fact :: []
  | None -> []

(** val bound_facts : ctx -> Affine.t list -> int -> Ssa.finished Ssa.Value.t
    option -> Affine.t -> (Affine.t list * Affine.t list) * ctx **)

let bound_facts c side lower0 upper0 counter =
  match upper0 with
  | Some u -> (
    let p, c0 = linearize c side u in
    let bound, side0 = p in
    match Affine.sub_checked bound counter with
    | Some fact -> (fact :: lower_facts lower0 counter, side0), c0
    | None -> (lower_facts lower0 counter, side0), c0)
  | None -> (lower_facts lower0 counter, side), c

(** val no_wrap_at : int -> int -> Ssa.finished Ssa.Value.t option -> block ->
    block -> int -> bool **)

let no_wrap_at step lower0 upper0 header0 source index =
  let p, c = linearize new_ctx [] (Ssa.Block.param header0 index) in
  let counter, side = p in
  let p0, c0 = bound_facts c side lower0 upper0 counter in
  let bounds, side0 = p0 in
  let p1, _ = guards_at c0 side0 source in
  let guards, side1 = p1 in
  let goal =
    match Affine.add_const_checked counter step with
    | Some lifted -> Affine.sub_checked (Affine.constant Int.max_int) lifted
    | None -> None
  in
  match goal with
  | Some goal0 -> entails (bounds @ guards @ side1) goal0
  | None -> false

(** val verify_certificate : block -> biv -> int -> Ssa.finished Ssa.Value.t
    option -> bool **)

let verify_certificate entry biv0 lower0 upper0 =
  let loop0 = biv0.loop in
  match signed_step biv0 with
  | Some step ->
    (0 < step && not (Ssa.Block.equal loop0.header entry))
    && List.for_all
         (fun source ->
           no_wrap_at step lower0 upper0 loop0.header source biv0.param_index)
         loop0.back_edge_sources
  | None -> false

(** val find_certificates : op_def -> block -> block list -> range_certificate
    list **)

let find_certificates op_def0 entry blocks =
  List.concat_map
    (fun pat ->
      let _, bivs = pat in
      List.filter_map
        (fun biv0 ->
          match least_literal_argument biv0.loop biv0.param_index with
          | Some lower0 ->
            let upper0 = find_upper op_def0 blocks biv0 in
            if verify_certificate entry biv0 lower0 upper0
            then Some { biv = biv0; lower = lower0; upper = upper0 }
            else None
          | None -> None)
        bivs)
    (analyze op_def0 blocks)

(** val certificate_facts : ctx -> range_certificate -> Affine.t list * ctx **)

let certificate_facts c cert =
  let header0 = cert.biv.loop.header in
  let id, c0 = intern c (Ssa.Block.param header0 cert.biv.param_index) in
  let p, c1 = bound_facts c0 [] cert.lower cert.upper (Affine.var id) in
  let facts, side = p in
  facts @ side, c1

(** val add_range_fact : block -> (Affine.t list * ctx) -> range_certificate ->
    Affine.t list * ctx **)

let add_range_fact target acc cert =
  let facts, c = acc in
  if Ssa.Block.Set.mem target cert.biv.loop.body
  then
    let new_facts, c0 = certificate_facts c cert in
    new_facts @ facts, c0
  else facts, c

(** val range_facts : range_certificate list -> block -> ctx -> Affine.t list *
    ctx **)

let range_facts certificates target c =
  List.fold_left (add_range_fact target) ([], c) certificates
