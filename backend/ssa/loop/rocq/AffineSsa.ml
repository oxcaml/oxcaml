open AffineExpr
open FourierMotzkin
open IntArith
open LoopComparisons
open SsaDominance
open SsaModel

type ctx =
  { atoms : (int * Ssa.finished Ssa.Value.t) list;
    next : int;
    budget : int
  }

(** val new_ctx : ctx **)

let new_ctx = { atoms = []; next = 0; budget = 256 }

(** val refill_budget : ctx -> ctx **)

let refill_budget c = { atoms = c.atoms; next = c.next; budget = 256 }

(** val spend_budget : ctx -> int -> ctx **)

let spend_budget c k = { atoms = c.atoms; next = c.next; budget = c.budget - k }

(** val intern : ctx -> Ssa.finished Ssa.Value.t -> int * ctx **)

let intern c v =
  match
    List.find_opt
      (fun pat ->
        let _, w = pat in
        Ssa.Value.equal v w)
      c.atoms
  with
  | Some p ->
    let id, _ = p in
    id, c
  | None ->
    let id = c.next in
    id, { atoms = (id, v) :: c.atoms; next = id + 1; budget = c.budget }

(** val atomize : ctx -> Ssa.finished Ssa.Value.t -> (t * Range.t option) * ctx
    **)

let atomize c v =
  let id, c0 = intern c v in
  (Atom id, Some Range.full), c0

(** val keep_if_range_known : ctx -> Ssa.finished Ssa.Value.t -> t -> Range.t
    option -> (t * Range.t option) * ctx **)

let keep_if_range_known c v node range =
  match range with Some _ -> (node, range), c | None -> atomize c v

(** val add_optional_ranges : Range.t option -> Range.t option -> Range.t option
    **)

let add_optional_ranges a b =
  match a with
  | Some a0 -> ( match b with Some b0 -> Range.add a0 b0 | None -> None)
  | None -> None

(** val sub_optional_ranges : Range.t option -> Range.t option -> Range.t option
    **)

let sub_optional_ranges a b =
  match a with
  | Some a0 -> ( match b with Some b0 -> Range.sub a0 b0 | None -> None)
  | None -> None

(** val scale_optional_range : int -> Range.t option -> Range.t option **)

let scale_optional_range k = function
  | Some r0 -> Range.scale k r0
  | None -> None

(** val expr_of_value : ctx -> Ssa.finished Ssa.Value.t -> (t * Range.t option)
    * ctx **)

let rec expr_of_value c v =
  (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
    match v with
    | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
      f_res id op (Array.to_list args) i
    | Ssa.Instruction.Block_param (b, i) -> f_param b i)
    (fun _ op args _ ->
      match op with
      | Operation.Const_int n ->
        let n0 = Int64.of_nativeint n in
        if int64_is_int n0
        then
          let k = Int64.to_int n0 in
          (Const k, Some (Range.const k)), c
        else atomize c v
      | Operation.Intop op0 -> (
        match op0 with
        | Operation.Iadd -> (
          match args with
          | [] -> atomize c v
          | a :: l -> (
            match l with
            | [] -> atomize c v
            | b :: l0 -> (
              match l0 with
              | [] ->
                if 2 <= c.budget
                then
                  let c0 = spend_budget c 2 in
                  let p, c1 = expr_of_value c0 a in
                  let ea, ra = p in
                  let p0, c2 = expr_of_value c1 b in
                  let eb, rb = p0 in
                  keep_if_range_known c2 v
                    (Add (ea, eb))
                    (add_optional_ranges ra rb)
                else atomize c v
              | _ :: _ -> atomize c v)))
        | Operation.Isub -> (
          match args with
          | [] -> atomize c v
          | a :: l -> (
            match l with
            | [] -> atomize c v
            | b :: l0 -> (
              match l0 with
              | [] ->
                if 2 <= c.budget
                then
                  let c0 = spend_budget c 2 in
                  let p, c1 = expr_of_value c0 a in
                  let ea, ra = p in
                  let p0, c2 = expr_of_value c1 b in
                  let eb, rb = p0 in
                  keep_if_range_known c2 v
                    (Sub (ea, eb))
                    (sub_optional_ranges ra rb)
                else atomize c v
              | _ :: _ -> atomize c v)))
        | _ -> atomize c v)
      | Operation.Intop_imm (op0, m) -> (
        match op0 with
        | Operation.Iadd -> (
          match args with
          | [] -> atomize c v
          | a :: l -> (
            match l with
            | [] ->
              if 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let p, c1 = expr_of_value c0 a in
                let ea, ra = p in
                keep_if_range_known c1 v
                  (Add (ea, Const m))
                  (add_optional_ranges ra (Some (Range.const m)))
              else atomize c v
            | _ :: _ -> atomize c v))
        | Operation.Isub -> (
          match args with
          | [] -> atomize c v
          | a :: l -> (
            match l with
            | [] ->
              if 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let p, c1 = expr_of_value c0 a in
                let ea, ra = p in
                keep_if_range_known c1 v
                  (Sub (ea, Const m))
                  (sub_optional_ranges ra (Some (Range.const m)))
              else atomize c v
            | _ :: _ -> atomize c v))
        | Operation.Ior -> (
          match args with
          | [] -> atomize c v
          | a :: l -> (
            match l with
            | [] ->
              if 0 <= m && 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let p, c1 = expr_of_value c0 a in
                let ea, o = p in
                match o with
                | Some r ->
                  let id, c2 = intern c1 v in
                  (Or_atom (id, ea, m), Range.or_mask m r), c2
                | None -> atomize c1 v
              else atomize c v
            | _ :: _ -> atomize c v))
        | Operation.Ilsl -> (
          match args with
          | [] -> atomize c v
          | a :: l -> (
            match l with
            | [] ->
              if (0 <= m && m < 16) && 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let p, c1 = expr_of_value c0 a in
                let ea, ra = p in
                keep_if_range_known c1 v
                  (Scale (1 lsl m, ea))
                  (scale_optional_range (1 lsl m) ra)
              else atomize c v
            | _ :: _ -> atomize c v))
        | Operation.Ilsr -> (
          match args with
          | [] -> atomize c v
          | _ :: l -> (
            match l with
            | [] ->
              if 1 <= m && m < 64
              then
                let id, c0 = intern c v in
                (Lsr_atom (id, m), Some (Range.shr_logical m)), c0
              else atomize c v
            | _ :: _ -> atomize c v))
        | Operation.Iasr -> (
          match args with
          | [] -> atomize c v
          | a :: l -> (
            match l with
            | [] ->
              if (0 <= m && m < 16) && 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let p, c1 = expr_of_value c0 a in
                let ea, o = p in
                match o with
                | Some _ ->
                  let id, c2 = intern c1 v in
                  (Shr_atom (id, ea, m), Some (Range.shr_signed m)), c2
                | None -> atomize c1 v
              else atomize c v
            | _ :: _ -> atomize c v))
        | _ -> atomize c v)
      | Operation.Specific s -> (
        match args with
        | [] -> atomize c v
        | a :: l -> (
          match l with
          | [] -> (
            match Arch.linear_specific_operation s with
            | Some p -> (
              let l0, k = p in
              match l0 with
              | [] -> atomize c v
              | ma :: l1 -> (
                match l1 with
                | [] ->
                  if 1 <= c.budget
                  then
                    let c0 = spend_budget c 1 in
                    let p0, c1 = expr_of_value c0 a in
                    let ea, ra = p0 in
                    keep_if_range_known c1 v
                      (Add (Scale (ma, ea), Const k))
                      (add_optional_ranges
                         (scale_optional_range ma ra)
                         (Some (Range.const k)))
                  else atomize c v
                | _ :: _ -> atomize c v))
            | None -> atomize c v)
          | b :: l0 -> (
            match l0 with
            | [] -> (
              match Arch.linear_specific_operation s with
              | Some p -> (
                let l1, k = p in
                match l1 with
                | [] -> atomize c v
                | ma :: l2 -> (
                  match l2 with
                  | [] -> atomize c v
                  | mb :: l3 -> (
                    match l3 with
                    | [] ->
                      if 2 <= c.budget
                      then
                        let c0 = spend_budget c 2 in
                        let p0, c1 = expr_of_value c0 a in
                        let ea, ra = p0 in
                        let p1, c2 = expr_of_value c1 b in
                        let eb, rb = p1 in
                        keep_if_range_known c2 v
                          (Add (Add (Scale (ma, ea), Scale (mb, eb)), Const k))
                          (add_optional_ranges
                             (add_optional_ranges
                                (scale_optional_range ma ra)
                                (scale_optional_range mb rb))
                             (Some (Range.const k)))
                      else atomize c v
                    | _ :: _ -> atomize c v)))
              | None -> atomize c v)
            | _ :: _ -> atomize c v)))
      | _ -> atomize c v)
    (fun _ _ -> atomize c v)
    v

(** val push_sides : Affine.t list -> Affine.t list -> Affine.t list **)

let push_sides existing_facts new_facts =
  List.fold_left
    (fun existing_facts0 fact ->
      if List.exists (Affine.equal fact) existing_facts0
      then existing_facts0
      else fact :: existing_facts0)
    existing_facts new_facts

(** val linearize : ctx -> Affine.t list -> Ssa.finished Ssa.Value.t ->
    (Affine.t * Affine.t list) * ctx **)

let linearize c side v =
  let p, c0 = expr_of_value (refill_budget c) v in
  let e, _ = p in
  match to_affine e with
  | Some p0 ->
    let form, sides = p0 in
    (form, push_sides side sides), c0
  | None ->
    let id, c1 = intern c0 v in
    (Affine.var id, side), c1

(** val goal_atom : ctx -> Ssa.finished Ssa.Value.t -> t * ctx **)

let goal_atom c v =
  let id, c0 = intern c v in
  Atom id, c0

(** val expr_of_value_goal : ctx -> Ssa.finished Ssa.Value.t -> t * ctx **)

let rec expr_of_value_goal c v =
  (fun f_res f_param (v : Ssa.finished Ssa.Value.t) ->
    match v with
    | Ssa.Instruction.Res ({ Ssa.Instruction.id; op; args; _ }, i) ->
      f_res id op (Array.to_list args) i
    | Ssa.Instruction.Block_param (b, i) -> f_param b i)
    (fun _ op args _ ->
      match op with
      | Operation.Const_int n ->
        let n0 = Int64.of_nativeint n in
        if int64_is_int n0 then Const (Int64.to_int n0), c else goal_atom c v
      | Operation.Intop op0 -> (
        match op0 with
        | Operation.Iadd -> (
          match args with
          | [] -> goal_atom c v
          | a :: l -> (
            match l with
            | [] -> goal_atom c v
            | b :: l0 -> (
              match l0 with
              | [] ->
                if 2 <= c.budget
                then
                  let c0 = spend_budget c 2 in
                  let ea, c1 = expr_of_value_goal c0 a in
                  let eb, c2 = expr_of_value_goal c1 b in
                  Add (ea, eb), c2
                else goal_atom c v
              | _ :: _ -> goal_atom c v)))
        | Operation.Isub -> (
          match args with
          | [] -> goal_atom c v
          | a :: l -> (
            match l with
            | [] -> goal_atom c v
            | b :: l0 -> (
              match l0 with
              | [] ->
                if 2 <= c.budget
                then
                  let c0 = spend_budget c 2 in
                  let ea, c1 = expr_of_value_goal c0 a in
                  let eb, c2 = expr_of_value_goal c1 b in
                  Sub (ea, eb), c2
                else goal_atom c v
              | _ :: _ -> goal_atom c v)))
        | _ -> goal_atom c v)
      | Operation.Intop_imm (op0, k) -> (
        match op0 with
        | Operation.Iadd -> (
          match args with
          | [] -> goal_atom c v
          | a :: l -> (
            match l with
            | [] ->
              if 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let ea, c1 = expr_of_value_goal c0 a in
                Add (ea, Const k), c1
              else goal_atom c v
            | _ :: _ -> goal_atom c v))
        | Operation.Isub -> (
          match args with
          | [] -> goal_atom c v
          | a :: l -> (
            match l with
            | [] ->
              if 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let ea, c1 = expr_of_value_goal c0 a in
                Sub (ea, Const k), c1
              else goal_atom c v
            | _ :: _ -> goal_atom c v))
        | Operation.Ilsl -> (
          match args with
          | [] -> goal_atom c v
          | a :: l -> (
            match l with
            | [] ->
              if (0 <= k && k < 16) && 1 <= c.budget
              then
                let c0 = spend_budget c 1 in
                let ea, c1 = expr_of_value_goal c0 a in
                Scale (1 lsl k, ea), c1
              else goal_atom c v
            | _ :: _ -> goal_atom c v))
        | _ -> goal_atom c v)
      | Operation.Specific s -> (
        match args with
        | [] -> goal_atom c v
        | a :: l -> (
          match l with
          | [] -> (
            match Arch.linear_specific_operation s with
            | Some p -> (
              let l0, k = p in
              match l0 with
              | [] -> goal_atom c v
              | ma :: l1 -> (
                match l1 with
                | [] ->
                  if 1 <= c.budget
                  then
                    let c0 = spend_budget c 1 in
                    let ea, c1 = expr_of_value_goal c0 a in
                    Add (Scale (ma, ea), Const k), c1
                  else goal_atom c v
                | _ :: _ -> goal_atom c v))
            | None -> goal_atom c v)
          | b :: l0 -> (
            match l0 with
            | [] -> (
              match Arch.linear_specific_operation s with
              | Some p -> (
                let l1, k = p in
                match l1 with
                | [] -> goal_atom c v
                | ma :: l2 -> (
                  match l2 with
                  | [] -> goal_atom c v
                  | mb :: l3 -> (
                    match l3 with
                    | [] ->
                      if 2 <= c.budget
                      then
                        let c0 = spend_budget c 2 in
                        let ea, c1 = expr_of_value_goal c0 a in
                        let eb, c2 = expr_of_value_goal c1 b in
                        Add (Add (Scale (ma, ea), Scale (mb, eb)), Const k), c2
                      else goal_atom c v
                    | _ :: _ -> goal_atom c v)))
              | None -> goal_atom c v)
            | _ :: _ -> goal_atom c v)))
      | _ -> goal_atom c v)
    (fun _ _ -> goal_atom c v)
    v

(** val linearize_goal : ctx -> Ssa.finished Ssa.Value.t -> Affine.t * ctx **)

let linearize_goal c v =
  let e, c0 = expr_of_value_goal (refill_budget c) v in
  match to_affine e with
  | Some p ->
    let form, _ = p in
    form, c0
  | None ->
    let id, c1 = intern c0 v in
    Affine.var id, c1

(** val comparison_facts : ctx -> Affine.t list -> bool -> Ssa.finished
    Ssa.Value.t -> (Affine.t list * Affine.t list) * ctx **)

let comparison_facts c side negate cond =
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
          | [] -> ([], side), c
          | a :: l -> (
            match l with
            | [] -> ([], side), c
            | b :: l0 -> (
              match l0 with
              | [] ->
                let p, c0 = linearize c side a in
                let la, side0 = p in
                let p0, c1 = linearize c0 side0 b in
                let lb, side1 = p0 in
                (facts negate cmp la lb, side1), c1
              | _ :: _ -> ([], side), c)))
        | _ -> ([], side), c)
      | Operation.Intop_imm (op0, k) -> (
        match op0 with
        | Operation.Icomp cmp -> (
          match args with
          | [] -> ([], side), c
          | a :: l -> (
            match l with
            | [] ->
              let p, c0 = linearize c side a in
              let la, side0 = p in
              (facts negate cmp la (Affine.constant k), side0), c0
            | _ :: _ -> ([], side), c))
        | _ -> ([], side), c)
      | _ -> ([], side), c)
    (fun _ _ -> ([], side), c)
    cond

(** val guard_test : block -> block -> (Ssa.finished Ssa.Value.t * bool) option
    **)

let guard_test target dominator =
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
    (fun index targets ->
      match targets with
      | [] -> None
      | ifnot :: l -> (
        match l with
        | [] -> None
        | ifso :: l0 -> (
          match l0 with
          | [] ->
            if
              Ssa.Block.dominates ifso target && sole_predecessor dominator ifso
            then Some (index, false)
            else if
              Ssa.Block.dominates ifnot target
              && sole_predecessor dominator ifnot
            then Some (index, true)
            else None
          | _ :: _ -> None)))
    (fun _ -> None)
    (fun _ -> None)
    (Ssa.Block.terminator dominator)

(** val guard_facts : block -> ((Affine.t list * Affine.t list) * ctx) -> block
    -> (Affine.t list * Affine.t list) * ctx **)

let guard_facts target st dominator =
  let p, c = st in
  let acc, side = p in
  match guard_test target dominator with
  | Some p0 ->
    let index, negate = p0 in
    let p1, c0 = comparison_facts c side negate index in
    let fs, side0 = p1 in
    (fs @ acc, side0), c0
  | None -> (acc, side), c

(** val guards_at : ctx -> Affine.t list -> block -> (Affine.t list * Affine.t
    list) * ctx **)

let guards_at c side target =
  List.fold_left (guard_facts target)
    (([], side), c)
    (Ssa.Block.strict_dominators target)
