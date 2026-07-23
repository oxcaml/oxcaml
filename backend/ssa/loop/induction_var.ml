[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module Dom = Ssa_dominators.Make (S)

  (* Natural-loop detection is pure graph theory and lives in {!Natural_loop};
     instantiate it with the SSA block graph. *)
  module Block_graph = struct
    type node = S.Block.t

    module Set = S.Block.Set
    module Tbl = S.Block.Tbl

    let equal = S.Block.equal

    let nodes = S.blocks

    let successors = Dom.successors

    let predecessors = Dom.predecessors

    let dominates = Dom.dominates
  end

  module NL = Natural_loop.Make (Block_graph)

  type loop = NL.loop =
    { header : S.Block.t;
      body : S.Block.Set.t;
      back_edges : S.Block.t list
    }

  let find_loops = NL.find_loops

  let edge_dominates = NL.edge_dominates

  (* === Induction-variable classification === *)

  (* Structural reference equality on SSA values we care about. Ops compare by
     id; block params compare by (block, index). We don't recurse into Proj. *)
  let instr_same (a : S.Instruction.t) (b : S.Instruction.t) =
    match a, b with
    | Op { id = id1; _ }, Op { id = id2; _ } -> S.Instruction.Id.equal id1 id2
    | ( Block_param { block = b1; param_index = i1 },
        Block_param { block = b2; param_index = i2 } ) ->
      S.Block.equal b1 b2 && Int.equal i1 i2
    | ( ( Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
        | Stack_check _ | Name_for_debugger _ ),
        _ ) ->
      false

  (* Is [v] the [Block_param] for parameter [index] of [block]? *)
  let is_header_param (block : S.Block.t) (index : int) (v : S.Instruction.t) =
    match v with
    | Block_param { block = b; param_index } ->
      S.Block.equal b block && Int.equal param_index index
    | Op _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _
    | Name_for_debugger _ ->
      false

  (* Map each Op's id to the block in which it is defined. Memoized: the graph's
     blocks and their bodies do not change under a fixed [S] (the in-place
     rewrites the passes perform only replace terminators). *)
  let op_def_lazy : S.Block.t S.Instruction.Id.Tbl.t Lazy.t =
    lazy
      (let tbl = S.Instruction.Id.Tbl.create 64 in
       List.iter
         (fun (bl : S.Block.t) ->
           Array.iter
             (fun (i : S.Instruction.t) ->
               match i with
               | Op { id; _ } -> S.Instruction.Id.Tbl.replace tbl id bl
               | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
               | Stack_check _ | Name_for_debugger _ ->
                 ())
             bl.body)
         S.blocks;
       tbl)

  let op_def () = Lazy.force op_def_lazy

  let is_const (v : S.Instruction.t) =
    match v with
    | Op
        { op =
            ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
            | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ );
          _
        } ->
      true
    | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      false

  let const_int (v : S.Instruction.t) : int option =
    match v with
    | Op { op = Const_int n; _ }
      when Nativeint.equal (Nativeint.of_int (Nativeint.to_int n)) n ->
      Some (Nativeint.to_int n)
    | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      None

  (* Is [v] available (and loop-invariant) on entry to [preheader]? Its defining
     block must dominate [preheader], so a value built there can refer to it.
     Constants are rematerialised by the callers, so they qualify regardless of
     where they were defined. *)
  let rec available_at (preheader : S.Block.t) (v : S.Instruction.t) : bool =
    is_const v
    ||
    match v with
    | Op { id; _ } -> (
      match S.Instruction.Id.Tbl.find_opt (op_def ()) id with
      | Some bl -> S.Block.dominates bl preheader
      | None -> false)
    | Block_param { block; _ } -> S.Block.dominates block preheader
    | Proj { src; _ } -> available_at preheader src
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      false

  let back_edge_set (loop : loop) : S.Block.Set.t =
    List.fold_left
      (fun s b -> S.Block.Set.add b s)
      S.Block.Set.empty loop.back_edges

  let entry_predecessors (loop : loop) : S.Block.t list =
    List.filter
      (fun p -> not (List.exists (S.Block.equal p) loop.back_edges))
      (S.Block.predecessors loop.header)

  let preheader_of (loop : loop) : S.Block.t option =
    match entry_predecessors loop with
    | [preheader] -> Some preheader
    | [] | _ :: _ :: _ -> None

  (* A value is loop-invariant wrt [loop_body] if its defining block lies
     outside the body, or it is a compile-time constant. We do not try to hoist
     more generally. *)
  let rec is_loop_invariant op_def (loop_body : S.Block.Set.t)
      (v : S.Instruction.t) =
    match v with
    | Op
        { op =
            ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
            | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ );
          _
        } ->
      true
    | Op { id; _ } -> (
      match S.Instruction.Id.Tbl.find_opt op_def id with
      | None -> false
      | Some bl -> not (S.Block.Set.mem bl loop_body))
    | Block_param { block; _ } -> not (S.Block.Set.mem block loop_body)
    | Proj { src; _ } -> is_loop_invariant op_def loop_body src
    | Tuple _ | Push_trap _ | Pop_trap _ | Stack_check _ | Name_for_debugger _
      ->
      true

  type step =
    | Step_const of int
    | Step_var of S.Instruction.t

  type biv =
    { loop : loop;
      param_index : int;
      init : S.Instruction.t list;
      update : S.Instruction.t list;
      step : step;
      sign : [`Add | `Sub]
    }

  let step_same a b =
    match a, b with
    | Step_const x, Step_const y -> Int.equal x y
    | Step_var x, Step_var y -> instr_same x y
    | Step_const _, Step_var _ | Step_var _, Step_const _ -> false

  (* The signed per-iteration step of a constant-step BIV. *)
  let signed_step (biv : biv) : int option =
    match biv.step with
    | Step_const c -> Some (match biv.sign with `Add -> c | `Sub -> -c)
    | Step_var _ -> None

  (* Given [is_self] = "is the header block_param", decide whether [v] is of the
     form [self + c], [c + self] or [self - c] for loop-invariant [c]. *)
  let classify_update ~is_self ~op_def ~loop_body (v : S.Instruction.t) :
      (step * [`Add | `Sub]) option =
    match v with
    | Op { op = Intop_imm (Iadd, c); args = [| x |]; _ } when is_self x ->
      Some (Step_const c, `Add)
    | Op { op = Intop_imm (Isub, c); args = [| x |]; _ } when is_self x ->
      Some (Step_const c, `Sub)
    | Op { op = Intop Iadd; args = [| x; y |]; _ } ->
      if is_self x && is_loop_invariant op_def loop_body y
      then Some (Step_var y, `Add)
      else if is_self y && is_loop_invariant op_def loop_body x
      then Some (Step_var x, `Add)
      else None
    | Op { op = Intop Isub; args = [| x; y |]; _ } ->
      if is_self x && is_loop_invariant op_def loop_body y
      then Some (Step_var y, `Sub)
      else None
    | Op _ | Block_param _ | Proj _ | Tuple _ | Push_trap _ | Pop_trap _
    | Stack_check _ | Name_for_debugger _ ->
      None

  (* Collect the argument array of [Goto] terminators targeting the header.
     [Goto] args are optional: a [None] marks a parameter the builder dropped as
     unused. *)
  let pred_args_to_header ~header (pred : S.Block.t) :
      S.Instruction.t option array option =
    match pred.terminator with
    | Goto { goto; args } when S.Block.equal goto header -> Some args
    | Goto _ | Branch _ | Switch _ | Return _ | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call _ | Invalid _ ->
      None

  let analyze_loop ~op_def (loop : loop) : biv list =
    let { header; body; back_edges } = loop in
    let back_set =
      List.fold_left
        (fun s b -> S.Block.Set.add b s)
        S.Block.Set.empty back_edges
    in
    let all_pred_args =
      List.filter_map
        (fun p ->
          Option.map (fun args -> p, args) (pred_args_to_header ~header p))
        (S.Block.predecessors header)
    in
    let back_pred_args, init_pred_args =
      List.partition (fun (p, _) -> S.Block.Set.mem p back_set) all_pred_args
    in
    let bivs = ref [] in
    Array.iteri
      (fun index _ ->
        let is_self = is_header_param header index in
        let back_vals_opt =
          List.map (fun (_, args) -> args.(index)) back_pred_args
        in
        let init_vals =
          List.filter_map (fun (_, args) -> args.(index)) init_pred_args
        in
        match back_vals_opt with
        | [] -> ()
        | _ -> (
          if List.for_all Option.is_some back_vals_opt
          then
            let back_vals = List.map Option.get back_vals_opt in
            match back_vals with
            | [] -> ()
            | first :: rest -> (
              match classify_update ~is_self ~op_def ~loop_body:body first with
              | None -> ()
              | Some (step, sign) ->
                let agrees =
                  List.for_all
                    (fun v ->
                      match
                        classify_update ~is_self ~op_def ~loop_body:body v
                      with
                      | Some (step', sign') ->
                        step_same step step' && sign = sign'
                      | None -> false)
                    rest
                in
                if agrees
                then
                  bivs
                    := { loop;
                         param_index = index;
                         init = init_vals;
                         update = back_vals;
                         step;
                         sign
                       }
                       :: !bivs)))
      header.params;
    List.rev !bivs

  let analyze () : (loop * biv list) list =
    let loops = find_loops () in
    let op_def = op_def () in
    List.map (fun loop -> loop, analyze_loop ~op_def loop) loops

  (* === Printing === *)

  let print_step ppf = function
    | Step_const c -> Format.fprintf ppf "%d" c
    | Step_var v -> S.Instruction.print_as_ref ppf v

  let print_init ppf = function
    | [] -> Format.fprintf ppf "<none>"
    | vs ->
      Format.fprintf ppf "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           S.Instruction.print_as_ref)
        vs

  let print_biv ppf (biv : biv) =
    let op = match biv.sign with `Add -> "+=" | `Sub -> "-=" in
    Format.fprintf ppf "param=%d init=%a step %s %a" biv.param_index print_init
      biv.init op print_step biv.step

  let print ppf (loops : (loop * biv list) list) =
    Format.fprintf ppf "@[<v>induction variables:";
    match loops with
    | [] -> Format.fprintf ppf " <no loops>@]"
    | _ ->
      List.iter
        (fun ((loop : loop), bivs) ->
          Format.fprintf ppf "@,  loop header=%d" (loop.header.id :> int);
          match bivs with
          | [] -> Format.fprintf ppf "@,    <no basic induction variables>"
          | _ ->
            List.iter (fun b -> Format.fprintf ppf "@,    %a" print_biv b) bivs)
        loops;
      Format.fprintf ppf "@]"
end
