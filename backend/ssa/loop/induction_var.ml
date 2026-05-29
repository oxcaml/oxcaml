[@@@ocaml.warning "+a-4-40-41-42-44"]

module Make (S : Ssa.Finished_graph) = struct
  module Dom = Ssa_dominators.Make (S)

  (* === Natural loop detection ===

     An edge u -> v is a back edge iff v dominates u. The natural loop of such
     an edge consists of the header v plus every node that can reach u in the
     CFG without passing through v. *)

  type loop =
    { header : S.Block.t;
      body : S.Block.Set.t;
      back_edges : S.Block.t list
    }

  let natural_loop_body ~header ~back_preds =
    let body = ref (S.Block.Set.singleton header) in
    let rec walk worklist =
      match worklist with
      | [] -> ()
      | bl :: rest ->
        let ps = Dom.predecessors bl in
        let added = ref rest in
        List.iter
          (fun p ->
            if not (S.Block.Set.mem p !body)
            then (
              body := S.Block.Set.add p !body;
              added := p :: !added))
          ps;
        walk !added
    in
    let seeds =
      List.filter
        (fun bp ->
          if S.Block.Set.mem bp !body
          then false
          else (
            body := S.Block.Set.add bp !body;
            true))
        back_preds
    in
    walk seeds;
    !body

  let find_loops () : loop list =
    let header_tbl : S.Block.t list S.Block.Tbl.t = S.Block.Tbl.create 8 in
    List.iter
      (fun bl ->
        List.iter
          (fun succ ->
            if Dom.dominates succ bl
            then
              let existing =
                match S.Block.Tbl.find_opt header_tbl succ with
                | Some l -> l
                | None -> []
              in
              S.Block.Tbl.replace header_tbl succ (bl :: existing))
          (Dom.successors bl))
      S.blocks;
    S.Block.Tbl.fold
      (fun header back_preds acc ->
        let body = natural_loop_body ~header ~back_preds in
        { header; body; back_edges = back_preds } :: acc)
      header_tbl []

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

  (* Map each Op's id to the block in which it is defined. *)
  let build_op_def_block () : S.Block.t S.Instruction.Id.Tbl.t =
    let tbl = S.Instruction.Id.Tbl.create 64 in
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
    tbl

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
    let op_def = build_op_def_block () in
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
