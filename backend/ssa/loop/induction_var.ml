[@@@ocaml.warning "+a-4-40-41-42-44"]

open Ssa

(* === Successors of a block === *)

let block_successors (b : block) : block list =
  match b.terminator with
  | Never | Return _ | Raise _ | Tailcall_func _ -> []
  | Goto { goto; _ } -> [goto]
  | Branch { conditions; else_goto } ->
    let succs = ref [else_goto] in
    Array.iter (fun (_, dst) -> succs := dst :: !succs) conditions;
    !succs
  | Switch (targets, _) -> Array.to_list targets
  | Tailcall_self { destination; _ } -> [destination]
  | Call { continuation; exn_continuation; _ }
  | Prim { continuation; exn_continuation; _ } ->
    continuation
    :: (match exn_continuation with Some l -> [l] | None -> [])
  | Invalid { continuation; _ } -> (
    match continuation with Some l -> [l] | None -> [])

(* === Dominator tree (Cooper-Harvey-Kennedy), plus predecessor map === *)

type dom_info =
  { dom_in : int Block.Tbl.t;
    dom_out : int Block.Tbl.t;
    preds : block list Block.Tbl.t
  }

let compute_dom_info (t : Ssa.t) : dom_info =
  let preds = Block.Tbl.create 16 in
  List.iter (fun bl -> Block.Tbl.replace preds bl []) t.blocks;
  let add_pred ~src ~dst =
    match Block.Tbl.find_opt preds dst with
    | Some ps -> Block.Tbl.replace preds dst (src :: ps)
    | None -> ()
  in
  List.iter
    (fun bl ->
      List.iter (fun dst -> add_pred ~src:bl ~dst) (block_successors bl))
    t.blocks;
  let rpo =
    let visited = Block.Tbl.create 16 in
    let order = ref [] in
    let rec dfs bl =
      if not (Block.Tbl.mem visited bl)
      then (
        Block.Tbl.replace visited bl ();
        List.iter dfs (block_successors bl);
        order := bl :: !order)
    in
    dfs t.entry;
    !order
  in
  let rpo_index = Block.Tbl.create 16 in
  List.iteri (fun i bl -> Block.Tbl.replace rpo_index bl i) rpo;
  let idom = Block.Tbl.create 16 in
  Block.Tbl.replace idom t.entry t.entry;
  let intersect b1 b2 =
    let b1 = ref b1 and b2 = ref b2 in
    while not (block_equal !b1 !b2) do
      while Block.Tbl.find rpo_index !b1 > Block.Tbl.find rpo_index !b2 do
        b1 := Block.Tbl.find idom !b1
      done;
      while Block.Tbl.find rpo_index !b2 > Block.Tbl.find rpo_index !b1 do
        b2 := Block.Tbl.find idom !b2
      done
    done;
    !b1
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun bl ->
        if not (block_equal bl t.entry)
        then
          let ps = Block.Tbl.find preds bl in
          let processed = List.filter (fun p -> Block.Tbl.mem idom p) ps in
          match processed with
          | [] -> ()
          | first :: rest ->
            let new_idom = List.fold_left intersect first rest in
            if
              not
                (match Block.Tbl.find_opt idom bl with
                | Some d -> block_equal d new_idom
                | None -> false)
            then (
              Block.Tbl.replace idom bl new_idom;
              changed := true))
      rpo
  done;
  let dom_children = Block.Tbl.create 16 in
  Block.Tbl.iter
    (fun bl parent ->
      if not (block_equal bl parent)
      then
        let kids =
          match Block.Tbl.find_opt dom_children parent with
          | Some l -> l
          | None -> []
        in
        Block.Tbl.replace dom_children parent (bl :: kids))
    idom;
  let dom_in = Block.Tbl.create 16 in
  let dom_out = Block.Tbl.create 16 in
  let time = ref 0 in
  let rec compute_times bl =
    Block.Tbl.replace dom_in bl !time;
    incr time;
    (match Block.Tbl.find_opt dom_children bl with
    | Some kids -> List.iter compute_times kids
    | None -> ());
    Block.Tbl.replace dom_out bl !time;
    incr time
  in
  compute_times t.entry;
  { dom_in; dom_out; preds }

let dominates info a b =
  Block.Tbl.find info.dom_in a <= Block.Tbl.find info.dom_in b
  && Block.Tbl.find info.dom_out a >= Block.Tbl.find info.dom_out b

(* === Natural loop detection ===

   An edge u -> v is a back edge iff v dominates u. The natural loop of such
   an edge consists of the header v plus every node that can reach u in the
   CFG without passing through v. *)

type loop =
  { header : block;
    body : Block.Set.t;
    back_edges : block list
  }

let natural_loop_body info ~header ~back_preds =
  let body = ref (Block.Set.singleton header) in
  let rec walk worklist =
    match worklist with
    | [] -> ()
    | bl :: rest ->
      let ps = Block.Tbl.find info.preds bl in
      let added = ref rest in
      List.iter
        (fun p ->
          if not (Block.Set.mem p !body)
          then (
            body := Block.Set.add p !body;
            added := p :: !added))
        ps;
      walk !added
  in
  let seeds =
    List.filter
      (fun bp ->
        if Block.Set.mem bp !body
        then false
        else (
          body := Block.Set.add bp !body;
          true))
      back_preds
  in
  walk seeds;
  !body

let find_loops (t : Ssa.t) (info : dom_info) : loop list =
  let header_tbl : block list Block.Tbl.t = Block.Tbl.create 8 in
  List.iter
    (fun bl ->
      List.iter
        (fun succ ->
          if dominates info succ bl
          then
            let existing =
              match Block.Tbl.find_opt header_tbl succ with
              | Some l -> l
              | None -> []
            in
            Block.Tbl.replace header_tbl succ (bl :: existing))
        (block_successors bl))
    t.blocks;
  Block.Tbl.fold
    (fun header back_preds acc ->
      let body = natural_loop_body info ~header ~back_preds in
      { header; body; back_edges = back_preds } :: acc)
    header_tbl []

(* === Induction-variable classification === *)

(* Structural reference equality on SSA values we care about. Ops compare by
   id; block params compare by (block, index). We don't recurse into Proj. *)
let instr_same (a : instruction) (b : instruction) =
  match a, b with
  | Op { id = id1; _ }, Op { id = id2; _ } -> InstructionId.equal id1 id2
  | ( Block_param { block = b1; index = i1; _ },
      Block_param { block = b2; index = i2; _ } ) ->
    block_equal b1 b2 && Int.equal i1 i2
  | ( ( Op _ | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
      | Name_for_debugger _ ),
      _ ) ->
    false

(* Map each Op's id to the block in which it is defined. *)
let build_op_def_block (t : Ssa.t) : block InstructionId.Tbl.t =
  let tbl = InstructionId.Tbl.create 64 in
  List.iter
    (fun (bl : block) ->
      Array.iter
        (fun (i : instruction) ->
          match i with
          | Op { id; _ } -> InstructionId.Tbl.replace tbl id bl
          | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
          | Name_for_debugger _ ->
            ())
        bl.body)
    t.blocks;
  tbl

(* A value is loop-invariant wrt [loop_body] if its defining block lies
   outside the body, or it is a compile-time constant. We do not try to hoist
   more generally. *)
let rec is_loop_invariant op_def (loop_body : Block.Set.t) (v : instruction) =
  match v with
  | Op
      { op =
          ( Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
          | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ );
        _
      } ->
    true
  | Op { id; _ } -> (
    match InstructionId.Tbl.find_opt op_def id with
    | None -> false
    | Some bl -> not (Block.Set.mem bl loop_body))
  | Block_param { block; _ } -> not (Block.Set.mem block loop_body)
  | Proj { src; _ } -> is_loop_invariant op_def loop_body src
  | Pushtrap _ | Poptrap _ | Stack_check _ | Name_for_debugger _ -> true

type step =
  | Step_const of int
  | Step_var of instruction

type biv =
  { loop : loop;
    param_index : int;
    init : instruction list;
    update : instruction list;
    step : step;
    sign : [ `Add | `Sub ]
  }

let step_same a b =
  match a, b with
  | Step_const x, Step_const y -> Int.equal x y
  | Step_var x, Step_var y -> instr_same x y
  | Step_const _, Step_var _ | Step_var _, Step_const _ -> false

(* Given [self] = the header block_param, decide whether [v] is of the form
   [self + c], [c + self] or [self - c] for loop-invariant [c]. *)
let classify_update ~self ~op_def ~loop_body (v : instruction) :
    (step * [ `Add | `Sub ]) option =
  match v with
  | Op { op = Intop_imm (Iadd, c); args = [| x |]; _ }
    when instr_same x self ->
    Some (Step_const c, `Add)
  | Op { op = Intop_imm (Isub, c); args = [| x |]; _ }
    when instr_same x self ->
    Some (Step_const c, `Sub)
  | Op { op = Intop Iadd; args = [| x; y |]; _ } ->
    if instr_same x self && is_loop_invariant op_def loop_body y
    then Some (Step_var y, `Add)
    else if instr_same y self && is_loop_invariant op_def loop_body x
    then Some (Step_var x, `Add)
    else None
  | Op { op = Intop Isub; args = [| x; y |]; _ } ->
    if instr_same x self && is_loop_invariant op_def loop_body y
    then Some (Step_var y, `Sub)
    else None
  | Op _ | Block_param _ | Proj _ | Pushtrap _ | Poptrap _ | Stack_check _
  | Name_for_debugger _ ->
    None

let header_predecessors (header : block) : block list =
  match header.desc with
  | Merge { predecessors } -> predecessors
  | TrapHandler { predecessors } -> predecessors
  | BranchTarget { predecessor } -> [predecessor]
  | FunctionStart | CallContinuation _ -> []

(* Collect (pred, args) pairs from Goto terminators targeting the header. *)
let pred_args_to_header ~header (pred : block) : instruction array option =
  match pred.terminator with
  | Goto { goto; args } when block_equal goto header -> Some args
  | Never | Goto _ | Branch _ | Switch _ | Return _ | Raise _
  | Tailcall_self _ | Tailcall_func _ | Call _ | Prim _ | Invalid _ ->
    None

let analyze_loop ~op_def (loop : loop) : biv list =
  let { header; body; back_edges } = loop in
  let back_set =
    List.fold_left (fun s b -> Block.Set.add b s) Block.Set.empty back_edges
  in
  let all_pred_args =
    List.filter_map
      (fun p ->
        Option.map (fun args -> p, args) (pred_args_to_header ~header p))
      (header_predecessors header)
  in
  let back_pred_args, init_pred_args =
    List.partition (fun (p, _) -> Block.Set.mem p back_set) all_pred_args
  in
  let bivs = ref [] in
  Array.iteri
    (fun index typ ->
      let self : instruction = Block_param { block = header; index; typ } in
      let back_vals =
        List.map (fun (_, args) -> args.(index)) back_pred_args
      in
      let init_vals =
        List.map (fun (_, args) -> args.(index)) init_pred_args
      in
      match back_vals with
      | [] -> ()
      | first :: rest -> (
        match classify_update ~self ~op_def ~loop_body:body first with
        | None -> ()
        | Some (step, sign) ->
          let agrees =
            List.for_all
              (fun v ->
                match classify_update ~self ~op_def ~loop_body:body v with
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
                 :: !bivs))
    header.params;
  List.rev !bivs

let analyze (t : Ssa.t) : (loop * biv list) list =
  let info = compute_dom_info t in
  let loops = find_loops t info in
  let op_def = build_op_def_block t in
  List.map (fun loop -> loop, analyze_loop ~op_def loop) loops

(* === Printing === *)

let print_step ppf = function
  | Step_const c -> Format.fprintf ppf "%d" c
  | Step_var v -> print_instr_ref ppf v

let print_init ppf = function
  | [] -> Format.fprintf ppf "<none>"
  | vs ->
    Format.fprintf ppf "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         print_instr_ref)
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
      (fun (loop, bivs) ->
        Format.fprintf ppf "@,  loop header=%d" loop.header.id;
        match bivs with
        | [] -> Format.fprintf ppf "@,    <no basic induction variables>"
        | _ ->
          List.iter (fun b -> Format.fprintf ppf "@,    %a" print_biv b) bivs)
      loops;
    Format.fprintf ppf "@]"
