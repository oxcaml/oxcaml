(* Lattice-valued ZDDs (no memoization, no hash-consing) with cached [down0]
   values stored in each node to avoid repeatedly walking [lo] chains.
   This module mirrors [typing/ikinds/ldd.ml] but strips all memo tables and
   unique tables. We keep the rigid-variable table so the same rigid names map
   to the same solver variables across a run. *)

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (V : ORDERED) = struct
  module C = Axis_lattice

  type node = Obj.t
  (* node ::= node_block | int (leaf) -- we use Obj magic to unbox leaves *)

  type node_block =
    { v : var;
      lo : node;
      hi : node;
      down0 : C.t
    }

  and var =
    { id : int; (* ZDD order: smaller id = higher *)
      mutable state : var_state (* type+state of the variable *)
    }

  and var_state =
    | Unsolved
    | Solved of node
    | Rigid of V.t

  module Var = struct
    type t = var

    let var_id = ref (-1)

    let rigid_var_start = 100000000

    let rigid_var_id = ref rigid_var_start

    module VMap = Map.Make (struct
      type t = V.t

      let compare = V.compare
    end)

    let rigid_tbl : t VMap.t ref = ref VMap.empty

    let reset_rigids () =
      (* Clear all cached rigid variables and reset the counter so we don't
         retain solver-global state across runs. *)
      rigid_tbl := VMap.empty;
      rigid_var_id := rigid_var_start

    let make state =
      match state with
      | Rigid _ ->
        incr rigid_var_id;
        { id = !rigid_var_id; state }
      | _ ->
        incr var_id;
        { id = !var_id; state }

    let make_var () = make Unsolved

    let make_rigid ~name () =
      match VMap.find_opt name !rigid_tbl with
      | Some v -> v
      | None ->
        let v = make (Rigid name) in
        rigid_tbl := VMap.add name v !rigid_tbl;
        v

    let _id v = v.id
  end

  (* --------- node helpers --------- *)
  let[@inline] is_leaf (n : node) : bool = Obj.is_int n

  let[@inline] leaf_value (n : node) : C.t = Obj.obj n

  let[@inline] node_block (n : node) : node_block = Obj.obj n

  let[@inline] node_v (n : node) : var = (node_block n).v

  let[@inline] node_lo (n : node) : node = (node_block n).lo

  let[@inline] node_hi (n : node) : node = (node_block n).hi

  let[@inline] node_down0 (n : node) : C.t = (node_block n).down0

  let[@inline] make_node (v : var) (lo : node) (hi : node) : node =
    let down0 = if is_leaf lo then leaf_value lo else node_down0 lo
    in
    Obj.repr ({ v; lo; hi; down0 } : node_block)

  let[@inline] leaf (c : C.t) : node = Obj.repr c

  let bot = leaf C.bot

  let top = leaf C.top

  let[@inline] is_bot_node (n : node) : bool = C.equal (leaf_value n) C.bot

  let[@inline] down0 (n : node) : C.t =
    if is_leaf n then leaf_value n else node_down0 n

  (* Construct a node; must be in canonical form: hi = hi - lo. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    if is_bot_node hi then lo else make_node v lo hi

  (* Subtract subsets h - l (no memoization). *)
  let rec canonicalize (h : node) (l : node) : node =
    if is_leaf h
    then leaf (C.co_sub (leaf_value h) (down0 l))
    else if is_leaf l
    then canonicalize_right_leaf h l
    else
      let vh = node_v h in
      let hlo = node_lo h in
      let hhi = node_hi h in
      let vl = node_v l in
      let llo = node_lo l in
      let lhi = node_hi l in
      if vh.id = vl.id
      then
        let lo' = canonicalize hlo llo in
        let hi' = canonicalize (canonicalize hhi lhi) llo in
        if hlo == lo' && hhi == hi' then h else
        node_raw vh lo' hi'
      else if vh.id < vl.id
      then
        let lo' = canonicalize hlo l in
        let hi' = canonicalize hhi l in
        if hlo == lo' && hhi == hi' then h else
        node_raw vh lo' hi'
      else canonicalize h llo

  and canonicalize_right_leaf (h : node) (leaf_l : node) : node =
    let leaf_val = leaf_value leaf_l in
    canonicalize_right_leaf_aux h leaf_val

  and canonicalize_right_leaf_aux (h : node) (leaf_l : C.t) : node =
    if is_leaf h
    then leaf (C.co_sub (leaf_value h) leaf_l)
    else
      let vh = node_v h in
      let hlo = node_lo h in
      let hhi = node_hi h in
      let lo' = canonicalize_right_leaf_aux hlo leaf_l in
      let hi' = canonicalize_right_leaf_aux hhi leaf_l in
      if hlo == lo' && hhi == hi' then h else
      node_raw vh lo' hi'

  let node (v : var) (lo : node) (hi : node) : node =
    let hi' =
      canonicalize hi lo
    in
    node_raw v lo hi'

  (* --------- boolean algebra over nodes (no memoization) --------- *)
  let rec join (a : node) (b : node) =
    if is_leaf a
    then join_with_leaf a b
    else if is_leaf b
    then join_with_leaf b a
    else
      let va = node_v a in
      let alo = node_lo a in
      let ahi = node_hi a in
      let vb = node_v b in
      let blo = node_lo b in
      let bhi = node_hi b in
      if va.id = vb.id
      then
        node_raw va (join alo blo)
          (join (canonicalize ahi blo) (canonicalize bhi alo))
      else if va.id < vb.id
      then node_raw va (join alo b) (canonicalize ahi b)
      else node_raw vb (join a blo) (canonicalize bhi a)

  and join_with_leaf (leaf_a : node) (other : node) =
    let leaf_val = leaf_value leaf_a in
    (* Fast path *)
    if C.leq leaf_val (down0 other) then other else
    join_with_leaf_aux leaf_val other

  and join_with_leaf_aux (leaf_a : C.t) (other : node) =
    if is_leaf other
    then leaf (C.join leaf_a (leaf_value other))
    else
      let vb = node_v other in
      let blo = node_lo other in
      let bhi = node_hi other in
      let lo' = join_with_leaf_aux leaf_a blo in
      let hi' = canonicalize_right_leaf_aux bhi leaf_a in
      if lo' == blo && hi' == bhi then other else
      node_raw vb lo' hi'

  let rec meet (a : node) (b : node) =
    if is_leaf a
    then meet_with_leaf a b
    else if is_leaf b
    then meet_with_leaf b a
    else
      let va = node_v a in
      let alo = node_lo a in
      let ahi = node_hi a in
      let vb = node_v b in
      let blo = node_lo b in
      let bhi = node_hi b in
      if va.id = vb.id
      then
        let lo = meet alo blo in
        let hi = meet (join ahi alo) (join bhi blo) in
        node va lo hi
      else if va.id < vb.id
      then node va (meet alo b) (meet ahi b)
      else node vb (meet a blo) (meet a bhi)

  and meet_with_leaf (leaf_a : node) (other : node) =
    let leaf_val = leaf_value leaf_a in
    meet_with_leaf_aux leaf_val other

  and meet_with_leaf_aux (leaf_a : C.t) (other : node) =
    if is_leaf other
    then leaf (C.meet leaf_a (leaf_value other))
    else
      let vb = node_v other in
      let blo = node_lo other in
      let bhi = node_hi other in
      let lo' = meet_with_leaf_aux leaf_a blo in
      let hi' = meet_with_leaf_aux leaf_a bhi in
      if lo' == blo && hi' == bhi then other else
      node vb lo' hi'

  (* --------- public constructors --------- *)
  let[@inline] const (c : C.t) = leaf c

  let[@inline] mk_var (v : var) = node v bot top

  let rigid (name : V.t) = Var.make_rigid ~name ()

  let new_var () = Var.make_var ()

  (* --------- restrictions (x ← ⊥ / ⊤) (no memoization) --------- *)
  let rec restrict0 (x : var) (w : node) : node =
    if is_leaf w
    then w
    else
      let n = node_block w in
      if x.id < n.v.id
      then w
      else if n.v.id = x.id
      then n.lo
      else
        let lo' = restrict0 x n.lo in
        let hi' = restrict0 x n.hi in
        if lo' == n.lo && hi' == n.hi then w else
        node n.v lo' hi'

  let rec restrict1 (x : var) (w : node) : node =
    if is_leaf w
    then w
    else
      let n = node_block w in
      if x.id < n.v.id
      then w
      else if n.v.id = x.id
      then join n.lo n.hi
      else
        let lo' = restrict1 x n.lo in
        let hi' = restrict1 x n.hi in
        if lo' == n.lo && hi' == n.hi then w else
        node n.v lo' hi'

  (* --------- force (no memoization) --------- *)
  let rec force (w : node) : node =
    if is_leaf w
    then w
    else
      let n = node_block w in
      if n.v.id > Var.rigid_var_start
      then w
      else
        let lo' = force n.lo in
        let hi' = force n.hi in
        match n.v.state with
        | Solved d ->
          let d' = force d in
          n.v.state <- Solved d';
          join lo' (meet hi' d')
        | Unsolved ->
          if lo' == n.lo && hi' == n.hi
          then w
          else
            let d' = mk_var n.v in
            join lo' (meet hi' d')
        | Rigid _ -> failwith "force: rigid variable shouldn't be here"

  and var (v : var) = mk_var v

  (* This function is equivalent to `restrict0 x (force w)` *)
  let rec restrict0_force (x : var) (w : node) : node =
    if is_leaf w then w else
    let n = node_block w in
    match n.v.state with
    | Solved d -> 
      let lo' = restrict0_force x n.lo in
      let hi' = restrict0_force x n.hi in
      (* let d' = restrict0_force x d in *)
      let d_forced = force d in
      n.v.state <- Solved d_forced;
      let d' = restrict0 x d_forced in
      join lo' (meet hi' d')
    | Unsolved ->
      let lo' = restrict0_force x n.lo in
      if n.v.id == x.id then lo' else
      let hi' = restrict0_force x n.hi in
      if lo' == n.lo && hi' == n.hi then w else
      let d' = mk_var n.v in
      join lo' (meet hi' d')
    | Rigid _ -> w

  let sub_subsets (a : node) (b : node) =
    let a = force a in
    let b = force b in
    canonicalize a b

  (* --------- solve-on-install (no queues for LFP; GFP uses a stack)
       --------- *)
  let solve_lfp (var : var) (rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_lfp: rigid variable"
    | Solved _ -> invalid_arg "solve_lfp: solved variable"
    | Unsolved ->
      (* let rhs_forced = force rhs_raw in
      var.state <- Solved (restrict0 var rhs_forced) *)
      var.state <- Solved (restrict0_force var rhs_forced)

  let solve_gfp (var : var) (rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_gfp: rigid variable"
    | Solved _ -> invalid_arg "solve_gfp: solved variable"
    | Unsolved ->
      let rhs_forced = force rhs_raw in
      var.state <- Solved (restrict1 var rhs_forced)

  let lfp_queue = Stack.create ()

  let gfp_queue = Stack.create ()

  let enqueue_lfp (var : var) (rhs_raw : node) : unit =
    (* Immediate solve for LFP, matching ldd.ml behavior *)
    solve_lfp var rhs_raw

  let enqueue_gfp (var : var) (rhs_raw : node) : unit =
    Stack.push (var, rhs_raw) gfp_queue

  let solve_pending_lfps () : unit =
    while not (Stack.is_empty lfp_queue) do
      let var, rhs_raw = Stack.pop lfp_queue in
      solve_lfp var rhs_raw
    done

  let solve_pending_gfps () : unit =
    while not (Stack.is_empty gfp_queue) do
      let var, rhs_raw = Stack.pop gfp_queue in
      solve_gfp var rhs_raw
    done

  let solve_pending () : unit =
    solve_pending_lfps ();
    solve_pending_gfps ()

  (* Decompose into linear terms *)
  let decompose_linear ~(universe : var list) (n : node) =
    let rec go vs m ns =
      match vs with
      | [] -> m, ns
      | v :: vs' ->
        go vs' (restrict0 v m) (restrict1 v m :: List.map (restrict0 v) ns)
    in
    let base, linears = go universe (force n) [] in
    base, List.rev linears

  let leq (a : node) (b : node) =
    let a = force a in
    let b = force b in
    is_bot_node (sub_subsets a b)

  let rec round_up' (n : node) =
    if is_leaf n
    then leaf_value n
    else
      let nb = node_block n in
      let lo' = round_up' nb.lo in
      let hi' = round_up' nb.hi in
      C.join lo' hi'

  let round_up (n : node) =
    solve_pending ();
    let n = force n in
    round_up' n

  (* --------- polynomial-style pretty printer --------- *)
  let to_named_terms_with (pp_unsolved : var -> string) (w : node) :
      (C.t * string list) list =
    let rec aux (acc : string list) (w : node) : (C.t * string list) list =
      if is_leaf w
      then
        let c = leaf_value w in
        if C.equal c C.bot then [] else [c, acc]
      else
        let n = node_block w in
        let acc_hi =
          match n.v.state with
          | Rigid name -> V.to_string name :: acc
          | Unsolved -> pp_unsolved n.v :: acc
          | Solved _ -> failwith "solved vars should not appear after force"
        in
        let lo_list = aux acc n.lo in
        let hi_list = aux acc_hi n.hi in
        lo_list @ hi_list
    in
    aux [] (force w)

  let to_named_terms (w : node) : (C.t * string list) list =
    to_named_terms_with
      (fun v ->
        match v.state with
        | Rigid name -> V.to_string name
        | Unsolved -> "<unsolved-var:" ^ string_of_int v.id ^ ">"
        | Solved _ -> failwith "solved vars should not appear after force")
      w

  let pp (w : node) : string =
    let pp_coeff = C.to_string in
    (* Aggregate duplicate rigid var-sets by join on coefficients. *)
    let tbl : (string list, C.t) Hashtbl.t = Hashtbl.create 16 in
    let add_entry (c, names) =
      let vs = List.sort String.compare names in
      match Hashtbl.find_opt tbl vs with
      | None -> Hashtbl.add tbl vs c
      | Some prev -> Hashtbl.replace tbl vs (C.join prev c)
    in
    List.iter add_entry (to_named_terms w);
    let terms =
      Hashtbl.fold
        (fun vs c acc -> if C.equal c C.bot then acc else (vs, c) :: acc)
        tbl []
    in
    if terms = []
    then "⊥"
    else
      let term_body vs c =
        let is_top = C.equal c C.top in
        match vs, is_top with
        | [], true -> "⊤", false
        | [], false -> pp_coeff c, false
        | _ :: _, true -> String.concat " ⊓ " vs, List.length vs > 1
        | _ :: _, false -> pp_coeff c ^ " ⊓ " ^ String.concat " ⊓ " vs, true
      in
      let items =
        terms
        |> List.map (fun (vs, c) ->
               let body, has_meet = term_body vs c in
               body, has_meet)
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      let n_terms = List.length items in
      items
      |> List.map (fun (body, has_meet) ->
             if n_terms > 1 && has_meet then "(" ^ body ^ ")" else body)
      |> String.concat " ⊔ "

  let leq_with_reason (a : node) (b : node) =
    solve_pending ();
    let a = force a in
    let b = force b in
    let diff = sub_subsets a b in
    let witness = round_up' diff in
    match C.non_bot_axes witness with
    | [] -> None
    | axes -> Some axes

  let map_rigid (f : V.t -> node) (n : node) : node =
    let rec aux (n : node) : node =
      if is_leaf n
      then n
      else
        let nb = node_block n in
        let v = nb.v in
        let lo = nb.lo in
        let hi = nb.hi in
        match v.state with
        | Rigid name ->
          let lo' = aux lo in
          let hi' = aux hi in
          let replacement = f name in
          join lo' (meet hi' replacement)
        | Unsolved ->
          let lo' = aux lo in
          let hi' = aux hi in
          if lo' == lo && hi' == hi
          then n
          else
            let var_node = mk_var v in
            join lo' (meet hi' var_node)
        | Solved _ -> assert false
    in
    aux (force n)

  (* Clear all memo tables (none here); reset rigids to avoid unbounded
     growth. *)
  let clear_memos () : unit = Var.reset_rigids ()

  (* --------- structural debug printer --------- *)
  let pp_debug (w : node) : string =
    let pp_coeff = C.to_string in
    let b = Buffer.create 1024 in
    let module NodeTbl = Hashtbl.Make (struct
      type t = node

      let equal = ( == )

      let hash = Hashtbl.hash
    end) in
    let id_tbl = NodeTbl.create 97 in
    let printed : (int, unit) Hashtbl.t = Hashtbl.create 97 in
    let next_id = ref 0 in
    let get_id (n : node) : int =
      match NodeTbl.find_opt id_tbl n with
      | Some id -> id
      | None ->
        let id = !next_id in
        incr next_id;
        NodeTbl.add id_tbl n id;
        id
    in
    let pp_var_info (v : var) : string =
      let state_s =
        match v.state with
        | Unsolved -> "Unsolved"
        | Rigid name -> "Rigid(" ^ V.to_string name ^ ")"
        | Solved n -> "Solved(#" ^ string_of_int (get_id n) ^ ")"
      in
      Printf.sprintf "v#%d:%s" v.id state_s
    in
    let rec go indent (n : node) : unit =
      let id = get_id n in
      if Hashtbl.mem printed id
      then Buffer.add_string b (Printf.sprintf "%s#%d = <ref>\n" indent id)
      else (
        Hashtbl.add printed id ();
        if is_leaf n
        then
          Buffer.add_string b
            (Printf.sprintf "%sLeaf#%d c=%s\n" indent id
               (pp_coeff (leaf_value n)))
        else
          let nb = node_block n in
          Buffer.add_string b
            (Printf.sprintf "%sNode#%d %s down0=%s lo=#%d hi=#%d\n" indent id
               (pp_var_info nb.v) (pp_coeff nb.down0) (get_id nb.lo)
               (get_id nb.hi));
          let indent' = indent ^ "  " in
          go indent' nb.lo;
          go indent' nb.hi)
    in
    go "" w;
    Buffer.contents b
end
