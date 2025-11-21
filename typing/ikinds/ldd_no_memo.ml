(* Lattice-valued ZDDs (no memoization, no hash-consing).
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
  (* --------- variables --------- *)
  type node =
    | Leaf of C.t
    | Node of
        { v : var;
          lo : node;
          hi : node
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
      if V.compare name name <> 0
      then make (Rigid name)
      else
        match VMap.find_opt name !rigid_tbl with
        | Some v -> v
        | None ->
          let v = make (Rigid name) in
          rigid_tbl := VMap.add name v !rigid_tbl;
          v

    let _id v = v.id
  end

  (* --------- leaf and constants (no interning) --------- *)
  let leaf (c : C.t) : node = Leaf c

  let bot = leaf C.bot

  let top = leaf C.top

  let is_bot_node = function Leaf c -> C.equal c C.bot | _ -> false

  let is_top_node = function Leaf c -> C.equal c C.top | _ -> false

  (* For asserting that var ids are strictly increasing down the tree. *)
  let var_index (n : node) : int =
    match n with Leaf _ -> 999999999 | Node n -> n.v.id

  let rec down0 = function Leaf c -> c | Node { lo; _ } -> down0 lo

  (* Construct a node; must be in canonical form: hi = hi - lo. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    assert (v.id < var_index lo);
    assert (v.id < var_index hi);
    if is_bot_node hi then lo else Node { v; lo; hi }

  (* Subtract subsets h - l (no memoization). *)
  let rec canonicalize (h : node) (l : node) : node =
    if h == l
    then bot
    else if is_bot_node l
    then h
    else if is_top_node l
    then bot
    else
      match h with
      | Leaf c -> leaf (C.co_sub c (down0 l))
      | Node nh -> (
        match l with
        | Leaf _ -> node_raw nh.v (canonicalize nh.lo l) (canonicalize nh.hi l)
        | Node nl ->
          if nh.v.id = nl.v.id
          then
            let lo' = canonicalize nh.lo nl.lo in
            let hi' = canonicalize (canonicalize nh.hi nl.hi) nl.lo in
            node_raw nh.v lo' hi'
          else if nh.v.id < nl.v.id
          then node_raw nh.v (canonicalize nh.lo l) (canonicalize nh.hi l)
          else canonicalize h nl.lo)

  let node (v : var) (lo : node) (hi : node) : node =
    let hi' = canonicalize hi lo in
    node_raw v lo hi'

  (* --------- boolean algebra over nodes (no memoization) --------- *)
  let rec join (a : node) (b : node) =
    if a == b
    then a
    else if is_bot_node a
    then b
    else if is_bot_node b
    then a
    else if is_top_node a
    then top
    else if is_top_node b
    then top
    else
      match a, b with
      | Leaf c1, Leaf c2 -> leaf (C.join c1 c2)
      | Node na, Node nb ->
        if na.v.id = nb.v.id
        then
          node_raw na.v (join na.lo nb.lo)
            (join (canonicalize na.hi nb.lo) (canonicalize nb.hi na.lo))
        else if na.v.id < nb.v.id
        then node_raw na.v (join na.lo b) (canonicalize na.hi b)
        else node_raw nb.v (join a nb.lo) (canonicalize nb.hi a)
      | Leaf _, Node nb -> node_raw nb.v (join a nb.lo) (canonicalize nb.hi a)
      | Node na, Leaf _ -> node_raw na.v (join na.lo b) (canonicalize na.hi b)

  let rec meet (a : node) (b : node) =
    if a == b
    then a
    else if is_bot_node a
    then bot
    else if is_bot_node b
    then bot
    else if is_top_node a
    then b
    else if is_top_node b
    then a
    else
      match a, b with
      | Leaf c1, Leaf c2 -> leaf (C.meet c1 c2)
      | (Leaf _ as x), Node na | Node na, (Leaf _ as x) ->
        node na.v (meet na.lo x) (meet na.hi x)
      | Node na, Node nb ->
        if na.v.id = nb.v.id
        then
          let lo = meet na.lo nb.lo in
          (* Simple conservative hi: *)
          let hi = meet (join na.hi na.lo) (join nb.hi nb.lo) in
          node na.v lo hi
        else if na.v.id < nb.v.id
        then node na.v (meet na.lo b) (meet na.hi b)
        else node nb.v (meet a nb.lo) (meet a nb.hi)

  (* --------- public constructors --------- *)
  let const (c : C.t) = leaf c

  let mk_var (v : var) = node v bot top

  let rigid (name : V.t) = Var.make_rigid ~name ()

  let new_var () = Var.make_var ()

  (* --------- restrictions (x ← ⊥ / ⊤) (no memoization) --------- *)
  let rec restrict0 (x : var) (w : node) : node =
    match w with
    | Leaf _ -> w
    | Node n ->
      if x.id < n.v.id
      then w
      else if n.v.id = x.id
      then restrict0 x n.lo
      else node n.v (restrict0 x n.lo) (restrict0 x n.hi)

  let rec restrict1 (x : var) (w : node) : node =
    match w with
    | Leaf _ -> w
    | Node n ->
      if x.id < n.v.id
      then w
      else if n.v.id = x.id
      then join n.lo n.hi
      else node n.v (restrict1 x n.lo) (restrict1 x n.hi)

  (* --------- force (no memoization) --------- *)
  let rec force (w : node) : node =
    match w with
    | Leaf _ -> w
    | Node n ->
      if n.v.id > Var.rigid_var_start
      then w
      else
        let lo' = force n.lo and hi' = force n.hi in
        match n.v.state with
        | Solved d ->
          let d' = force d in
          join lo' (meet hi' d')
        | Unsolved ->
          if lo' == n.lo && hi' == n.hi
          then w
          else
            let d' = force (mk_var n.v) in
            join lo' (meet hi' d')
        | Rigid _ -> failwith "force: rigid variable shouldn't be here"

  and var (v : var) = mk_var v

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
      let rhs_forced = force rhs_raw in
      var.state <- Solved (restrict0 var rhs_forced)

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

  let profile_enabled = false

  let solve_pending () : unit =
    let t0 = Sys.time () in
    solve_pending_lfps ();
    let t1 = Sys.time () in
    solve_pending_gfps ();
    let t2 = Sys.time () in
    let lfp_ms = (t1 -. t0) *. 1000. in
    let gfp_ms = (t2 -. t1) *. 1000. in
    if profile_enabled
    then
      Printf.printf "solve_pending: LFPs %.3f ms, GFPs %.3f ms\n" lfp_ms gfp_ms

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
    match n with
    | Leaf c -> c
    | Node n ->
      let lo' = round_up' n.lo in
      let hi' = round_up' n.hi in
      C.join lo' hi'

  let round_up (n : node) =
    solve_pending ();
    let n = force n in
    round_up' n

  (* --------- polynomial-style pretty printer --------- *)
  let to_named_terms_with (pp_unsolved : var -> string) (w : node) :
      (C.t * string list) list =
    let rec aux (acc : string list) (w : node) : (C.t * string list) list =
      match w with
      | Leaf c -> if C.equal c C.bot then [] else [c, acc]
      | Node n ->
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
    let diff = sub_subsets a b |> force in
    let witness = round_up' diff in
    match C.non_bot_axes witness with
    | [] -> None
    | axes -> Some axes

  let map_rigid (f : V.t -> node) (n : node) : node =
    let rec aux (n : node) : node =
      match n with
      | Leaf _ -> n
      | Node { v; lo; hi } -> (
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
        | Solved _ -> assert false)
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
        match n with
        | Leaf c ->
          Buffer.add_string b
            (Printf.sprintf "%sLeaf#%d c=%s\n" indent id (pp_coeff c))
        | Node { v; lo; hi } ->
          Buffer.add_string b
            (Printf.sprintf "%sNode#%d %s lo=#%d hi=#%d\n" indent id
               (pp_var_info v) (get_id lo) (get_id hi));
          let indent' = indent ^ "  " in
          go indent' lo;
          go indent' hi)
    in
    go "" w;
    Buffer.contents b
end
