(* Lattice-valued ZDDs *)

module type LATTICE = sig
  type t

  val bot : t

  val top : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val co_sub : t -> t -> t (* residual: join a (co_sub b a) = join a b *)

  val to_string : t -> string (* optional, for debug/printing *)

  val equal : t -> t -> bool

  val hash : t -> int

  val find_non_bot_axis : t -> int option
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (C : LATTICE) (V : ORDERED) = struct
  (* --------- variables --------- *)
  type node =
    | Leaf of
        { id : int;
          c : C.t
        }
    | Node of
        { id : int;
          v : var;
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
      match VMap.find_opt name !rigid_tbl with
      | Some v -> v
      | None ->
        let v = make (Rigid name) in
        rigid_tbl := VMap.add name v !rigid_tbl;
        v

    let _id v = v.id
  end

  (* --------- stable node ids --------- *)
  let next_node_id = ref (-1)

  let fresh_id () =
    incr next_node_id;
    !next_node_id

  let node_id = function Leaf n -> n.id | Node n -> n.id

  (* --------- leaf interning (ensures one pointer per equal coeff) --------- *)
  module LeafTbl = Hashtbl.Make (C)

  let leaf_tbl : node LeafTbl.t = LeafTbl.create 97

  let leaf (c : C.t) : node =
    match LeafTbl.find_opt leaf_tbl c with
    | Some n -> n
    | None ->
      let n = Leaf { id = fresh_id (); c } in
      LeafTbl.add leaf_tbl c n;
      n

  let bot = leaf C.bot

  let top = leaf C.top

  let is_bot_node n = n == bot

  let is_top_node n = n == top

  (* --------- unique table for internal nodes --------- *)
  module UKey = struct
    type t = int * int * int (* v.id, lo.id, hi.id *)

    let equal (a, b, c) (a', b', c') = a = a' && b = b' && c = c'

    let hash = Hashtbl.hash
  end

  module Unique = Hashtbl.Make (UKey)

  let initial_hashtbl_size = 1024

  let uniq_tbl : node Unique.t = Unique.create initial_hashtbl_size

  (* --------- persistent memos --------- *)
  module NodeTbl = struct
    module Tbl = Hashtbl.Make (struct
      type t = int

      let equal = ( = )

      let hash n = Hashtbl.hash n
    end)

    let create () = Tbl.create 1024

    let find_opt tbl n = Tbl.find_opt tbl (node_id n)

    let add tbl n r = Tbl.add tbl (node_id n) r

    let clear tbl = Tbl.clear tbl
  end

  (* Pack two 30-bit ids into one 60-bit int key. *)
  module PairKey = struct
    let bits = 30

    let mask = (1 lsl bits) - 1

    let[@inline] make_int a b =
      (* debug guard, elide in production *)
      assert (a land lnot mask = 0 && b land lnot mask = 0);
      ((a land mask) lsl bits) lor (b land mask)

    let[@inline] of_nodes h l = make_int (node_id h) (node_id l)
  end

  module NodePairTbl = struct
    module Tbl = Hashtbl.Make (struct
      type t = int

      let equal = ( = )

      let hash = Hashtbl.hash
    end)

    let create () = Tbl.create initial_hashtbl_size

    let find_opt tbl n m = Tbl.find_opt tbl (PairKey.of_nodes n m)

    let add tbl n m r = Tbl.add tbl (PairKey.of_nodes n m) r

    let clear = Tbl.clear
  end

  (* For asserting that var ids are strictly increasing down the tree. *)
  let var_index (n : node) : int =
    match n with Leaf _ -> 999999999 | Node n -> n.v.id

  (* Construct a node and hash-cons it. Must be in canonical form: hi = hi -
     low. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    assert (v.id < var_index lo);
    assert (v.id < var_index hi);
    if is_bot_node hi
    then lo
    else
      let key = v.id, node_id lo, node_id hi in
      match Unique.find_opt uniq_tbl key with
      | Some n -> n
      | None ->
        Global_counters.inc "node_raw_alloc";
        let n = Node { id = fresh_id (); v; lo; hi } in
        Unique.add uniq_tbl key n;
        n

  (* Subtract subsets h - l *)
  let memo_subs = NodePairTbl.create ()

  let rec down0 = function Leaf { c; _ } -> c | Node { lo; _ } -> down0 lo

  let rec canonicalize (h : node) (l : node) : node =
    if node_id h = node_id l
    then bot
    else if is_bot_node l
    then h
    else if is_top_node l
    then
      bot
      (* No need to test if h is top or bot because that path is fast anyway *)
    else
      (* Value at empty set *)
      match NodePairTbl.find_opt memo_subs h l with
      | Some r -> r
      | None ->
        Global_counters.inc "canonicalize";
        let r =
          match h with
          | Leaf x -> leaf (C.co_sub x.c (down0 l))
          | Node nh -> (
            match l with
            | Leaf _ ->
              node_raw nh.v (canonicalize nh.lo l) (canonicalize nh.hi l)
            | Node nl ->
              if nh.v.id = nl.v.id
              then
                let lo' = canonicalize nh.lo nl.lo in
                (* let hi' = canonicalize (canonicalize nh.hi nl.lo) nl.hi in *)
                let hi' = canonicalize (canonicalize nh.hi nl.hi) nl.lo in
                (* let hi' = canonicalize nh.hi (join nl.lo nl.hi) in *)
                node_raw nh.v lo' hi'
              else if nh.v.id < nl.v.id
              then node_raw nh.v (canonicalize nh.lo l) (canonicalize nh.hi l)
              else (* h.id > l.id *)
                canonicalize h nl.lo)
        in
        NodePairTbl.add memo_subs h l r;
        if node_id h == node_id r then Global_counters.inc "canonicalize_hit";
        r

  let node (v : var) (lo : node) (hi : node) : node =
    (* Don't need to memo this because canonicalize and node_raw are memoized *)
    let hi' = canonicalize hi lo in
    node_raw v lo hi'

  let memo_join = NodePairTbl.create ()

  let rec join (a : node) (b : node) =
    if node_id a = node_id b
    then a
    else if is_bot_node a
    then b
    else if is_bot_node b
    then a
    else if is_top_node a
    then top
    else if is_top_node b
    then top
    else if node_id a > node_id b
    then join b a
    else
      match NodePairTbl.find_opt memo_join a b with
      | Some r -> r
      | None ->
        Global_counters.inc "join";
        let r =
          match a, b with
          | Leaf x, Leaf y -> leaf (C.join x.c y.c)
          | Node na, Node nb ->
            if na.v.id = nb.v.id
            then
              (* node na.v (join na.lo nb.lo) (join na.hi nb.hi) *)
              node_raw na.v (join na.lo nb.lo)
                (join (canonicalize na.hi nb.lo) (canonicalize nb.hi na.lo))
            else if na.v.id < nb.v.id
            then
              (* node na.v (join na.lo b) (join na.hi b) *)
              node_raw na.v (join na.lo b) (canonicalize na.hi b)
            else
              (* node nb.v (join a nb.lo) (join a nb.hi) *)
              node_raw nb.v (join a nb.lo) (canonicalize nb.hi a)
          | Leaf _, Node nb ->
            (* node nb.v (join a nb.lo) (join a nb.hi) *)
            node_raw nb.v (join a nb.lo) (canonicalize nb.hi a)
          | Node na, Leaf _ ->
            (* node na.v (join na.lo b) (join na.hi b) *)
            node_raw na.v (join na.lo b) (canonicalize na.hi b)
        in
        NodePairTbl.add memo_join a b r;
        r

  let memo_meet = NodePairTbl.create ()

  let rec meet (a : node) (b : node) =
    if node_id a = node_id b
    then a
    else if is_bot_node a
    then bot
    else if is_bot_node b
    then bot
    else if is_top_node a
    then b
    else if is_top_node b
    then a
    else if node_id a > node_id b
    then meet b a
    else
      match NodePairTbl.find_opt memo_meet a b with
      | Some r -> r
      | None ->
        Global_counters.inc "meet";
        let r =
          match a, b with
          | Leaf x, Leaf y -> leaf (C.meet x.c y.c)
          | (Leaf _ as x), Node na | Node na, (Leaf _ as x) ->
            node na.v (meet na.lo x) (meet na.hi x)
          | Node na, Node nb ->
            if na.v.id = nb.v.id
            then
              let lo = meet na.lo nb.lo in
              (* let hi =
                 join (meet na.hi nb.lo)
                   (join (meet na.lo nb.hi) (meet na.hi nb.hi)) *)
              (* let left = sub_subsets (join na.hi na.lo) lo in
                 let right = sub_subsets (join nb.hi nb.lo) lo in
                 let hi = meet left right in *)
              let hi = meet (join na.hi na.lo) (join nb.hi nb.lo) in
              node na.v lo hi
            else if na.v.id < nb.v.id
            then
              (* let lo = meet na.lo b in let hi = meet na.hi b in node_raw na.v
                 lo (sub_subsets hi na.lo) *)
              node na.v (meet na.lo b) (meet na.hi b)
            else node nb.v (meet a nb.lo) (meet a nb.hi)
        in
        NodePairTbl.add memo_meet a b r;
        r

  (* --------- public constructors --------- *)
  let const (c : C.t) = leaf c

  let mk_var (v : var) = node v bot top

  let rigid (name : V.t) = Var.make_rigid ~name ()

  let new_var () = Var.make_var ()

  (* --------- restrictions (x ← ⊥ / ⊤) --------- *)
  module VarNodePairTbl = struct
    module Tbl = Hashtbl.Make (struct
      type t = int * int

      let equal (a, b) (c, d) = a = c && b = d

      let hash = Hashtbl.hash
    end)

    let create () = Tbl.create 1024

    let find_opt tbl v n = Tbl.find_opt tbl (v.id, node_id n)

    let add tbl v n r = Tbl.add tbl (v.id, node_id n) r

    let clear tbl = Tbl.clear tbl
  end

  let memo_restrict0 = VarNodePairTbl.create ()

  let rec restrict0 (x : var) (w : node) : node =
    match VarNodePairTbl.find_opt memo_restrict0 x w with
    | Some r -> r
    | None ->
      let r =
        match w with
        | Leaf _ -> w
        | Node n ->
          if x.id < n.v.id
          then w
          else if n.v.id = x.id
          then restrict0 x n.lo
          else node n.v (restrict0 x n.lo) (restrict0 x n.hi)
      in
      VarNodePairTbl.add memo_restrict0 x w r;
      r

  let memo_restrict1 = VarNodePairTbl.create ()

  let rec restrict1 (x : var) (w : node) : node =
    match VarNodePairTbl.find_opt memo_restrict1 x w with
    | Some r -> r
    | None ->
      let r =
        match w with
        | Leaf _ -> w
        | Node n ->
          if x.id < n.v.id
          then w
          else if n.v.id = x.id
          then join n.lo n.hi
          else node n.v (restrict1 x n.lo) (restrict1 x n.hi)
      in
      VarNodePairTbl.add memo_restrict1 x w r;
      r

  (* --------- force (per-call memo; no env object) --------- *)
  let memo_force = NodeTbl.create ()

  let rec force (w : node) : node =
    match NodeTbl.find_opt memo_force w with
    | Some r -> r
    | None ->
      Global_counters.inc "force";
      let r =
        match w with
        | Leaf _ -> w
        | Node n -> (
          if n.v.id > Var.rigid_var_start
          then w
          else
            let lo' = force n.lo and hi' = force n.hi in
            match n.v.state with
            | Solved d ->
              let d' = force d in
              join lo' (meet hi' d')
            | Unsolved ->
              if node_id lo' = node_id n.lo && node_id hi' = node_id n.hi
              then w
              else
                let d' = force (mk_var n.v) in
                join lo' (meet hi' d')
            | Rigid _ -> failwith "force: rigid variable shouldn't be here")
      in
      NodeTbl.add memo_force w r;
      r

  and var (v : var) = mk_var v

  let sub_subsets (a : node) (b : node) =
    let a = force a in
    let b = force b in
    canonicalize a b

  (* --------- solve-on-install (per-call; no env) --------- *)
  let solve_lfp (var : var) (rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_lfp: rigid variable"
    | Solved _ -> invalid_arg "solve_lfp: solved variable"
    | Unsolved ->
      let rhs_forced = force rhs_raw in
      var.state <- Solved (restrict0 var rhs_forced);
      NodeTbl.clear memo_force

  let solve_gfp (var : var) (rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_gfp: rigid variable"
    | Solved _ -> invalid_arg "solve_gfp: solved variable"
    | Unsolved ->
      let rhs_forced = force rhs_raw in
      var.state <- Solved (restrict1 var rhs_forced);
      NodeTbl.clear memo_force

  let lfp_queue = Stack.create ()

  let gfp_queue = Stack.create ()

  let enqueue_lfp (var : var) (rhs_raw : node) : unit =
    (* Stack.push (var, rhs_raw) lfp_queue *)
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
    (* Solve all pending LFPs first, then GFPs, and print timings. *)
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
    node_id (join a b) = node_id b

  let rec find_non_bot_axis (n : node) =
    match n with
    | Leaf { c; _ } -> C.find_non_bot_axis c
    | Node n -> (
      match find_non_bot_axis n.lo with
      | Some i -> Some i
      | None -> find_non_bot_axis n.hi)

  (* --------- polynomial-style pretty printer --------- *)
  (* Prints using the same conventions as lattice_polynomial.pp:
     - Deterministic term ordering (sort by rendered body string)
     - Parentheses around meets when there are multiple terms
     - Constants: ⊥ when empty; ⊤ when constant-top. *)
  (* Extract terms with optional naming function for non-rigid vars. *)
  let to_named_terms_with (pp_unsolved : var -> string) (w : node) :
      (C.t * string list) list =
    let rec aux (acc : string list) (w : node) : (C.t * string list) list =
      match w with
      | Leaf { c; _ } -> if C.equal c C.bot then [] else [c, acc]
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

  (* Pretty-print in polynomial style. If [pp_var] is provided, it is used to
     name non-rigid variables when encountered on hi-edges (e.g., unsolved vars
     for debugging/tests). *)
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
    (* Print the two nodes for debugging. *)
    solve_pending ();
    let a = force a in
    let b = force b in
    (* Format.eprintf "[ldd] leq_with_reason %s@ %s@" (pp a) (pp b); *)
    (* if leq a b then None *)
    (* else *)
    let diff = sub_subsets a b in
    find_non_bot_axis diff

  let memo_round_up' = NodeTbl.create ()

  let rec round_up' (n : node) =
    match NodeTbl.find_opt memo_round_up' n with
    | Some c -> c
    | None -> (
      match n with
      | Leaf { c; _ } -> c
      | Node n ->
        let lo' = round_up' n.lo in
        let hi' = round_up' n.hi in
        C.join lo' hi')

  let round_up (n : node) =
    solve_pending ();
    let n = force n in
    round_up' n

  (* Clear all memo tables *)
  let clear_memos () : unit =
    LeafTbl.clear leaf_tbl;
    Unique.clear uniq_tbl;
    NodeTbl.clear memo_force;
    NodePairTbl.clear memo_join;
    NodePairTbl.clear memo_meet;
    VarNodePairTbl.clear memo_restrict0;
    VarNodePairTbl.clear memo_restrict1;
    NodePairTbl.clear memo_subs;
    NodeTbl.clear memo_round_up';
    (* Also clear rigids to avoid unbounded growth across solvers. *)
    Var.reset_rigids ()

  (* --------- structural debug printer --------- *)
  (* Prints full DAG structure with node ids, var ids/states, and shared-node
     references. Useful to diagnose memoization/path effects. *)
  let pp_debug (w : node) : string =
    let pp_coeff = C.to_string in
    let b = Buffer.create 1024 in
    let seen = Hashtbl.create 97 in
    let pp_var_info (v : var) : string =
      let state_s =
        match v.state with
        | Unsolved -> "Unsolved"
        | Rigid name -> "Rigid(" ^ V.to_string name ^ ")"
        | Solved n -> "Solved(#" ^ string_of_int (node_id n) ^ ")"
      in
      Printf.sprintf "v#%d:%s" v.id state_s
    in
    let rec go indent (n : node) : unit =
      let id = node_id n in
      if Hashtbl.mem seen id
      then Buffer.add_string b (Printf.sprintf "%s#%d = <ref>\n" indent id)
      else (
        Hashtbl.add seen id true;
        match n with
        | Leaf { id; c } ->
          Buffer.add_string b
            (Printf.sprintf "%sLeaf#%d c=%s\n" indent id (pp_coeff c))
        | Node { id; v; lo; hi } ->
          Buffer.add_string b
            (Printf.sprintf "%sNode#%d %s lo=#%d hi=#%d\n" indent id
               (pp_var_info v) (node_id lo) (node_id hi));
          let indent' = indent ^ "  " in
          go indent' lo;
          go indent' hi)
    in
    go "" w;
    Buffer.contents b
end
