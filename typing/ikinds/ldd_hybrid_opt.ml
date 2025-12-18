(* Lattice-valued ZDDs (no memoization, no hash-consing) with cached [down0]
   values stored in each node to avoid repeatedly walking [lo] chains.

   This is an optimized variant of [ldd_hybrid.ml] that adds fast paths and
   reduces allocation on the hot path. *)

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
    { id : int;
      mutable state : var_state;
      mutable var_node : node
    }

  and var_state =
    | Unsolved
    | Solved of node
    | Rigid of V.t

  let compare_var_full (a : var) (b : var) : int =
    match a.state, b.state with
    | Rigid na, Rigid nb -> V.compare na nb
    | _ ->
        invalid_arg
          "compare_var: unexpected id collision on non-rigid vars"

  let[@inline] compare_var (a : var) (b : var) : int =
    if a == b
    then 0
    else
      let h = Int.compare a.id b.id in
      if h <> 0 then h else compare_var_full a b

  (* --------- node helpers --------- *)
  let[@inline] is_leaf (n : node) : bool = Obj.is_int n

  let[@inline] leaf_value (n : node) : C.t = Obj.obj n

  let[@inline] node_block (n : node) : node_block = Obj.obj n

  let[@inline] node_down0 (n : node) : C.t = (node_block n).down0

  let[@inline] make_node (v : var) (lo : node) (hi : node) : node =
    let down0 = if is_leaf lo then leaf_value lo else node_down0 lo in
    Obj.repr ({ v; lo; hi; down0 } : node_block)

  let[@inline] leaf (c : C.t) : node = Obj.repr c

  let bot = leaf C.bot

  let top = leaf C.top

  let[@inline] is_bot_node (n : node) : bool = n == bot

  let[@inline] down0 (n : node) : C.t =
    if is_leaf n then leaf_value n else node_down0 n

  (* Construct a node; must be in canonical form: hi = hi - lo. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    if is_bot_node hi then lo else make_node v lo hi

  module Var = struct
    type t = var

    let next_non_rigid_id = ref (-1)

    let rigid_tbl : (int, t list) Hashtbl.t = Hashtbl.create 97

    let reset_rigids () = ()
    (* Hashtbl.clear rigid_tbl *)

    let[@inline] stable_hash (x : V.t) : int = Hashtbl.seeded_hash 0 x

    (* Keep rigid ids strictly above any non-rigid ids. We choose the top half
       of the *positive* int range, so rigid ids stay positive and satisfy the
       invariant used by [force] to avoid descending under rigids. *)
    let rigid_var_start = 1 lsl (Sys.word_size - 3)

    let[@inline] rigid_id (name : V.t) : int =
      let h = stable_hash name land (rigid_var_start - 1) in
      (* Ensure [id > rigid_var_start] (not just [>=]), since [force] uses a
         strict inequality. *)
      rigid_var_start lor (h lor 1)

    let make_var () =
      let next = !next_non_rigid_id + 1 in
      if next >= rigid_var_start
      then invalid_arg "Ldd_hybrid_opt: exhausted non-rigid id range";
      next_non_rigid_id := next;
      let v = { id = next; state = Unsolved; var_node = bot } in
      v.var_node <- node_raw v bot top;
      v

    let make_rigid ~name () =
      let id = rigid_id name in
      match Hashtbl.find_opt rigid_tbl id with
      | None ->
          let v = { id; state = Rigid name; var_node = bot } in
          v.var_node <- node_raw v bot top;
          Hashtbl.add rigid_tbl id [v];
          v
      | Some vars -> (
          match
            List.find_opt
              (fun (v : t) ->
                match v.state with
                | Rigid name' -> name == name' || V.compare name name' = 0
                | Unsolved | Solved _ -> false)
              vars
          with
          | Some v -> v
          | None ->
              let v = { id; state = Rigid name; var_node = bot } in
              v.var_node <- node_raw v bot top;
              Hashtbl.replace rigid_tbl id (v :: vars);
              v)
  end

  (* Subtract subsets h - l (no memoization). *)
  let rec canonicalize (h : node) (l : node) : node =
    if h == l
    then bot
    else if l == bot
    then h
    else if is_leaf h
    then
      let dh = leaf_value h in
      let dl = down0 l in
      if C.equal dl C.bot then h else leaf (C.co_sub dh dl)
    else if is_leaf l
    then canonicalize_right_leaf h l
    else
      let hb = node_block h in
      let lb = node_block l in
      let cmp = compare_var hb.v lb.v in
      if cmp = 0
      then
        let lo' = canonicalize hb.lo lb.lo in
        let hi1 =
          if lb.hi == bot then hb.hi else canonicalize hb.hi lb.hi
        in
        let hi' = if lb.lo == bot then hi1 else canonicalize hi1 lb.lo in
        if hb.lo == lo' && hb.hi == hi' then h else node_raw hb.v lo' hi'
      else if cmp < 0
      then
        let lo' = canonicalize hb.lo l in
        let hi' = canonicalize hb.hi l in
        if hb.lo == lo' && hb.hi == hi' then h else node_raw hb.v lo' hi'
      else canonicalize h lb.lo

  and canonicalize_right_leaf (h : node) (leaf_l : node) : node =
    let leaf_val = leaf_value leaf_l in
    if C.equal leaf_val C.bot then h
    else canonicalize_right_leaf_aux h leaf_val

  and canonicalize_right_leaf_aux (h : node) (leaf_l : C.t) : node =
    if is_leaf h
    then leaf (C.co_sub (leaf_value h) leaf_l)
    else
      let hb = node_block h in
      let lo' = canonicalize_right_leaf_aux hb.lo leaf_l in
      let hi' = canonicalize_right_leaf_aux hb.hi leaf_l in
      if hb.lo == lo' && hb.hi == hi' then h else node_raw hb.v lo' hi'

  let node (v : var) (lo : node) (hi : node) : node =
    if lo == bot then node_raw v lo hi else node_raw v lo (canonicalize hi lo)

  (* --------- boolean algebra over nodes (no memoization) --------- *)
  let rec join (a : node) (b : node) =
    if a == b
    then a
    else if is_leaf a
    then join_with_leaf a b
    else if is_leaf b
    then join_with_leaf b a
    else
      let na = node_block a in
      let nb = node_block b in
      let cmp = compare_var na.v nb.v in
      if cmp = 0
      then
        node_raw na.v (join na.lo nb.lo)
          (join (canonicalize na.hi nb.lo) (canonicalize nb.hi na.lo))
      else if cmp < 0
      then node_raw na.v (join na.lo b) (canonicalize na.hi b)
      else node_raw nb.v (join a nb.lo) (canonicalize nb.hi a)

  and join_with_leaf (leaf_a : node) (other : node) =
    let leaf_val = leaf_value leaf_a in
    if C.equal leaf_val C.top
    then top
    else
    (* Fast path *)
    if C.leq leaf_val (down0 other) then other
    else join_with_leaf_aux leaf_val other

  and join_with_leaf_aux (leaf_a : C.t) (other : node) =
    if is_leaf other
    then leaf (C.join leaf_a (leaf_value other))
    else
      let nb = node_block other in
      let lo' = join_with_leaf_aux leaf_a nb.lo in
      let hi' = canonicalize_right_leaf_aux nb.hi leaf_a in
      if lo' == nb.lo && hi' == nb.hi then other else node_raw nb.v lo' hi'

  let rec meet (a : node) (b : node) =
    if a == b
    then a
    else if is_leaf a
    then meet_with_leaf a b
    else if is_leaf b
    then meet_with_leaf b a
    else
      let na = node_block a in
      let nb = node_block b in
      let cmp = compare_var na.v nb.v in
      if cmp = 0
      then
        let lo = meet na.lo nb.lo in
        let hi = meet (join na.hi na.lo) (join nb.hi nb.lo) in
        node na.v lo hi
      else if cmp < 0
      then node na.v (meet na.lo b) (meet na.hi b)
      else node nb.v (meet a nb.lo) (meet a nb.hi)

  and meet_with_leaf (leaf_a : node) (other : node) =
    let leaf_val = leaf_value leaf_a in
    if C.equal leaf_val C.top
    then other
    else if C.equal leaf_val C.bot
    then bot
    else meet_with_leaf_aux leaf_val other

  and meet_with_leaf_aux (leaf_a : C.t) (other : node) =
    if is_leaf other
    then leaf (C.meet leaf_a (leaf_value other))
    else
      let nb = node_block other in
      let lo' = meet_with_leaf_aux leaf_a nb.lo in
      let hi' = meet_with_leaf_aux leaf_a nb.hi in
      if lo' == nb.lo && hi' == nb.hi then other else node nb.v lo' hi'

  (* --------- public constructors --------- *)
  let[@inline] const (c : C.t) = leaf c

  let[@inline] mk_var (v : var) : node = v.var_node

  let rigid (name : V.t) = Var.make_rigid ~name ()

  let new_var () = Var.make_var ()

  (* --------- restrictions (x ← ⊥ / ⊤) (no memoization) --------- *)
  let rec restrict0 (x : var) (w : node) : node =
    if is_leaf w
    then w
    else
      let n = node_block w in
      let cmp = compare_var x n.v in
      if cmp < 0
      then w
      else if cmp = 0
      then n.lo
      else
        let lo' = restrict0 x n.lo in
        let hi' = restrict0 x n.hi in
        if lo' == n.lo && hi' == n.hi then w else node n.v lo' hi'

  let rec restrict1 (x : var) (w : node) : node =
    if is_leaf w
    then w
    else
      let n = node_block w in
      let cmp = compare_var x n.v in
      if cmp < 0
      then w
      else if cmp = 0
      then join n.lo n.hi
      else
        let lo' = restrict1 x n.lo in
        let hi' = restrict1 x n.hi in
        if lo' == n.lo && hi' == n.hi then w else node n.v lo' hi'

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
    if x.id > Var.rigid_var_start then restrict0 x (force w)
    else
    if is_leaf w
    then w
    else
      let n = node_block w in
      match n.v.state with
      | Solved d ->
          let lo' = restrict0_force x n.lo in
          let hi' = restrict0_force x n.hi in
          let d_forced = force d in
          n.v.state <- Solved d_forced;
          let d' = restrict0 x d_forced in
          join lo' (meet hi' d')
      | Unsolved ->
          let lo' = restrict0_force x n.lo in
          if compare_var n.v x = 0
          then lo'
          else
            let hi' = restrict0_force x n.hi in
            if lo' == n.lo && hi' == n.hi
            then w
            else
              let d' = mk_var n.v in
              join lo' (meet hi' d')
      | Rigid _ -> w

  let sub_subsets (a : node) (b : node) =
    canonicalize (force a) (force b)

  let sub_subsets_forced (a : node) (b : node) : node = canonicalize a b

  (* --------- solve-on-install (no queues for LFP; GFP uses a stack)
       --------- *)
  let solve_lfp (var : var) (rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_lfp: rigid variable"
    | Solved _ -> invalid_arg "solve_lfp: solved variable"
    | Unsolved -> var.state <- Solved (restrict0_force var rhs_raw)

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
    is_bot_node (sub_subsets_forced a b)

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

  let is_const (n : node) : bool =
    let n = force n in
    is_leaf n

  (* --------- polynomial-style pretty printer --------- *)
  let to_named_terms_with (pp_unsolved : var -> string) (w : node) :
      (C.t * string list) list =
    let rec aux (acc_vars : string list) (w : node)
        (acc_terms : (C.t * string list) list) =
      if is_leaf w
      then
        let c = leaf_value w in
        if C.equal c C.bot then acc_terms else (c, acc_vars) :: acc_terms
      else
        let n = node_block w in
        let acc_terms = aux acc_vars n.lo acc_terms in
        let acc_hi =
          match n.v.state with
          | Rigid name -> V.to_string name :: acc_vars
          | Unsolved -> pp_unsolved n.v :: acc_vars
          | Solved _ -> failwith "solved vars should not appear after force"
        in
        aux acc_hi n.hi acc_terms
    in
    aux [] (force w) [] |> List.rev

  let to_named_terms (w : node) : (C.t * string list) list =
    to_named_terms_with
      (fun v ->
        match v.state with
        | Rigid name -> V.to_string name
        | Unsolved -> "<unsolved-var:" ^ string_of_int v.id ^ ">"
        | Solved _ ->
            failwith "solved vars should not appear after force")
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
        | _ :: _, false ->
            pp_coeff c ^ " ⊓ " ^ String.concat " ⊓ " vs, true
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
    let diff = sub_subsets_forced a b in
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
        | Solved _ ->
            invalid_arg "map_rigid: solved vars should not appear after force"
    in
    aux (force n)

  let clear_memos () : unit = Var.reset_rigids ()

  (* --------- structural debug printer --------- *)
  let pp_debug (w : node) : string =
    let pp_coeff = C.to_string in
    let b = Buffer.create 1024 in
    let[@inline] hash_node (n : node) : int =
      (Obj.magic n : int) land max_int
    in
    let module NodeTbl = Hashtbl.Make (struct
      type t = node

      let equal = ( == )

      let hash = hash_node
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
