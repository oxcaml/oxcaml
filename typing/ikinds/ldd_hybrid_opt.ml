(* Lattice-valued ZDDs.

   Big picture: we represent LDDs as a ZDD-like DAG ordered by [var] id.
   A [node_block] is the lattice expression lo ⊔ (var ⊓ hi), with the
   invariant that all vars appearing in [lo] and [hi] are strictly greater
   than the node's [var]. We also maintain canonical form [hi = hi - lo],
   so [hi] is disjoint from [lo]. *)

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (V : ORDERED) = struct

  type node = Obj.t
  (* node ::= node_block | int (leaf) -- we use Obj magic to unbox leaves *)

  type node_block =
    { v : var;
      lo : node;
      hi : node;
      (* Cached lattice contribution of the all-zero (lo) branch. *)
      down0 : Axis_lattice.t
    }

  and var =
    { id : int;
      mutable state : var_state;
      (* Cached node for just this var (⊥ ⊔ (v ⊓ ⊤)) to avoid reallocating. *)
      mutable var_node : node
    }

  and var_state =
    | Unsolved
    | Solved of node
    | Rigid of V.t

  let compare_var_full (a : var) (b : var) : int =
    match a.state, b.state with
    | Rigid na, Rigid nb -> V.compare na nb
    | _ -> invalid_arg "compare_var: unexpected id collision on non-rigid vars"

  let[@inline] compare_var (a : var) (b : var) : int =
    if a == b
    then 0
    else
      let h = Int.compare a.id b.id in
      if h <> 0 then h else compare_var_full a b

  (* --------- node helpers --------- *)
  let[@inline] is_leaf (n : node) : bool = Obj.is_int n

  let _assert_axis_lattice_is_int (x : Axis_lattice.t) = (x :> int)

  module Unsafe = struct
    let[@inline] leaf_value (n : node) : Axis_lattice.t = Obj.obj n

    let[@inline] node_block (n : node) : node_block = Obj.obj n

    let[@inline] node_down0 (n : node) : Axis_lattice.t =
      (node_block n).down0
  end

  let[@inline] make_node (v : var) (lo : node) (hi : node) : node =
    let down0 =
      if is_leaf lo then Unsafe.leaf_value lo else Unsafe.node_down0 lo
    in
    Obj.repr ({ v; lo; hi; down0 } : node_block)

  let[@inline] leaf (c : Axis_lattice.t) : node = Obj.repr c

  let bot = leaf Axis_lattice.bot

  let top = leaf Axis_lattice.top

  let[@inline] is_bot_node (n : node) : bool = n == bot

  let[@inline] down0 (n : node) : Axis_lattice.t =
    if is_leaf n then Unsafe.leaf_value n else Unsafe.node_down0 n

  (* Construct a node; must be in canonical form: hi = hi - lo. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    if is_bot_node hi then lo else make_node v lo hi

  module Var = struct
    type t = var

    let prev_non_rigid_id = ref (-1)

    let rigid_tbl : (int, t list) Hashtbl.t = Hashtbl.create 97

    let reset_rigids () = ()
    (* Hashtbl.clear rigid_tbl *)

    let[@inline] stable_hash (x : V.t) : int = Hashtbl.seeded_hash 0 x

    (* Keep rigid ids strictly above any non-rigid ids. We choose the top half
       of the *positive* int range, so rigid ids stay positive and satisfy the
       invariant used by [inline_solved_vars] to avoid descending under
       rigids. *)
    let rigid_var_start = 1 lsl (Sys.word_size - 3)

    let[@inline] rigid_id (name : V.t) : int =
      let h = stable_hash name land (rigid_var_start - 1) in
      (* Ensure [id > rigid_var_start] (not just [>=]), since
         [inline_solved_vars] uses a strict inequality. *)
      rigid_var_start lor (h lor 1)

    let make_var () =
      let next = !prev_non_rigid_id + 1 in
      if next >= rigid_var_start
      then invalid_arg "Ldd_hybrid_opt: exhausted non-rigid id range";
      prev_non_rigid_id := next;
      let v = { id = next; state = Unsolved; var_node = bot } in
      (* [var_node] is the node representing the variable itself:
         ⊥ ⊔ (v ⊓ ⊤). *)
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

  (* Subtract subsets hi - lo (co-Heyting subtraction).
     This preserves ordering and maintains the canonical form [hi = hi - lo]. *)
  (* Ordering fast path: if [lo] has a larger top var, it cannot appear
     under [hi], so we only recurse into [hi]'s subtrees. *)
  let rec canonicalize ~(hi : node) ~(lo : node) : node =
    if hi == lo
    then bot
    else if lo == bot
    then hi
    else if is_leaf hi
    then
      let dh = Unsafe.leaf_value hi in
      let dl = down0 lo in
      if Axis_lattice.equal dl Axis_lattice.bot
      then hi
      else leaf (Axis_lattice.co_sub dh dl)
    else if is_leaf lo
    then canonicalize_right_leaf ~hi ~lo
    else
      let hb = Unsafe.node_block hi in
      let lb = Unsafe.node_block lo in
      let cmp = compare_var hb.v lb.v in
      if cmp = 0
      then
        let lo' = canonicalize ~hi:hb.lo ~lo:lb.lo in
        let hi1 =
          if lb.hi == bot
          then hb.hi
          else canonicalize ~hi:hb.hi ~lo:lb.hi
        in
        let hi' =
          if lb.lo == bot
          then hi1
          else canonicalize ~hi:hi1 ~lo:lb.lo
        in
        if hb.lo == lo' && hb.hi == hi' then hi else node_raw hb.v lo' hi'
      else if cmp < 0
      then
        let lo' = canonicalize ~hi:hb.lo ~lo in
        let hi' = canonicalize ~hi:hb.hi ~lo in
        if hb.lo == lo' && hb.hi == hi' then hi else node_raw hb.v lo' hi'
      else canonicalize ~hi ~lo:lb.lo

  and canonicalize_right_leaf ~(hi : node) ~(lo : node) : node =
    let rec aux ~(hi : node) (leaf_l : Axis_lattice.t) : node =
      if is_leaf hi
      then leaf (Axis_lattice.co_sub (Unsafe.leaf_value hi) leaf_l)
      else
        let hb = Unsafe.node_block hi in
        let lo' = aux ~hi:hb.lo leaf_l in
        let hi' = aux ~hi:hb.hi leaf_l in
        if hb.lo == lo' && hb.hi == hi' then hi else node_raw hb.v lo' hi'
    in
    let leaf_val = Unsafe.leaf_value lo in
    if Axis_lattice.equal leaf_val Axis_lattice.bot
    then hi
    else aux ~hi leaf_val

  (* Build a canonical node; ensures [hi] is disjoint from [lo]. *)
  let node (v : var) (lo : node) (hi : node) : node =
    if lo == bot
    then node_raw v lo hi
    else node_raw v lo (canonicalize ~hi ~lo)

  (* --------- boolean algebra over nodes --------- *)
  (* Variable-order merge: [cmp] decides which side can be descended. *)
  let rec join (a : node) (b : node) =
    if a == b
    then a
    else if is_leaf a
    then
      let leaf_val = Unsafe.leaf_value a in
      join_with_leaf leaf_val b
    else if is_leaf b
    then
      let leaf_val = Unsafe.leaf_value b in
      join_with_leaf leaf_val a
    else
      let na = Unsafe.node_block a in
      let nb = Unsafe.node_block b in
      let cmp = compare_var na.v nb.v in
      if cmp = 0
      then
        node_raw na.v (join na.lo nb.lo)
          (join
             (canonicalize ~hi:na.hi ~lo:nb.lo)
             (canonicalize ~hi:nb.hi ~lo:na.lo))
      else if cmp < 0
      then node_raw na.v (join na.lo b) (canonicalize ~hi:na.hi ~lo:b)
      else node_raw nb.v (join a nb.lo) (canonicalize ~hi:nb.hi ~lo:a)

  and join_with_leaf (leaf_a : Axis_lattice.t) (other : node) =
    let rec aux (leaf_a : Axis_lattice.t) (other : node) =
      if is_leaf other
      then leaf (Axis_lattice.join leaf_a (Unsafe.leaf_value other))
      else
        let nb = Unsafe.node_block other in
        let lo' = aux leaf_a nb.lo in
        let hi' = canonicalize_right_leaf ~hi:nb.hi ~lo:(leaf leaf_a) in
        if lo' == nb.lo && hi' == nb.hi then other else node_raw nb.v lo' hi'
    in
    if Axis_lattice.equal leaf_a Axis_lattice.top
    then top
    else if (* Fast path: [down0] summarizes the lo-chain in order. *)
            Axis_lattice.leq leaf_a (down0 other)
    then other
    else aux leaf_a other

  (* Variable-order merge: [cmp] decides which side can be descended. *)
  let rec meet (a : node) (b : node) =
    if a == b
    then a
    else if is_leaf a
    then meet_with_leaf a b
    else if is_leaf b
    then meet_with_leaf b a
    else
      let na = Unsafe.node_block a in
      let nb = Unsafe.node_block b in
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
    let rec aux (leaf_a : Axis_lattice.t) (other : node) =
      if is_leaf other
      then leaf (Axis_lattice.meet leaf_a (Unsafe.leaf_value other))
      else
        let nb = Unsafe.node_block other in
        let lo' = aux leaf_a nb.lo in
        let hi' = aux leaf_a nb.hi in
        if lo' == nb.lo && hi' == nb.hi then other else node nb.v lo' hi'
    in
    let leaf_val = Unsafe.leaf_value leaf_a in
    if Axis_lattice.equal leaf_val Axis_lattice.top
    then other
    else if Axis_lattice.equal leaf_val Axis_lattice.bot
    then bot
    else aux leaf_val other

  (* --------- public constructors --------- *)
  let[@inline] const (c : Axis_lattice.t) = leaf c

  let[@inline] mk_var (v : var) : node = v.var_node

  let rigid (name : V.t) = Var.make_rigid ~name ()

  let new_var () = Var.make_var ()

  (* --------- assignments (x ← ⊥ / ⊤) --------- *)
  (** Assign variable to bottom [var := ⊥], simplifying the diagram. *)
  let rec assign_bot ~(var : var) (w : node) : node =
    if is_leaf w
    then w
    else
      let n = Unsafe.node_block w in
      let cmp = compare_var var n.v in
      if cmp < 0
      then
        (* Fast path: [var] < node var, so it cannot appear below. *)
        w
      else if cmp = 0
      then n.lo
      else
        let lo' = assign_bot ~var n.lo in
        let hi' = assign_bot ~var n.hi in
        if lo' == n.lo && hi' == n.hi then w else node n.v lo' hi'

  (** Assign variable to top [var := ⊤], simplifying the diagram. *)
  let rec assign_top ~(var : var) (w : node) : node =
    if is_leaf w
    then w
    else
      let n = Unsafe.node_block w in
      let cmp = compare_var var n.v in
      if cmp < 0
      then
        (* Fast path: [var] < node var, so it cannot appear below. *)
        w
      else if cmp = 0
      then join n.lo n.hi
      else
        let lo' = assign_top ~var n.lo in
        let hi' = assign_top ~var n.hi in
        if lo' == n.lo && hi' == n.hi then w else node n.v lo' hi'

  (* --------- inline solved vars ---------
     Inline solved variables by replacing them with their solutions.
     We do not descend under rigid vars (ids above [rigid_var_start]). *)
  let rec inline_solved_vars (w : node) : node =
    if is_leaf w
    then w
    else
      let n = Unsafe.node_block w in
      if n.v.id > Var.rigid_var_start
      then
        (* Fast path: rigid ids are above all non-rigids, so they cannot
           appear under non-rigid nodes. *)
        w
      else
        let lo' = inline_solved_vars n.lo in
        let hi' = inline_solved_vars n.hi in
        match n.v.state with
        | Solved d ->
          let d' = inline_solved_vars d in
          n.v.state <- Solved d';
          join lo' (meet hi' d')
        | Unsolved ->
          if lo' == n.lo && hi' == n.hi
          then w
          else
            let d' = mk_var n.v in
            join lo' (meet hi' d')
        | Rigid _ ->
          failwith "inline_solved_vars: rigid variable shouldn't be here"

  (* This function is equivalent to `assign_bot ~var (inline_solved_vars w)` *)
  let rec assign_bot_force ~(var : var) (w : node) : node =
    if var.id > Var.rigid_var_start
    then assign_bot ~var (inline_solved_vars w)
    else if is_leaf w
    then w
    else
      let n = Unsafe.node_block w in
      match n.v.state with
      | Solved d ->
        let lo' = assign_bot_force ~var n.lo in
        let hi' = assign_bot_force ~var n.hi in
        let d_forced = inline_solved_vars d in
        n.v.state <- Solved d_forced;
        let d' = assign_bot ~var d_forced in
        join lo' (meet hi' d')
      | Unsolved ->
        let lo' = assign_bot_force ~var n.lo in
        if compare_var n.v var = 0
        then lo'
        else
          let hi' = assign_bot_force ~var n.hi in
          if lo' == n.lo && hi' == n.hi
          then w
          else
            let d' = mk_var n.v in
            join lo' (meet hi' d')
      | Rigid _ -> w

  (* [sub_subsets a b] computes a - b. *)
  let sub_subsets (a : node) (b : node) =
    canonicalize ~hi:(inline_solved_vars a) ~lo:(inline_solved_vars b)

  let sub_subsets_forced (a : node) (b : node) : node =
    canonicalize ~hi:a ~lo:b

  (* --------- solve-on-install (no queues for LFP; GFP uses a stack)
       --------- *)
  let solve_lfp (var : var) (rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_lfp: rigid variable"
    | Solved _ -> invalid_arg "solve_lfp: solved variable"
    | Unsolved ->
      var.state <- Solved (assign_bot_force ~var rhs_raw)

  let solve_gfp (var : var) (rhs_raw : node) : unit =
    match var.state with
    | Rigid _ -> invalid_arg "solve_gfp: rigid variable"
    | Solved _ -> invalid_arg "solve_gfp: solved variable"
    | Unsolved ->
      let rhs_forced = inline_solved_vars rhs_raw in
      var.state <- Solved (assign_top ~var rhs_forced)

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

  (* Decompose into linear terms by successive cofactoring over [universe]. *)
  let decompose_into_linear_terms ~(universe : var list) (n : node) =
    let rec go vs m ns =
      match vs with
      | [] -> m, ns
      | v :: vs' ->
        go vs'
          (assign_bot ~var:v m)
          (assign_top ~var:v m
           :: List.map (fun node -> assign_bot ~var:v node) ns)
    in
    let base, linears = go universe (inline_solved_vars n) [] in
    base, List.rev linears

  let leq (a : node) (b : node) =
    solve_pending ();
    let a = inline_solved_vars a in
    let b = inline_solved_vars b in
    is_bot_node (sub_subsets_forced a b)

  let rec round_up' (n : node) =
    if is_leaf n
    then Unsafe.leaf_value n
    else
      let nb = Unsafe.node_block n in
      let lo' = round_up' nb.lo in
      let hi' = round_up' nb.hi in
      Axis_lattice.join lo' hi'

  let round_up (n : node) =
    solve_pending ();
    let n = inline_solved_vars n in
    round_up' n

  let is_const (n : node) : bool =
    let n = inline_solved_vars n in
    is_leaf n

  (* --------- polynomial-style pretty printer --------- *)
  let to_named_terms_with (pp_unsolved : var -> string) (w : node) :
      (Axis_lattice.t * string list) list =
    let rec aux (acc_vars : string list) (w : node)
        (acc_terms : (Axis_lattice.t * string list) list) =
      if is_leaf w
      then
        let c = Unsafe.leaf_value w in
        if Axis_lattice.equal c Axis_lattice.bot
        then acc_terms
        else (c, acc_vars) :: acc_terms
      else
        let n = Unsafe.node_block w in
        let acc_terms = aux acc_vars n.lo acc_terms in
        let acc_hi =
          match n.v.state with
          | Rigid name -> V.to_string name :: acc_vars
          | Unsolved -> pp_unsolved n.v :: acc_vars
          | Solved _ ->
            failwith "solved vars should not appear after inline_solved_vars"
        in
        aux acc_hi n.hi acc_terms
    in
    aux [] (inline_solved_vars w) [] |> List.rev

  let to_named_terms (w : node) : (Axis_lattice.t * string list) list =
    to_named_terms_with
      (fun v ->
        match v.state with
        | Rigid name -> V.to_string name
        | Unsolved -> "<unsolved-var:" ^ string_of_int v.id ^ ">"
        | Solved _ ->
          failwith "solved vars should not appear after inline_solved_vars")
      w

  let pp (w : node) : string =
    let pp_coeff = Axis_lattice.to_string in
    (* Aggregate duplicate rigid var-sets by join on coefficients. *)
    let tbl : (string list, Axis_lattice.t) Hashtbl.t = Hashtbl.create 16 in
    let add_entry (c, names) =
      let vs = List.sort String.compare names in
      match Hashtbl.find_opt tbl vs with
      | None -> Hashtbl.add tbl vs c
      | Some prev -> Hashtbl.replace tbl vs (Axis_lattice.join prev c)
    in
    List.iter add_entry (to_named_terms w);
    let terms =
      Hashtbl.fold
        (fun vs c acc ->
          if Axis_lattice.equal c Axis_lattice.bot
          then acc
          else (vs, c) :: acc)
        tbl []
    in
    if terms = []
    then "⊥"
    else
      let term_body vs c =
        let is_top = Axis_lattice.equal c Axis_lattice.top in
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

  (* Empty list means [a ⊑ b] succeeds.
     Non-empty list is the witness axes where it fails. *)
  let leq_with_reason (a : node) (b : node) : int list =
    solve_pending ();
    let a = inline_solved_vars a in
    let b = inline_solved_vars b in
    let diff = sub_subsets_forced a b in
    let witness = round_up' diff in
    Axis_lattice.non_bot_axes witness

  let map_rigid (f : V.t -> node) (n : node) : node =
    let rec aux (n : node) : node =
      if is_leaf n
      then n
      else
        let nb = Unsafe.node_block n in
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
            (* One might think we can directly construct a node here,
               but that would break our invariants if the `f` function
               inserted arbitrary variables in lo' and hi'. *)
            join lo' (meet hi' var_node)
        | Solved _ ->
          invalid_arg
            "map_rigid: solved vars should not appear after inline_solved_vars"
    in
    aux (inline_solved_vars n)

  (* --------- structural debug printer --------- *)
  let pp_debug (w : node) : string =
    let pp_coeff = Axis_lattice.to_string in
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
               (pp_coeff (Unsafe.leaf_value n)))
        else
          let nb = Unsafe.node_block n in
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
