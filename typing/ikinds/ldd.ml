(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Lattice-valued decision diagrams (LDDs), represented as a ZDD-style
   (zero-suppressed decision diagram) DAG ordered by [var] id.
   LDD = lattice-valued decision diagram; ZDD = zero-suppressed decision
   diagram (see https://en.wikipedia.org/wiki/Zero-suppressed_decision_diagram).
   We do not hash-cons nodes; sharing comes from structure and recursive
   construction. We maintain canonical form [hi = hi - lo], so [hi] is
   disjoint from [lo]. *)

module type ORDERED = Ldd_intf.ORDERED

module Make (V : ORDERED) = struct
  module Name = V

  type node = Obj.t
  (* node ::= node_block | int (leaf) -- we use Obj magic to unbox leaves *)

  (* [node_block] represents [lo ⊔ (v ⊓ hi)].
     Invariant: vars in [lo] and [hi] have strictly larger ids than [v]. *)
  type node_block =
    { v : var;
      lo : node;
      hi : node;
      down0 : Axis_lattice.t
      (** Cached lattice value of the lo->lo->..->lo leaf. *)
    }

  and var =
      { id : int;
      mutable state : var_state;
      mutable var_node : node
      (** [var_node] is the node representing just this var
          (⊥ ⊔ (v ⊓ ⊤)). *)
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
  let[@inline] is_leaf (node : node) : bool = Obj.is_int node

  let _assert_axis_lattice_is_int (x : Axis_lattice.t) = (x :> int)

  module Unsafe = struct
    let[@inline] leaf_value (node : node) : Axis_lattice.t = Obj.obj node

    let[@inline] node_block (node : node) : node_block = Obj.obj node

    let[@inline] node_down0 (node : node) : Axis_lattice.t =
      (node_block node).down0
  end

  let[@inline] make_node (v : var) (lo : node) (hi : node) : node =
    let down0 =
      if is_leaf lo then Unsafe.leaf_value lo else Unsafe.node_down0 lo
    in
    Obj.repr ({ v; lo; hi; down0 } : node_block)

  let[@inline] leaf (c : Axis_lattice.t) : node = Obj.repr c

  let bot = leaf Axis_lattice.bot

  let top = leaf Axis_lattice.top

  let[@inline] is_bot_node (node : node) : bool = node == bot

  let[@inline] down0 (node : node) : Axis_lattice.t =
    if is_leaf node
    then Unsafe.leaf_value node
    else Unsafe.node_down0 node

  (* Construct a node; must be in canonical form: hi = hi - lo. *)
  let node_raw (v : var) (lo : node) (hi : node) : node =
    if is_bot_node hi then lo else make_node v lo hi

  module Var = struct
    type t = var

    (* Id scheme: non-rigid vars are numbered sequentially. Rigid vars get
       a stable hash-based id so they are consistent across compilation
       units; collisions are handled by storing a bucket and comparing
       names. *)
    let prev_non_rigid_id = ref (-1)

    let rigid_tbl : (int, t list) Hashtbl.t = Hashtbl.create 97

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
      then invalid_arg "Ldd: exhausted non-rigid id range";
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

  (** Subtract subsets hi - lo (co-Heyting subtraction).
      This preserves ordering and maintains canonical form
      [hi = hi - lo]. *)
  let rec canonicalize ~(hi : node) ~(lo : node) : node =
    (* Ordering fast path: if [lo] has a larger top var, it cannot appear
       under [hi], so we only recurse into [hi]'s subtrees. *)
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
      let hi_block = Unsafe.node_block hi in
      let lo_block = Unsafe.node_block lo in
      let order = compare_var hi_block.v lo_block.v in
      if order = 0
      then
        let lo' = canonicalize ~hi:hi_block.lo ~lo:lo_block.lo in
        let hi1 =
          if lo_block.hi == bot
          then hi_block.hi
          else canonicalize ~hi:hi_block.hi ~lo:lo_block.hi
        in
        let hi' =
          if lo_block.lo == bot
          then hi1
          else canonicalize ~hi:hi1 ~lo:lo_block.lo
        in
        if hi_block.lo == lo' && hi_block.hi == hi'
        then hi
        else node_raw hi_block.v lo' hi'
      else if order < 0
      then
        let lo' = canonicalize ~hi:hi_block.lo ~lo in
        let hi' = canonicalize ~hi:hi_block.hi ~lo in
        if hi_block.lo == lo' && hi_block.hi == hi'
        then hi
        else node_raw hi_block.v lo' hi'
      else canonicalize ~hi ~lo:lo_block.lo

  and canonicalize_right_leaf ~(hi : node) ~(lo : node) : node =
    let rec aux ~(hi : node) (leaf_l : Axis_lattice.t) : node =
      if is_leaf hi
      then leaf (Axis_lattice.co_sub (Unsafe.leaf_value hi) leaf_l)
      else
        let hi_block = Unsafe.node_block hi in
        let lo' = aux ~hi:hi_block.lo leaf_l in
        let hi' = aux ~hi:hi_block.hi leaf_l in
        if hi_block.lo == lo' && hi_block.hi == hi'
        then hi
        else node_raw hi_block.v lo' hi'
    in
    let leaf_val = Unsafe.leaf_value lo in
    if Axis_lattice.equal leaf_val Axis_lattice.bot
    then hi
    else aux ~hi leaf_val

  (** Build a canonical node; ensures [hi] is disjoint from [lo]. *)
  let node (v : var) ~(lo : node) ~(hi : node) : node =
    if lo == bot
    then node_raw v lo hi
    else node_raw v lo (canonicalize ~hi ~lo)

  (* --------- boolean algebra over nodes --------- *)
  let rec join (a : node) (b : node) =
    (* Variable-order merge: [order] decides which side can be descended.
       This is similar to merge on sorted lists as we go down one path of
       the tree, but unlike merge on lists, we traverse all paths down the
       tree.
    *)
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
      let a_block = Unsafe.node_block a in
      let b_block = Unsafe.node_block b in
      let order = compare_var a_block.v b_block.v in
      if order = 0
      then
        node_raw a_block.v (join a_block.lo b_block.lo)
          (join
             (canonicalize ~hi:a_block.hi ~lo:b_block.lo)
             (canonicalize ~hi:b_block.hi ~lo:a_block.lo))
      else if order < 0
      then
        node_raw a_block.v (join a_block.lo b)
          (canonicalize ~hi:a_block.hi ~lo:b)
      else
        node_raw b_block.v (join a b_block.lo)
          (canonicalize ~hi:b_block.hi ~lo:a)

  and join_with_leaf (leaf_value : Axis_lattice.t) (node : node) =
    let rec aux (leaf_value : Axis_lattice.t) (node : node) =
      if is_leaf node
      then leaf (Axis_lattice.join leaf_value (Unsafe.leaf_value node))
      else
        let block = Unsafe.node_block node in
        let lo' = aux leaf_value block.lo in
        let hi' =
          canonicalize_right_leaf ~hi:block.hi ~lo:(leaf leaf_value)
        in
        if lo' == block.lo && hi' == block.hi
        then node
        else node_raw block.v lo' hi'
    in
    if Axis_lattice.equal leaf_value Axis_lattice.top
    then top
    else if (* Fast path: [down0] summarizes the lo-chain in order. *)
            Axis_lattice.leq leaf_value (down0 node)
    then node
    else aux leaf_value node

  let rec meet (a : node) (b : node) =
    (* Variable-order merge: [order] decides which side can be descended. *)
    if a == b
    then a
    else if is_leaf a
    then meet_with_leaf a b
    else if is_leaf b
    then meet_with_leaf b a
    else
      let a_block = Unsafe.node_block a in
      let b_block = Unsafe.node_block b in
      let order = compare_var a_block.v b_block.v in
      if order = 0
      then
        let lo = meet a_block.lo b_block.lo in
        let hi = meet (join a_block.hi a_block.lo)
                   (join b_block.hi b_block.lo) in
        node a_block.v ~lo ~hi
      else if order < 0
      then
        node a_block.v ~lo:(meet a_block.lo b) ~hi:(meet a_block.hi b)
      else
        node b_block.v ~lo:(meet a b_block.lo) ~hi:(meet a b_block.hi)

  and meet_with_leaf (leaf_node : node) (other : node) =
    let rec aux (leaf_value : Axis_lattice.t) (other : node) =
      if is_leaf other
      then leaf (Axis_lattice.meet leaf_value (Unsafe.leaf_value other))
      else
        let block = Unsafe.node_block other in
        let lo' = aux leaf_value block.lo in
        let hi' = aux leaf_value block.hi in
        if lo' == block.lo && hi' == block.hi
        then other
        else node block.v ~lo:lo' ~hi:hi'
    in
    let leaf_value = Unsafe.leaf_value leaf_node in
    if Axis_lattice.equal leaf_value Axis_lattice.top
    then other
    else if Axis_lattice.equal leaf_value Axis_lattice.bot
    then bot
    else aux leaf_value other

  (* --------- public constructors --------- *)
  let[@inline] const (c : Axis_lattice.t) = leaf c

  let[@inline] node_of_var (v : var) : node = v.var_node

  let rigid (name : V.t) = Var.make_rigid ~name ()

  let new_var () = Var.make_var ()

  (* --------- assignments (x ← ⊥ / ⊤) --------- *)
  (** Assign variable to bottom [var := ⊥]. *)
  let rec assign_bot ~(var : var) (node0 : node) : node =
    if is_leaf node0
    then node0
    else
      let block = Unsafe.node_block node0 in
      let order = compare_var var block.v in
      if order < 0
      then
        (* Fast path: [var] < node var, so it cannot appear below. *)
        node0
      else if order = 0
      then block.lo
      else
        let lo' = assign_bot ~var block.lo in
        let hi' = assign_bot ~var block.hi in
        if lo' == block.lo && hi' == block.hi
        then node0
        else node block.v ~lo:lo' ~hi:hi'

  (** Assign variable to top [var := ⊤]. *)
  let rec assign_top ~(var : var) (node0 : node) : node =
    if is_leaf node0
    then node0
    else
      let block = Unsafe.node_block node0 in
      let order = compare_var var block.v in
      if order < 0
      then
        (* Fast path: [var] < node var, so it cannot appear below. *)
        node0
      else if order = 0
      then join block.lo block.hi
      else
        let lo' = assign_top ~var block.lo in
        let hi' = assign_top ~var block.hi in
        if lo' == block.lo && hi' == block.hi
        then node0
        else node block.v ~lo:lo' ~hi:hi'

  (* --------- inline solved vars --------- *)
  (** Inline solved variables by replacing them with their solutions. *)
  let rec inline_solved_vars (node : node) : node =
    (* Do not descend under rigid vars. *)
    if is_leaf node
    then node
    else
      let block = Unsafe.node_block node in
      match block.v.state with
      | Rigid _ ->
        (* Fast path: rigid vars are above all non-rigids, so they cannot
           appear under non-rigid nodes. *)
        node
      | Solved d ->
        let lo' = inline_solved_vars block.lo in
        let hi' = inline_solved_vars block.hi in
        let d' = inline_solved_vars d in
        block.v.state <- Solved d';
        join lo' (meet hi' d')
      | Unsolved ->
        let lo' = inline_solved_vars block.lo in
        let hi' = inline_solved_vars block.hi in
        if lo' == block.lo && hi' == block.hi
        then node
        else
          let d' = node_of_var block.v in
          join lo' (meet hi' d')

  (** [assign_bot_inline ~var w] is equivalent to
      [assign_bot ~var (inline_solved_vars w)]. *)
  let rec assign_bot_inline ~(var : var) (node : node) : node =
    if var.id > Var.rigid_var_start
    then assign_bot ~var (inline_solved_vars node)
    else if is_leaf node
    then node
    else
      let block = Unsafe.node_block node in
      match block.v.state with
      | Solved d ->
        let lo' = assign_bot_inline ~var block.lo in
        let hi' = assign_bot_inline ~var block.hi in
        let d_forced = inline_solved_vars d in
        block.v.state <- Solved d_forced;
        let d' = assign_bot ~var d_forced in
        join lo' (meet hi' d')
      | Unsolved ->
        let lo' = assign_bot_inline ~var block.lo in
        if compare_var block.v var = 0
        then lo'
        else
          let hi' = assign_bot_inline ~var block.hi in
          if lo' == block.lo && hi' == block.hi
          then node
          else
            let d' = node_of_var block.v in
            join lo' (meet hi' d')
      | Rigid _ -> node

  let sub_subsets (a : node) (b : node) : node =
    canonicalize ~hi:(inline_solved_vars a) ~lo:(inline_solved_vars b)


  let solve_lfp (var : var) (rhs_raw : node) : unit =
    (* Solve the least fixpoint equation var := rhs_raw. 
       The rhs in general contains the var itself, and the
       fixpoint solution is var := rhs_raw[var := bot].
       We must also inline solved vars, because var itself
       may occur in a solution of an earlier solved var.
    *)
    match var.state with
    | Rigid _ -> invalid_arg "solve_lfp: rigid variable"
    | Solved _ -> invalid_arg "solve_lfp: solved variable"
    | Unsolved ->
      (* For efficiency, we use assign_bot_inline to 
         simultaneously inline solved vars and assign bot. *)
      var.state <- Solved (assign_bot_inline ~var rhs_raw)

  let solve_gfp (var : var) (rhs_raw : node) : unit =
    (* Solve the greatest fixpoint equation var := rhs_raw. 
       The rhs in general contains the var itself, and the
       fixpoint solution is var := rhs_raw[var := top].
       We must also inline solved vars, because var itself
       may occur in a solution of an earlier solved var.
    *)
    match var.state with
    | Rigid _ -> invalid_arg "solve_gfp: rigid variable"
    | Solved _ -> invalid_arg "solve_gfp: solved variable"
    | Unsolved ->
      (* gfp's are less performance critical, so we use two 
         separate steps to inline solved vars and assign top. *)
      let rhs_forced = inline_solved_vars rhs_raw in
      var.state <- Solved (assign_top ~var rhs_forced)

  let gfp_pending : (var * node) list ref = ref []

  let enqueue_gfp (var : var) (rhs_raw : node) : unit =
    gfp_pending := (var, rhs_raw) :: !gfp_pending

  let solve_pending_gfps () : unit =
    let pending = !gfp_pending in
    gfp_pending := [];
    List.iter (fun (var, rhs_raw) -> solve_gfp var rhs_raw) pending

  let solve_pending () : unit =
    solve_pending_gfps ()

  (** Decompose into linear terms over [universe]. *)
  let decompose_into_linear_terms ~(universe : var list) (n : node) =
    (* Successive restriction over [universe]. *)
    let rec go vars node coeffs =
      match vars with
      | [] -> node, coeffs
      | v :: rest ->
        let node_bot = assign_bot ~var:v node in
        let coeffs =
          assign_top ~var:v node
          :: List.map (fun coeff -> assign_bot ~var:v coeff) coeffs
        in
        go rest node_bot coeffs
    in
    let base, linears = go universe (inline_solved_vars n) [] in
    base, List.rev linears

  let rec round_up' (node : node) =
    if is_leaf node
    then Unsafe.leaf_value node
    else
      let block = Unsafe.node_block node in
      let lo = round_up' block.lo in
      let hi = round_up' block.hi in
      Axis_lattice.join lo hi

  let round_up (node : node) =
    solve_pending ();
    let node = inline_solved_vars node in
    round_up' node

  let is_const (node : node) : bool =
    let node = inline_solved_vars node in
    is_leaf node

  (* --------- polynomial-style pretty printer --------- *)
  let to_named_terms_with (pp_unsolved : var -> string) (node : node) :
      (Axis_lattice.t * string list) list =
    let rec aux (acc_vars : string list) (node : node)
        (acc_terms : (Axis_lattice.t * string list) list) =
      if is_leaf node
      then
        let c = Unsafe.leaf_value node in
        if Axis_lattice.equal c Axis_lattice.bot
        then acc_terms
        else (c, acc_vars) :: acc_terms
      else
        let block = Unsafe.node_block node in
        let acc_terms = aux acc_vars block.lo acc_terms in
        let acc_hi =
          match block.v.state with
          | Rigid name -> V.to_string name :: acc_vars
          | Unsolved -> pp_unsolved block.v :: acc_vars
          | Solved _ ->
            failwith "solved vars should not appear after inline_solved_vars"
        in
        aux acc_hi block.hi acc_terms
    in
    aux [] (inline_solved_vars node) [] |> List.rev

  let to_named_terms (node : node) : (Axis_lattice.t * string list) list =
    to_named_terms_with
      (fun v ->
        match v.state with
        | Rigid name -> V.to_string name
        | Unsolved -> "<unsolved-var:" ^ string_of_int v.id ^ ">"
        | Solved _ ->
          failwith "solved vars should not appear after inline_solved_vars")
      node

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
  let leq_with_reason (a : node) (b : node) :
      Jkind_axis.Axis.packed list =
    solve_pending ();
    let diff = sub_subsets a b in
    let witness = round_up' diff in
    Axis_lattice.non_bot_axes witness
    |> List.map Axis_lattice.axis_number_to_axis_packed

  let map_rigid (f : V.t -> node) (node : node) : node =
    let rec aux (node : node) : node =
      if is_leaf node
      then node
      else
        let block = Unsafe.node_block node in
        let var = block.v in
        let lo = block.lo in
        let hi = block.hi in
        match var.state with
        | Rigid name ->
          let lo' = aux lo in
          let hi' = aux hi in
          let replacement = f name in
          join lo' (meet hi' replacement)
        | Unsolved ->
          let lo' = aux lo in
          let hi' = aux hi in
          if lo' == lo && hi' == hi
          then node
          else
            let var_node = node_of_var var in
            (* One might think we can directly construct a node here,
               but that would break our invariants if the `f` function
               inserted arbitrary variables in lo' and hi'. *)
            join lo' (meet hi' var_node)
        | Solved _ ->
          invalid_arg
            "map_rigid: solved vars should not appear after inline_solved_vars"
    in
    aux (inline_solved_vars node)

  (* --------- structural debug printer --------- *)
  let pp_debug (node : node) : string =
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
    let rec go indent (node : node) : unit =
      let id = get_id node in
      if Hashtbl.mem printed id
      then Buffer.add_string b (Printf.sprintf "%s#%d = <ref>\n" indent id)
      else (
        Hashtbl.add printed id ();
        if is_leaf node
        then
          Buffer.add_string b
            (Printf.sprintf "%sLeaf#%d c=%s\n" indent id
               (pp_coeff (Unsafe.leaf_value node)))
        else
          let block = Unsafe.node_block node in
          Buffer.add_string b
            (Printf.sprintf "%sNode#%d %s down0=%s lo=#%d hi=#%d\n" indent id
               (pp_var_info block.v) (pp_coeff block.down0)
               (get_id block.lo) (get_id block.hi));
          let indent' = indent ^ "  " in
          go indent' block.lo;
          go indent' block.hi)
    in
    go "" node;
    Buffer.contents b
end
