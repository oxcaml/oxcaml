module Make () = struct
  type 'a root =
    { rank : int;
      value : 'a
    }

  type 'a t =
    { mutable node : 'a node;
      stamp : int
          (** See {!With_backtracking.Stamp}: changes to [node] are only logged
              if a snapshot was taken since [t] was created. *)
    }

  and 'a node =
    | Root of 'a root
    | Inner of 'a t

  type 'a union_find = 'a t

  module Change = struct
    type t = T : 'a union_find * 'a node -> t

    let undo (T (t, node)) = t.node <- node
  end

  include With_backtracking.Make (Change)

  let create value =
    { node = Root { rank = 0; value };
      stamp = With_backtracking.Stamp.fresh ()
    }

  let node t = t.node

  let set_node t node' =
    if With_backtracking.Stamp.needs_log t.stamp then log (T (t, node t));
    t.node <- node'

  (** [compress t ~imm_desc ~imm_desc_node ~prop_descs] compresses the path from
      [t] upwards to the root of [t]'s tree, where:
      - [imm_desc] is the child of [t] on the path, and [imm_desc_node] is its
        node ([Inner t]);
      - [prop_descs] are the nodes further down the path, below [imm_desc].

      Once the root is found, every node in [prop_descs] is re-pointed at the
      root's child's node, reusing it rather than allocating a new [Inner]. *)
  let rec compress t ~imm_desc ~imm_desc_node ~prop_descs =
    match node t with
    | Root r ->
      (* Perform path compression *)
      List.iter (fun t -> set_node t imm_desc_node) prop_descs;
      t, r
    | Inner t' as imm_desc_node ->
      compress t' ~imm_desc:t ~imm_desc_node ~prop_descs:(imm_desc :: prop_descs)

  let repr t =
    match node t with
    | Root r -> t, r
    | Inner t' as imm_desc_node ->
      compress t' ~imm_desc:t ~imm_desc_node ~prop_descs:[]

  let root t = match node t with Root r -> r | _ -> snd (repr t)

  let get t = (root t).value

  let set t value =
    let t, root = repr t in
    set_node t (Root { root with value })

  let same t1 t2 = fst (repr t1) == fst (repr t2)

  let union t1 t2 =
    let t1, r1 = repr t1 in
    let t2, r2 = repr t2 in
    if t1 != t2
    then
      let n1 = r1.rank in
      let n2 = r2.rank in
      if n1 < n2
      then set_node t1 (Inner t2)
      else (
        set_node t2 (Inner t1);
        if n1 = n2 then set_node t1 (Root { r1 with rank = n1 + 1 }))
end
