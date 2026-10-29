[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare

(** The implementation below is based on the following article:
    {v
    A Simple, Fast Dominance Algorithm
    Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
    v} *)

module List = ListLabels

let fatal = Misc.fatal_errorf

let debug = false

type doms = Label.t Label.Tbl.t

type dominance_frontiers = Label.Set.t Label.Tbl.t

type dominator_tree =
  { label : Label.t;
    children : dominator_tree list
  }

type t =
  { doms : doms;
    dominance_frontiers : dominance_frontiers;
    dominator_tree : dominator_tree
  }

let rec is_dominating (doms : doms) left right =
  if Label.equal left right
  then true
  else
    let parent = Label.Tbl.find doms right in
    if Label.equal right parent then false else is_dominating doms left parent

let is_strictly_dominating (doms : doms) left right =
  (not (Label.equal left right)) && is_dominating doms left right

let invariant_doms : Cfg.t -> doms -> unit =
 fun cfg doms ->
  (* Check that `doms` has the same keys as the CFG. *)
  if Label.Tbl.length cfg.blocks <> Label.Tbl.length doms
  then fatal "Cfg_dominators.invariant_doms: invalid doms length";
  Label.Tbl.iter
    (fun key _ ->
      if not (Label.Tbl.mem doms key)
      then
        fatal "Cfg_dominators.invariant_doms: missing label %a" Label.format key)
    cfg.blocks;
  (* Check that (i) the immediate dominator is a strict dominator, and (ii)
     there is no other intermediate strict dominator - except for the entry
     block, which is its own immediate dominator by convention. *)
  Label.Tbl.iter
    (fun n idom_n ->
      if Label.equal n cfg.entry_label
      then (
        if not (Label.equal n idom_n)
        then
          fatal
            "Cfg_dominators.invariant_doms: entry is not its own immediate \
             dominator")
      else (
        if not (is_strictly_dominating doms idom_n n)
        then
          fatal
            "Cfg_dominators.invariant_doms: the immediate dominator of %a, %a, \
             is not strictly dominating it"
            Label.format n Label.format idom_n;
        Label.Tbl.iter
          (fun m _ ->
            if
              is_strictly_dominating doms idom_n m
              && is_strictly_dominating doms m n
            then
              fatal
                "Cfg_dominators.invariant_doms: there is a strict dominator, \
                 %a, between %a and its immediate dominator, %a"
                Label.format m Label.format n Label.format idom_n)
          doms))
    doms

type post_order_stack = Cfg.basic_block Stack.t

let build_reverse_post_order : Cfg.t -> post_order_stack =
 fun cfg ->
  let stack = Stack.create () in
  let visited = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let rec visit label =
    if not (Label.Tbl.mem visited label)
    then (
      Label.Tbl.add visited label ();
      let block = Cfg.get_block_exn cfg label in
      Label.Set.iter visit (Cfg.successor_labels ~normal:true ~exn:true block);
      Stack.push block stack)
  in
  visit cfg.entry_label;
  stack

let iter_stack : post_order_stack -> f:(Cfg.basic_block -> unit) -> unit =
 fun post_order_stack ~f ->
  let stack = Stack.copy post_order_stack in
  while not (Stack.is_empty stack) do
    let block : Cfg.basic_block = Stack.pop stack in
    f block
  done

type order = int Label.Tbl.t

let build_order : Cfg.t -> post_order_stack * order =
 fun cfg ->
  let order = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let stack = build_reverse_post_order cfg in
  iter_stack stack ~f:(fun (block : Cfg.basic_block) ->
      let label = block.start in
      Label.Tbl.replace order label (Label.Tbl.length order));
  stack, order

(* See Figure 3 in the cited article. The only difference is the comparison,
   which is reversed because of the way we distribute the identifiers when we
   build [post_order]. *)
let intersect : doms -> order -> Label.t -> Label.t -> Label.t =
 fun doms post_order b1 b2 ->
  let finger1 = ref b1 in
  let finger2 = ref b2 in
  let before_in_post_order (left : Label.t) (right : Label.t) =
    Label.Tbl.find post_order left > Label.Tbl.find post_order right
  in
  while not (Label.equal !finger1 !finger2) do
    while before_in_post_order !finger1 !finger2 do
      match Label.Tbl.find_opt doms !finger1 with
      | None -> assert false
      | Some f ->
        assert (not (Label.equal f !finger1));
        finger1 := f
    done;
    while before_in_post_order !finger2 !finger1 do
      match Label.Tbl.find_opt doms !finger2 with
      | None -> assert false
      | Some f ->
        assert (not (Label.equal f !finger2));
        finger2 := f
    done
  done;
  !finger1

(* See Figure 3 in the cited article. "Undefined" is encoded here as a missing
   key. *)
let compute_doms : Cfg.t -> doms =
 fun cfg ->
  let doms = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  (* The entry is its own immediate dominator, even with incoming self tail
     calls. All other blocks are reachable from it. *)
  Label.Tbl.add doms cfg.entry_label cfg.entry_label;
  let stack, order = build_order cfg in
  let changed = ref true in
  while !changed do
    changed := false;
    iter_stack stack ~f:(fun (block : Cfg.basic_block) ->
        let label = block.start in
        if not (Label.equal label cfg.entry_label)
        then (
          let new_idom = ref None in
          let predecessor_labels = Cfg.predecessor_labels block in
          List.iter predecessor_labels ~f:(fun predecessor_label ->
              if Label.Tbl.mem doms predecessor_label
              then
                new_idom
                  := Some
                       (match !new_idom with
                       | None -> predecessor_label
                       | Some new_idom_pred ->
                         intersect doms order predecessor_label new_idom_pred));
          let new_idom =
            match !new_idom with
            | None ->
              Misc.fatal_errorf "no new idom for label %a" Label.format label
            | Some new_idom -> new_idom
          in
          match Label.Tbl.find_opt doms label with
          | None ->
            Label.Tbl.replace doms label new_idom;
            changed := true
          | Some dom_label ->
            if not (Label.equal dom_label new_idom)
            then (
              Label.Tbl.replace doms label new_idom;
              changed := true)))
  done;
  if debug then invariant_doms cfg doms;
  doms

let invariant_dominance_frontiers : Cfg.t -> doms -> dominance_frontiers -> unit
    =
 fun cfg doms dominance_frontiers ->
  Label.Tbl.iter
    (fun label frontier_labels ->
      Label.Set.iter
        (fun frontier_label ->
          if is_strictly_dominating doms label frontier_label
          then
            fatal
              "Cfg_dominators.invariant_dominance_frontiers: %a is strictly \
               dominating %a"
              Label.format label Label.format frontier_label;
          let block = Cfg.get_block_exn cfg frontier_label in
          let dominates_a_predecessor =
            Label.Set.exists
              (fun predecessor -> is_dominating doms label predecessor)
              block.predecessors
          in
          if not dominates_a_predecessor
          then
            fatal
              "Cfg_dominators.invariant_dominance_frontiers: %a does not \
               dominate any predecessor of %a"
              Label.format label Label.format frontier_label)
        frontier_labels)
    dominance_frontiers

(* See Figure 5 in the cited article. Incoming edges to the entry are treated as
   joins with the implicit function-entry edge. Since the entry is its own
   immediate dominator by convention, add it to its own frontier separately. *)
let compute_dominance_frontiers (cfg : Cfg.t) (doms : doms) :
    dominance_frontiers =
  let res = Label.Tbl.create (Label.Tbl.length doms) in
  Label.Tbl.iter
    (fun label _idom -> Label.Tbl.replace res label Label.Set.empty)
    doms;
  Label.Tbl.iter
    (fun label idom ->
      let block = Cfg.get_block_exn cfg label in
      let predecessor_labels = Cfg.predecessor_labels block in
      let is_entry = Label.equal label cfg.entry_label in
      match predecessor_labels with
      | [] -> ()
      | [_] when not is_entry -> ()
      | _ ->
        List.iter predecessor_labels ~f:(fun predecessor_label ->
            let runner = ref predecessor_label in
            while not (Label.equal !runner idom) do
              let curr = Label.Tbl.find res !runner in
              Label.Tbl.replace res !runner (Label.Set.add label curr);
              runner := Label.Tbl.find doms !runner
            done);
        if is_entry
        then
          Label.Tbl.replace res label
            (Label.Set.add label (Label.Tbl.find res label)))
    doms;
  if debug then invariant_dominance_frontiers cfg doms res;
  res

let iter_breadth_dominator_tree : dominator_tree -> f:(Label.t -> unit) -> unit
    =
 fun dominator_tree ~f ->
  let queue = Queue.create () in
  Queue.add dominator_tree queue;
  while not (Queue.is_empty queue) do
    let dom_tree = Queue.take queue in
    f dom_tree.label;
    List.iter dom_tree.children ~f:(fun child -> Queue.add child queue)
  done

let invariant_dominator_tree : Cfg.t -> doms -> dominator_tree -> unit =
 fun cfg doms dominator_tree ->
  let seen_by_iter = ref Label.Set.empty in
  iter_breadth_dominator_tree dominator_tree ~f:(fun label ->
      seen_by_iter := Label.Set.add label !seen_by_iter);
  let seen_by_cfg =
    Cfg.fold_blocks cfg ~init:Label.Set.empty
      ~f:(fun label _block seen_by_cfg -> Label.Set.add label seen_by_cfg)
  in
  if not (Label.Set.equal !seen_by_iter seen_by_cfg)
  then
    fatal
      "Cfg_dominators.invariant_dominator_tree: iterator did not see all blocks";
  if not (Label.equal dominator_tree.label cfg.entry_label)
  then fatal "Cfg_dominators.invariant_dominator_tree: root is not the entry";
  let rec check_parent ~parent tree =
    let immediate_dominator = Label.Tbl.find doms tree.label in
    if not (Label.equal immediate_dominator parent)
    then
      fatal
        "Cfg_dominators.invariant_dominator_tree: unexpected parent %a for \
         label %a"
        Label.format parent Label.format tree.label;
    List.iter tree.children ~f:(fun child ->
        check_parent ~parent:tree.label child)
  in
  check_parent ~parent:cfg.entry_label dominator_tree

let compute_dominator_tree : Cfg.t -> doms -> dominator_tree =
 fun cfg doms ->
  let children = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let rec build_tree (label : Label.t) : dominator_tree =
    let children_labels =
      match Label.Tbl.find_opt children label with
      | None -> []
      | Some labels -> labels
    in
    { label; children = List.map children_labels ~f:build_tree }
  in
  Cfg.iter_blocks cfg ~f:(fun label _block ->
      if not (Label.equal label cfg.entry_label)
      then
        let immediate_dominator = Label.Tbl.find doms label in
        let current =
          match Label.Tbl.find_opt children immediate_dominator with
          | None -> []
          | Some children -> children
        in
        Label.Tbl.replace children immediate_dominator (label :: current));
  let res = build_tree cfg.entry_label in
  if debug then invariant_dominator_tree cfg doms res;
  res

let build : Cfg.t -> t =
 fun cfg ->
  let doms = compute_doms cfg in
  let dominance_frontiers = compute_dominance_frontiers cfg doms in
  let dominator_tree = compute_dominator_tree cfg doms in
  if !Oxcaml_flags.cfg_dominators_validate
  then
    Profile.record ~accumulate:true "validate_dominators"
      (Cfg_dominators_validate.validate_idom cfg)
      doms;
  { doms; dominance_frontiers; dominator_tree }

let is_dominating t left right = is_dominating t.doms left right

let is_strictly_dominating t left right =
  is_strictly_dominating t.doms left right

let find_dominance_frontier t label =
  match Label.Tbl.find_opt t.dominance_frontiers label with
  | Some frontier -> frontier
  | None ->
    fatal "Cfg_dominators.find_dominance_frontier: no frontier for label %a"
      Label.format label

let dominator_tree t = t.dominator_tree
