[@@@ocaml.warning "+a-40-41-42"]

module Datalog = Flambda2_datalog.Datalog

module Node : sig
  type t

  include Datalog.Column.S with type t := t

  val of_label : Label.t -> t
end = struct
  include Datalog.Column.Make (struct
    let name = "cfg_node"

    let print fmt node = Format.fprintf fmt "n%d" node
  end)

  let of_label = Label.to_int
end

module Node_relation = Datalog.Schema.Relation1 (Node)
module Node_node_relation = Datalog.Schema.Relation2 (Node) (Node)

let create_relation name columns = Datalog.create_relation ~name columns

let edge = create_relation "cfg_dominators.edge" Node_node_relation.columns

let is_node = create_relation "cfg_dominators.is_node" Node_relation.columns

let entry = create_relation "cfg_dominators.entry" Node_relation.columns

let reachable = create_relation "cfg_dominators.reachable" Node_relation.columns

let not_dom =
  create_relation "cfg_dominators.not_dom" Node_node_relation.columns

let dom = create_relation "cfg_dominators.dom" Node_node_relation.columns

let not_idom =
  create_relation "cfg_dominators.not_idom" Node_node_relation.columns

let idom = create_relation "cfg_dominators.idom" Node_node_relation.columns

let expected_idom =
  create_relation "cfg_dominators.expected_idom" Node_node_relation.columns

let reachable_entry_rule =
  Datalog.compile ["entry"] (fun [entry_node] ->
      Datalog.where
        [Datalog.atom entry [entry_node]]
        (Datalog.deduce (Datalog.atom reachable [entry_node])))

let reachable_edge_rule =
  Datalog.compile ["source"; "target"] (fun [source; target] ->
      Datalog.where
        [Datalog.atom reachable [source]; Datalog.atom edge [source; target]]
        (Datalog.deduce (Datalog.atom reachable [target])))

let not_dom_entry_rule =
  Datalog.compile ["candidate"; "entry"] (fun [candidate; entry_node] ->
      Datalog.where
        [ Datalog.atom is_node [candidate];
          Datalog.atom entry [entry_node];
          Datalog.distinct Node.datalog_column_id candidate entry_node ]
        (Datalog.deduce (Datalog.atom not_dom [entry_node; candidate])))

let not_dom_edge_rule =
  Datalog.compile ["source"; "target"; "candidate"]
    (fun [source; target; candidate] ->
      Datalog.where
        [ Datalog.atom edge [source; target];
          Datalog.atom not_dom [source; candidate];
          Datalog.distinct Node.datalog_column_id candidate target ]
        (Datalog.deduce (Datalog.atom not_dom [target; candidate])))

let dom_rule =
  Datalog.compile ["candidate"; "node"] (fun [candidate; node] ->
      Datalog.where
        [ Datalog.atom reachable [candidate];
          Datalog.atom reachable [node];
          Datalog.not (Datalog.atom not_dom [node; candidate]) ]
        (Datalog.deduce (Datalog.atom dom [node; candidate])))

let not_idom_rule =
  Datalog.compile ["node"; "candidate"; "between"]
    (fun [node; candidate; between] ->
      Datalog.where
        [ Datalog.atom dom [node; candidate];
          Datalog.atom dom [node; between];
          Datalog.atom dom [candidate; between];
          Datalog.distinct Node.datalog_column_id node candidate;
          Datalog.distinct Node.datalog_column_id candidate between ]
        (Datalog.deduce (Datalog.atom not_idom [node; between])))

let idom_entry_rule =
  Datalog.compile ["entry"] (fun [entry_node] ->
      Datalog.where
        [Datalog.atom entry [entry_node]]
        (Datalog.deduce (Datalog.atom idom [entry_node; entry_node])))

let idom_rule =
  Datalog.compile ["node"; "candidate"] (fun [node; candidate] ->
      Datalog.where
        [ Datalog.atom dom [node; candidate];
          Datalog.distinct Node.datalog_column_id node candidate;
          Datalog.not (Datalog.atom not_idom [node; candidate]) ]
        (Datalog.deduce (Datalog.atom idom [node; candidate])))

let reachable_schedule =
  Datalog.Schedule.saturate [reachable_entry_rule; reachable_edge_rule]

let not_dom_schedule =
  Datalog.Schedule.saturate [not_dom_entry_rule; not_dom_edge_rule]

let dom_schedule = Datalog.Schedule.saturate [dom_rule]

let not_idom_schedule = Datalog.Schedule.saturate [not_idom_rule]

let idom_schedule = Datalog.Schedule.saturate [idom_entry_rule; idom_rule]

module Internal = struct
  let validate (facts : Cfg_validation_facts.Graph.t) expected =
    let node = Node.of_label in
    let node_facts =
      List.fold_left
        (fun relation label ->
          Node_relation.add_or_replace [node label] () relation)
        Node_relation.empty facts.Cfg_validation_facts.Graph.nodes
    in
    let edge_facts =
      List.fold_left
        (fun relation (source, target) ->
          Node_node_relation.add_or_replace
            [node source; node target]
            () relation)
        Node_node_relation.empty facts.edges
    in
    let expected_facts =
      Label.Tbl.fold
        (fun label immediate_dominator relation ->
          Node_node_relation.add_or_replace
            [node label; node immediate_dominator]
            () relation)
        expected Node_node_relation.empty
    in
    let db =
      Datalog.empty
      |> Datalog.set_table edge edge_facts
      |> Datalog.set_table is_node node_facts
      |> Datalog.set_table entry (Node_relation.singleton [node facts.entry] ())
      |> Datalog.set_table expected_idom expected_facts
    in
    let db = Datalog.Schedule.run reachable_schedule db in
    let db = Datalog.Schedule.run not_dom_schedule db in
    let db = Datalog.Schedule.run dom_schedule db in
    let db = Datalog.Schedule.run not_idom_schedule db in
    let db = Datalog.Schedule.run idom_schedule db in
    let actual = Datalog.get_table idom db in
    let validation_succeeded =
      Node.Map.equal (Node.Map.equal (fun () () -> true)) expected_facts actual
    in
    if validation_succeeded
    then Ok ()
    else Error "expected_idom and idom differ"
end

module Z3 = struct
  let code (facts : Cfg_validation_facts.Graph.t) expected =
    let cfg_nodes = facts.Cfg_validation_facts.Graph.nodes in
    let max_id =
      List.fold_left
        (fun max_id label -> Int.max max_id (Label.to_int label))
        0 cfg_nodes
    in
    let width =
      match max_id with 0 | 1 -> 1 | max_id -> 1 + Misc.log2 max_id
    in
    let id label = Printf.sprintf "(_ bv%d %d)" (Label.to_int label) width in
    let buffer = Buffer.create 4096 in
    let fmt = Format.formatter_of_buffer buffer in
    Format.fprintf fmt
      {|
(set-option :fp.engine datalog)
(define-sort node () (_ BitVec %d))
(declare-rel edge (node node))
(declare-rel is-node (node))
(declare-rel entry (node))
(declare-rel reachable (node))
(declare-rel not-dom (node node))
(declare-rel dom (node node))
(declare-rel not-idom (node node))
(declare-rel idom (node node))
(declare-rel expected-idom (node node))
(declare-rel bad ())
(declare-var a node)
(declare-var b node)
(declare-var c node)
(rule (=> (entry a) (reachable a)))
(rule (=> (and (reachable a) (edge a b)) (reachable b)))
(rule (=> (and (is-node a) (entry b) (distinct a b)) (not-dom b a)))
(rule (=> (and (edge b c) (not-dom b a) (distinct a c)) (not-dom c a)))
(rule (=> (and (reachable a) (reachable b) (not (not-dom b a))) (dom b a)))
(rule (=> (and (dom a b) (dom a c) (dom b c) (distinct a b) (distinct b c))
          (not-idom a c)))
(rule (=> (entry a) (idom a a)))
(rule (=> (and (dom a b) (distinct a b) (not (not-idom a b))) (idom a b)))
(rule (=> (and (idom a b) (not (expected-idom a b))) bad))
(rule (=> (and (expected-idom a b) (not (idom a b))) bad))
|}
      width;
    Cfg_z3.fmt_fact fmt "entry" [id facts.entry];
    List.iter (fun label -> Cfg_z3.fmt_fact fmt "is-node" [id label]) cfg_nodes;
    List.iter
      (fun (source, target) ->
        Cfg_z3.fmt_fact fmt "edge" [id source; id target])
      facts.edges;
    Label.Tbl.iter
      (fun label immediate_dominator ->
        Cfg_z3.fmt_fact fmt "expected-idom" [id label; id immediate_dominator])
      expected;
    Format.pp_print_string fmt "(query bad)";
    Format.pp_print_flush fmt ();
    Buffer.contents buffer
end

let fallback cfg internal_failure z3_code =
  let z3_result = Cfg_z3.run_validation_fallback z3_code in
  Misc.fatal_errorf
    "validate_idom: internal Datalog failed for CFG %S.@.%s@.%s@.Z3 \
     reproducer:@.%s"
    cfg.Cfg.fun_name internal_failure z3_result z3_code

let validate_idom (cfg : Cfg.t) expected =
  (* All CFG blocks must be reachable from the entry block. *)
  let facts = Cfg_validation_facts.Graph.create cfg in
  let z3_code () = Z3.code facts expected in
  match Internal.validate facts expected with
  | Ok () -> ()
  | Error error -> fallback cfg error (z3_code ())
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    let error =
      Format.sprintf "exception: %s@.%s" (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string backtrace)
    in
    fallback cfg error (z3_code ())
