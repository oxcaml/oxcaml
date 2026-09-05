[@@@ocaml.warning "+a-40-41-42"]

module Datalog = Flambda2_datalog.Datalog

module Node : sig
  type t

  include Datalog.Column.S with type t := t

  val of_label : Label.t -> t
end = struct
  include Datalog.Column.Make (struct
    let name = "cfg_reachability_node"

    let print fmt node = Format.fprintf fmt "n%d" node
  end)

  let of_label = Label.to_int
end

module Node_relation = Datalog.Schema.Relation1 (Node)
module Node_node_relation = Datalog.Schema.Relation2 (Node) (Node)

let create_relation name columns =
  (* Validation only compares final relations, so derivation provenance is
     unused. *)
  Datalog.create_relation ~provenance:false ~name columns

let edge = create_relation "cfg_reachability.edge" Node_node_relation.columns

let entry = create_relation "cfg_reachability.entry" Node_relation.columns

let reachable =
  create_relation "cfg_reachability.reachable" Node_relation.columns

let expected_reachable =
  create_relation "cfg_reachability.expected_reachable" Node_relation.columns

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

let schedule =
  Datalog.Schedule.saturate [reachable_entry_rule; reachable_edge_rule]

module Internal = struct
  let validate (facts : Cfg_validation_facts.Graph.t) =
    let node = Node.of_label in
    let expected_facts =
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
    let db =
      Datalog.empty
      |> Datalog.set_table edge edge_facts
      |> Datalog.set_table entry (Node_relation.singleton [node facts.entry] ())
      |> Datalog.set_table expected_reachable expected_facts
    in
    let db = Datalog.Schedule.run schedule db in
    let actual = Datalog.get_table reachable db in
    let validation_succeeded =
      Node.Map.equal (fun () () -> true) expected_facts actual
    in
    if validation_succeeded
    then Ok ()
    else Error "expected_reachable and reachable differ"
end

module Z3 = struct
  let code (facts : Cfg_validation_facts.Graph.t) =
    let nodes = facts.Cfg_validation_facts.Graph.nodes in
    let max_id =
      List.fold_left
        (fun max_id label -> Int.max max_id (Label.to_int label))
        0 nodes
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
(declare-rel entry (node))
(declare-rel reachable (node))
(declare-rel expected-reachable (node))
(declare-rel bad ())
(declare-var a node)
(declare-var b node)
(rule (=> (entry a) (reachable a)))
(rule (=> (and (reachable a) (edge a b)) (reachable b)))
(rule (=> (and (reachable a) (not (expected-reachable a))) bad))
(rule (=> (and (expected-reachable a) (not (reachable a))) bad))
|}
      width;
    Cfg_z3.fmt_fact fmt "entry" [id facts.entry];
    List.iter
      (fun label -> Cfg_z3.fmt_fact fmt "expected-reachable" [id label])
      nodes;
    List.iter
      (fun (source, target) ->
        Cfg_z3.fmt_fact fmt "edge" [id source; id target])
      facts.edges;
    Format.pp_print_string fmt "(query bad)";
    Format.pp_print_flush fmt ();
    Buffer.contents buffer
end

let fallback cfg internal_failure z3_code =
  let z3_result = Cfg_z3.run_validation_fallback z3_code in
  Misc.fatal_errorf
    "validate_reachability: internal Datalog failed for CFG %S.@.%s@.%s@.Z3 \
     reproducer:@.%s@.CFG:@.%a"
    cfg.Cfg.fun_name internal_failure z3_result z3_code Printcfg.cfg cfg

let validate_reachability (cfg : Cfg.t) =
  let facts = Cfg_validation_facts.Graph.create cfg in
  let z3_code () = Z3.code facts in
  match Internal.validate facts with
  | Ok () -> ()
  | Error error -> fallback cfg error (z3_code ())
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    let error =
      Format.sprintf "exception: %s@.%s" (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string backtrace)
    in
    fallback cfg error (z3_code ())
