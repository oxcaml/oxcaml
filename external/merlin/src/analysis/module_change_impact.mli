type t

val create : Module_implementation_facts.t -> t

type impact =
  { witness : Module_implementation_facts.Key.t;
    check : Module_implementation_facts.Check.t
  }

type result =
  { impacts : impact list;
    omissions : Module_implementation_facts.Omission.t list
  }

val query_exact : t -> Module_implementation_facts.Key.t -> result

val query_family : t -> Shape.Uid.t -> result

val global_omissions : t -> Module_implementation_facts.Omission.t list

module For_testing : sig
  type counts =
    { context_nodes : int;
      keys : int;
      edges : int;
      condensation_edges : int;
      edge_visits : int;
      use_moves : int;
      max_parent_depth : int
    }

  val counts : t -> counts

  val merge_contexts :
    t ->
    Module_implementation_facts.Context.t ->
    Module_implementation_facts.Context.t ->
    unit
end
