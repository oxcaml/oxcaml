[@@@ocaml.warning "+a-30-40-41-42"]

val split_live_ranges : bool Regalloc_utils.Param.t

val split_more_destruction_points : bool Regalloc_utils.Param.t

val split_around_loops : bool Regalloc_utils.Param.t

val indent : unit -> unit

val dedent : unit -> unit

val log : ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

val log_dominance_frontier : Cfg.t -> Cfg_dominators.t -> unit

val log_dominator_tree : Cfg_dominators.dominator_tree -> unit

val log_dominator_forest : Cfg_dominators.dominator_tree list -> unit

val log_substitution : Regalloc_substitution.t -> unit

val log_substitutions : Regalloc_substitution.map -> unit

val log_stack_subst : Regalloc_substitution.t -> unit

val live_at_block_beginning : Cfg_with_infos.t -> Label.t -> Reg.Set.t

type destruction_kind =
  | Destruction_on_all_paths
  | Destruction_only_on_exceptional_path

val equal_destruction_kind : destruction_kind -> destruction_kind -> bool

val destruction_point_at_end : Cfg.basic_block -> destruction_kind option

(** [is_rematerializable_shape desc] is [true] iff a basic instruction with that
    [desc] is a legitimate rematerialization source: it must be pure (no side
    effects, no observable memory write or fence), cheap enough to be worth
    duplicating, and must not destroy hardware registers in ways that change the
    surrounding allocation. The set of accepted shapes is shared between the
    analysis (cf. {!Uses.compute}) that classifies rematerialization sources and
    the validator (cf. [Regalloc_validate]) that recognizes the rematerialized
    instructions in the post-allocation CFG. *)
val is_rematerializable_shape : Cfg.basic -> bool
