[@@@ocaml.warning "+a-30-40-41-42"]

val split_live_ranges : bool Lazy.t

val split_more_destruction_points : bool Lazy.t

val split_around_loops : bool Lazy.t

val split_rematerialize : bool Lazy.t

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

type definition_kind =
  | Reload
  | Rematerialize of Regalloc_utils.Instruction.t

module Uses : sig
  type source =
    | Load of Regalloc_utils.Instruction.t
    | Move of Reg.t
    | Other

  val format_source : Format.formatter -> source -> unit

  type set =
    | At_most_once of { source : source }
    | Maybe_more_than_once

  val format_set : Format.formatter -> set -> unit

  type t = set Reg.Tbl.t

  val format : Format.formatter -> t -> unit

  val compute : Cfg_with_infos.t -> set Reg.Tbl.t
end

(** [try_rematerialize uses ~available reg] returns [Some ld] if [reg] can be
    rematerialized as a copy of the immutable load [ld]: [reg] must be set at
    most once, that single set must be the load (possibly via a move chain of
    length one), and all of the load's arguments must themselves be set at most
    once and be members of [available]. The [available] set is the set of
    registers expected to hold a usable value at the rematerialization point. *)
val try_rematerialize :
  Uses.t -> available:Reg.Set.t -> Reg.t -> Regalloc_utils.Instruction.t option
