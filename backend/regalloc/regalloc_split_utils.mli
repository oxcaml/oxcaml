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

(** [at_most_once_in uses available reg] is [true] iff [reg] is set at most once
    and is a member of [available]. *)
val at_most_once_in : Uses.t -> Reg.Set.t -> Reg.t -> bool

(** [try_rematerialize uses ~is_arg_ok reg] returns [Some ld] if [reg] can be
    rematerialized as a copy of the immutable load [ld]. The chain
    [reg <- move ... <- move <- load] is followed of arbitrary length; every
    register on the chain must be set at most once. The load is accepted iff
    [is_arg_ok] holds for every one of its arguments. *)
val try_rematerialize :
  Uses.t ->
  is_arg_ok:(Reg.t -> bool) ->
  Reg.t ->
  Regalloc_utils.Instruction.t option
