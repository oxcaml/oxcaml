(** Given a typed implementation and the signature that constrains it, find
    the kind, mode, and modality annotations that make the interface
    strictly wider — accepting more programs at use sites — without
    breaking the implementation. *)

(** Which side of an arrow a position sits on, and how the analysis got
    there. Positions are compared by [dir], so the same position found from
    two implementations of one interface has the same [dir]. *)
module Variance : sig
  type t = Covariant | Contravariant
end

module Arrow_pos : sig
  (** The syntactic route from the declaration's type to this position. *)
  type dir = Here | In_arg of dir | In_ret of dir

  type role = Arg | Ret

  type t = { dir : dir; nesting : int; role : role; variance : Variance.t }
end

(** One arrow position where the implementation supports [impl], a mode
    strictly stronger than the [intf] the interface declares. *)
type arrow_diff =
  { path : Arrow_pos.t; impl : Mode.Alloc.Const.t; intf : Mode.Alloc.Const.t }

(** The analysis result before it is rendered as text: what each interface
    declaration provably supports, keyed by the declaration's span in the
    .mli. Values here mention no live mode variables and no typing
    environment, so they can be held across Merlin pipelines and combined:
    an interface with several implementations supports an atom only if every
    implementation supports it, and intersecting per atom — rather than
    intersecting rendered edits — keeps the atoms the implementations agree
    on. *)
module Abstract : sig
  type diff =
    | Kind_annotation of string
    | Mode_diffs of
        { modality_diff :
            (impl:Mode.Modality.Const.t * intf:Mode.Modality.Const.t) option;
          arrow_diffs : arrow_diff list
        }

  type t = { decl_loc : Location.t; diff : diff }
end

(** Find the strengthenings [impl_sig] supports for the declarations of
    [intf_sig].

    [impl_sig] must be the signature the implementation (.ml) pipeline
    inferred for the unit ([Typedtree.structure.str_type]): its types share
    the pipeline's live mode variables, which the analysis reads. [env] is
    that pipeline's typing environment: the analysis must run in that
    pipeline's context, after typing and with no inclusion-check constraints
    live in the mode solver (Merlin skips the unit-level check, so this
    holds); the solver state is restored before returning. *)
val analyze :
  env:Env.t ->
  impl_sig:Types.signature ->
  intf_sig:Types.signature ->
  unit ->
  Abstract.t list

(** Render strengthenings as at most one code action strengthening the whole
    interface at once. When more than half of a signature's values support
    an atom, it is hoisted to the signature's floating [@@ ...] clause and
    the dissenting items are exempted with explicit weak atoms; the action's
    edits are therefore only sound applied atomically.

    [intf] must be the parse of the .mli the strengthenings were analyzed
    against, and [intf_file] that .mli's resolved path (the filename stamped
    into [intf]'s locations); declarations are found by their recorded
    spans, and a weakness whose declaration is not found — e.g. the .mli on
    disk has drifted — renders no edits. *)
val render :
  intf_file:string ->
  intf:Parsetree.signature ->
  Abstract.t list ->
  Query_protocol.Intf_weakness.code_action list

(** [analyze] followed by [render], for the single-implementation case. *)
val code_actions :
  env:Env.t ->
  impl_sig:Types.signature ->
  intf_sig:Types.signature ->
  intf_file:string ->
  intf:Parsetree.signature ->
  unit ->
  Query_protocol.Intf_weakness.code_action list
