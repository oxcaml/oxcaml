(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Simon Spies, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Diagnostic that tracks source-level variables through compilation
    passes. The diagnostic is active only when
    [!Clflags.dump_variable_availability] is set; all observation calls
    are no-ops otherwise.

    This module contains the machinery for the table, helpers and the
    report printer. It does not own any state: the table is created by
    applying [Make] to a uid description and a location description.
    Callers that need a single process-wide table should instantiate the
    functor exactly once at a well-known site (see
    [Type_shape.Variable_availability]).

    The functor takes a [Loc] parameter so that this module can carry
    actual locations (rather than pre-formatted strings) without
    depending on [Location] (which lives later in the dependency
    order). *)

(** The pipeline checkpoints at which observations may be recorded.
    The constructor names match the [Compiler_hooks] passes that
    populate them. *)
module Checkpoint : sig
  type t =
    | Typedtree
    | Raw_lambda
    | Lambda
    | Flambda2_input
    | Flambda2_simplified
    | Flambda2_output
    | Cmm
    | Debug_info_variables
    | Debug_info_nonempty_range
end

type source_kind =
  | Parameter of int
  | Local
  | For_index
  | Comprehension_index
  | Letop_param
  | Alias

module type Uid = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val print : Format.formatter -> t -> unit

  (** Sentinel value used to recognize "no stable identity" uids. Source
      variables registered with this uid are silently skipped, and
      observations recorded against this uid are silently dropped. *)
  val no_uid : t
end

module type Loc = sig
  type t

  val print : Format.formatter -> t -> unit
end

module type S = sig
  type uid

  type loc

  type observed_id =
    | Source_uid of uid
    | Projected_source_uid of { source_uid : uid; field : int }

  type source_function

  (** [is_enabled ()] is [!Clflags.dump_variable_availability]. *)
  val is_enabled : unit -> bool

  (** Reset per-compilation-unit state. Idempotent. *)
  val reset : unit -> unit

  (** Set the unit name shown at the top of the report. *)
  val set_unit_name : string -> unit

  (** Register a function-scope binder under which source variables can
      then be registered. [display_name] is shown in the report. *)
  val register_source_function :
    display_name:string ->
    location:loc ->
    source_function

  (** Register a module-scope binder. Variables registered under it are
      module-level [let] bindings rather than runtime locals, so they are
      excluded from the compilation-unit-level headline summary. *)
  val register_source_module :
    display_name:string ->
    location:loc ->
    source_function

  (** Register a source-level value binder under a function. Calls with
      the sentinel uid [Uid.no_uid] are silently skipped. Duplicate
      registrations (same uid) are also silently skipped. An implicit
      observation at checkpoint [Typedtree] is recorded for successful
      registrations. *)
  val register_source_variable :
    source_function ->
    uid:uid ->
    name:string ->
    location:loc ->
    kind:source_kind ->
    unit

  (** Record that the uid was seen at this pipeline checkpoint. Calls
      with the sentinel [Uid.no_uid] (or a projection thereof) and
      calls made while the diagnostic is disabled are silently
      dropped. *)
  val record_observation : checkpoint:Checkpoint.t -> observed_id -> unit

  (** Record that source uid [merged] has been alpha-renamed to share
      its runtime binding with [surviving]. After this call,
      observations recorded against [surviving] (at any checkpoint)
      are credited to [merged] as well, so a merged sibling shows the
      same effective survival as the surviving uid in the report. The
      report still lists both source-level bindings, but a merged one
      is annotated as such. Calls involving the sentinel [Uid.no_uid],
      or where [surviving] and [merged] are equal, are silently
      dropped. *)
  val register_merged_uid : surviving:uid -> merged:uid -> unit

  val print_report : Format.formatter -> unit
end

module Make (Uid : Uid) (Loc : Loc) :
  S with type uid = Uid.t and type loc = Loc.t
