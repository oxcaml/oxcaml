(******************************************************************************
 *                                  OxCaml                                    *
 *                        Simon Spies, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Diagnostic tracking source-level variables through compilation passes,
    active only when [!Clflags.dump_variable_availability] is set. Most entry
    points self-check the flag and do nothing when it is off, so no variable
    data accumulates. The exceptions are [register_source_function] and
    [register_source_module], which return a scope value and so cannot be
    no-ops; they rely on their callers to gate them. Callers may still check
    [is_enabled] first to avoid building up arguments that would be discarded.

    For each source variable we keep its {e trace} -- the checkpoints at which
    its uid was observed -- plus an optional {e drop} classification from the
    pass that eliminated it. The trace gives the outcome ([present] / [dropped]
    across a pipeline edge); the classification says whether a disappearance is
    an [[ok]] drop (never expected in DWARF) or a real coverage [[gap]].

    [Make] introduces top-level state for the table. *)

(** The pipeline checkpoints at which observations may be recorded. [Raw_lambda]
    and [Lambda] are populated from the like-named [Compiler_hooks] passes;
    [Typedtree] from [Compiler_hooks.Typed_tree_impl]; [Flambda2_input],
    [Flambda2_simplified] and [Flambda2_output] from direct [observe] calls in
    [Flambda2.flambda_to_flambda0]; and [Cmm] and [Debug_info] from direct calls
    in the backend ([select_utils]/ [cfg_selectgen] and the DWARF emitter
    respectively). *)
module Checkpoint : sig
  type t =
    | Typedtree
    | Raw_lambda
    | Lambda
    | Flambda2_input
    | Flambda2_simplified
    | Flambda2_output
    | Cmm
    | Debug_info
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
      variables registered with this uid are silently skipped, and observations
      recorded against this uid are silently dropped. *)
  val no_uid : t
end

module type Loc = sig
  type t

  (** Compact ["line:cols"] form without a filename, for per-variable rows where
      the file is constant across the compilation unit. *)
  val print_compact : Format.formatter -> t -> unit

  (** Compact ["file:line:cols"] form, for the per-function header. *)
  val print_with_file : Format.formatter -> t -> unit
end

module type S = sig
  type uid

  type loc

  type observed_id =
    | Source_uid of uid
    | Projected_source_uid of
        { source_uid : uid;
          field : int
        }

  type source_definition

  (** [is_enabled ()] is [!Clflags.dump_variable_availability]. *)
  val is_enabled : unit -> bool

  (** Reset per-compilation-unit state. Idempotent. *)
  val reset : unit -> unit

  (** Set the unit name shown at the top of the report. *)
  val set_unit_name : string -> unit

  (** Register a function-scope binder under which source variables can then be
      registered. [display_name] is shown in the report. *)
  val register_source_function :
    display_name:string -> location:loc -> source_definition

  (** Register a module-scope binder. Variables registered under it are
      module-level [let] bindings rather than runtime locals; they are
      automatically classified as [Module_level] ok-drops (so they are excluded
      from the coverage totals, since their absence from DWARF locals is
      expected). *)
  val register_source_module :
    display_name:string -> location:loc -> source_definition

  (** Register a source-level value binder under a function. Calls with the
      sentinel uid [Uid.no_uid] are silently skipped. Duplicate registrations
      (same uid) are also silently skipped. The variable's trace starts with an
      observation at checkpoint [Typedtree]. *)
  val register_source_variable :
    source_definition ->
    uid:uid ->
    name:string ->
    location:loc ->
    kind:source_kind ->
    unit

  (** Record that the uid was seen at this pipeline checkpoint. Calls with the
      sentinel [Uid.no_uid] (or a projection thereof), calls for uids that were
      never registered as source variables, and calls made while the diagnostic
      is disabled are all silently dropped. *)
  val record_observation : checkpoint:Checkpoint.t -> observed_id -> unit

  (** Reason a source binding disappears in a way that does not lose coverage:
      the user should not expect such a binding in DWARF, so the drop is
      reported as [[ok]] and the binding is excluded from the coverage totals.
  *)
  type ok_drop_reason =
    | Merged_with of uid
        (** The same [Ident.t] was reused with a fresh debug uid (e.g. an
            or-pattern arm whose bindings have been unified by typing). The
            named survivor's source location carries the binding. *)
    | Ignored_variable
        (** Pattern variable not used by the body (e.g. [_x]): either [Matching]
            never emitted a binding for it, or [simplify_lets] eliminated the
            binding because the use count was zero. *)
    | Function_became_catch
        (** Local function whose name was eliminated because all of its call
            sites were rewritten as [Lstaticcatch] / [Lstaticraise]
            continuations by [simplify_local_functions]. *)
    | Module_level
        (** Module-level [let] binding, not a runtime local. Applied
            automatically to variables registered under a module scope. *)

  (** Record that source uid [uid] was dropped losslessly for the given
      [reason]. Calls involving the sentinel [Uid.no_uid], or a [Merged_with]
      whose survivor equals [uid], are silently dropped. The earliest-registered
      classification wins. *)
  val register_ok_drop : uid:uid -> reason:ok_drop_reason -> unit

  (** Code site that is known to drop a source uid in a way that loses coverage.
      Each constructor names one specific spot in the compiler so the report can
      direct an investigator to the relevant file. New entries should describe
      the site, not the symptom, so they can be deleted once the underlying
      cause is fixed. *)
  type gap_cause =
    | Phantom_let_dropped_in_to_cmm
        (** [to_cmm_expr.ml]: a [Let_expr] at [Name_mode.phantom] is discarded
            by [to_cmm] instead of producing a [Cphantom_let]. *)
    | Missing_phantom_let_for_let_optimization
        (** [lambda_to_flambda.ml]: the [let v = e in v] optimization drops the
            source duid because no phantom let preserves it. *)
    | Lmutlet_handler_loses_duid
        (** [lambda_to_flambda.ml]: the [Lmutlet] handler introduces a fresh
            temporary with [Lambda.debug_uid_none]. *)

  (** Record that source uid [uid] was dropped at a known code site that loses
      coverage. The cause is shown in the per-variable row (and the drop counts
      as a [[gap]] in the totals) so an investigator can navigate to the
      responsible code. Calls with [Uid.no_uid] are silently dropped. The
      earliest-registered classification wins. *)
  val register_gap : uid:uid -> cause:gap_cause -> unit

  val print_report : Format.formatter -> unit
end

module Make (Uid : Uid) (Loc : Loc) :
  S with type uid = Uid.t and type loc = Loc.t
