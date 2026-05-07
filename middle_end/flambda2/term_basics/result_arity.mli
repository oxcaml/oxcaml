(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The arity of the result(s) of a function or application, when known.

    [Unknown] means that the layout of the result could not be determined (e.g.
    a function whose return type has layout [any]). [Bottom] means that the
    function or application never returns normally.

    No producer currently emits [Unknown] or [Bottom]; later patches introduce
    them for layout [any] results. Such results are restricted to tail position,
    where they are forwarded to the caller untouched rather than materialized.

    Policies for consumers:

    - Lowering and transformations that need a real arity (e.g. To_cmm, fexpr
      printing, inlining wrappers) must use [to_arity_exn], which fatal-errors
      on [Unknown] and [Bottom].

    - Bookkeeping paths that cannot introduce concrete result variables (e.g.
      the reaper) use [unarized_components_or_empty]. *)
type t = [`Unarized] Flambda_arity.t Or_unknown_or_bottom.t

val ok : [`Unarized] Flambda_arity.t -> t

val print : Format.formatter -> t -> unit

val equal_exact : t -> t -> bool

val equal_ignoring_subkinds : t -> t -> bool

val to_arity_exn : ?message:string -> t -> [`Unarized] Flambda_arity.t

val unarized_components_or_empty : t -> Flambda_kind.With_subkind.t list
