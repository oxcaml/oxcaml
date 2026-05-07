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

(** The arity of the result(s) of a piece of code, when known.

    [Unknown] means that the layout of the result could not be determined; this
    happens for a function whose return type has layout [any]. Such a function
    does not materialize its result: it forwards whatever its own callees
    returned straight through to its caller. [Bottom] means that the function
    never returns normally.

    An application's result is described by [Apply_expr.Return.t], which says
    both what shape the result has and where it goes; this type is what is left
    of that once the destination is forgotten. Consult [Apply_expr.Return.t]
    rather than this type whenever the destination matters. *)
type t = [`Unarized] Flambda_arity.t Or_unknown_or_bottom.t

val ok : [`Unarized] Flambda_arity.t -> t

val print : Format.formatter -> t -> unit

val equal_exact : t -> t -> bool

val equal_ignoring_subkinds : t -> t -> bool

(** Whether the arity is known to be exactly one value: the default calling
    convention, elided when printing. *)
val is_singleton_value : t -> bool

(** A fixed arity standing in for an [Unknown] or [Bottom] result arity: a
    single value of kind [any_value].

    For [Unknown] the placeholder does not describe the result, which is
    forwarded by a tail call and may have any layout; the result never
    materializes in the forwarding function, so nothing consults the placeholder
    for its actual registers. The placeholder need only be used consistently:
    the simplifier declares an unknown-result function's return continuation
    with exactly this arity ([Simplify_set_of_closures]) and records the
    continuation's uses with it too, so declarations and uses agree by
    construction. For [Bottom] control never reaches the result's destination
    and no value ever flows, so any fixed arity is safe wherever one is
    nevertheless required (e.g. the parameters of an unreachable return
    continuation, or an over-application whose intermediate result must in fact
    be a single function value). *)
val any_value_placeholder : [`Unarized] Flambda_arity.t

(** [Ok] arities are returned unchanged; [Unknown] and [Bottom] become
    [any_value_placeholder]. *)
val to_arity_with_placeholder : t -> [`Unarized] Flambda_arity.t
