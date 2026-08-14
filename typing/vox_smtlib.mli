(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Rendering obligations to SMT-LIB 2.6 scripts.

    There is one renderer, shared by the printing backend and the z3
    backend, so a printed script is byte-for-byte what z3 receives and a
    translation defect shows up as a baseline diff rather than a mysterious
    [unknown]. *)

(** The two queries of the discharge protocol.

    Polarity, written down because a sign error here silently inverts every
    result:
    - [Prove] asserts the hypotheses and the {e negated} goal; [unsat]
      means the goal holds in every state satisfying the hypotheses.
    - [Disprove] asserts the hypotheses and the goal itself; [unsat] means
      the goal fails in every such state, which is what a refutation
      claims.  A mere model of the [Prove] query is not a refutation: with
      uninterpreted functions and underspecified selectors it need not
      correspond to any reachable program state. *)
type query =
  | Prove
  | Disprove

(** [render query obligation] is the SMT-LIB script for [query], or an
    error if the obligation is ill-formed: an undeclared variable, symbol,
    sort or constructor, an arity or field-index mismatch, a malformed
    literal, or a duplicate declaration.  (Sorts of well-declared terms are
    not re-checked here; the solver reports those.)

    [timeout_ms] becomes [(set-option :timeout ...)].

    A [Prove] script names each hypothesis [h<id>] and ends with
    [(get-unsat-core)], [(get-model)] and [(get-info :reason-unknown)];
    whichever do not apply to the [check-sat] answer produce ignorable
    [(error ...)] lines.  A [Disprove] script only asks
    [(get-info :reason-unknown)]. *)
val render :
  ?timeout_ms:int -> query -> Vox_logic.Obligation.t -> (string, string) result
