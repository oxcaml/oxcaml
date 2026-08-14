(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The occurrence test for the name slot of arrow types.

    In [x:T -> U], the bare name [x] is a positional binder if [x] occurs
    free in a refinement predicate within [T] or [U], and the ordinary label
    [x] otherwise.  The test is purely syntactic and runs on the parsetree,
    before name resolution.  It respects the binders introduced inside
    predicates ([let], [fun] parameters, [match] cases) and the binders
    introduced by nested arrows.

    This module is the single implementation of that test; translation
    ([Typetexp]), type approximation ([Typecore]) and printing back to
    source ([Untypeast], to decide when a label must be escaped as [~x:])
    all go through it. *)

(** Does [name] occur free in a refinement predicate within one of the given
    types? *)
val name_used_in_refinement : string -> Parsetree.core_type list -> bool

(** Does [name] occur free in the given predicate expression (which is
    already inside a refinement)?  Used by the printers to decide when a
    printed label must be escaped as [~x:]. *)
val name_used_in_predicate : string -> Parsetree.expression -> bool
