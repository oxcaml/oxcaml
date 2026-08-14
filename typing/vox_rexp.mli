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

(** Generic operations on refinement predicates ({!Types.refinement_expression}).

    A predicate is resolved syntax: the only pieces of it that live in the
    type graph proper are its interior types (the types of
    [Rexp_constraint] nodes and of nested refinements within them).  The
    traversals below visit exactly those; the callers decide what to do
    with them. *)

open Types

(** Fold over the interior types of a predicate. *)
val fold_types :
  ('a -> type_expr -> 'a) -> 'a -> refinement_expression -> 'a

val iter_types : (type_expr -> unit) -> refinement_expression -> unit

(** Rebuild a predicate.  [type_expr] is applied to interior types.
    If [freshen] is set, every binder ident introduced inside the predicate
    is renamed to a fresh stamp ([Subst] freshens binder stamps on import;
    [Btype] does not).  [rename] maps externally-bound idents (arrow
    binders); [value_path] rewrites the paths of free idents. *)
val map :
  ?rename:Ident.t Ident.Map.t ->
  ?freshen:bool ->
  ?value_path:(Path.t -> Path.t) ->
  type_expr:(type_expr -> type_expr) ->
  refinement_expression -> refinement_expression

(** Syntactic alpha-equivalence.  [type_eq] compares interior types;
    [pairs] gives the pairing of externally-bound idents (the arrow binders
    of the two types being compared). *)
val equal :
  type_eq:(type_expr -> type_expr -> bool) ->
  pairs:(Ident.t * Ident.t) list ->
  refinement_expression -> refinement_expression -> bool

(** Back to surface syntax, for printing.  [var_name] chooses the printed
    name of a bound ident; [value_ident] renders a free ident from its
    resolved (possibly substituted) path; [core_type] renders an interior
    type.  Holes print as [_] via [Pexp_hole]. *)
val untype :
  var_name:(Ident.t -> string) ->
  value_ident:(Path.t -> Longident.t Location.loc) ->
  core_type:(type_expr -> Parsetree.core_type) ->
  refinement_expression -> Parsetree.expression

(** Does the predicate mention the given bound ident? *)
val mentions_ident : Ident.t -> refinement_expression -> bool

(** The first free value path in the predicate for which [f] answers, if
    any.  Interior types are not scanned here; the caller scans the type
    graph. *)
val find_value_path :
  (Path.t -> 'a option) -> refinement_expression -> 'a option
