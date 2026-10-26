(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Jeremy Yallop, University of Cambridge                   *)
(*               Gabriel Scherer, Project Parsifal, INRIA Saclay          *)
(*               Alban Reynaud, ENS Lyon                                  *)
(*                                                                        *)
(*   Copyright 2017 Jeremy Yallop                                         *)
(*   Copyright 2018 Alban Reynaud                                         *)
(*   Copyright 2018 INRIA                                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Static checking of recursive declarations. See the implementation for
    the rules. *)

type error =
  | Illegal_reference of Location.t * Ident.t
    (** The subexpression at the location refers to the recursively bound
        name where it may not. *)
  | Illegal_cycle of Location.t * Ident.t list
    (** The given names form a cycle of blocks that does not go through a
        function, a lazy value or a mutable field. The location is that of
        the reference that closes the cycle. *)

(** [classify_recursive_bindings bindings] checks the right-hand sides of
    the bindings [(x1, e1); ...; (xn, en)] of a [let rec] group, and returns
    how each of them is compiled. *)
val classify_recursive_bindings :
  (Ident.t * Typedtree.expression) list ->
  (Value_rec_types.recursive_binding_kind list, error) result

(** [split_preamble e] is [Some body] if [e] is an item of the preamble of
    a recursive definition ([let ... in body], [let open ... in body],
    [let module ... in body], [let exception ... in body] or [e1; body]). *)
val split_preamble : Typedtree.expression -> Typedtree.expression option

(** [check_class_expr ids ce] checks the definition [ce] of a class in a
    recursive group of classes [ids]. Returns [Error id] if a let-binding in
    the spine of [ce] refers to the class [id]. *)
val check_class_expr :
  Ident.t list -> Typedtree.class_expr -> (unit, Ident.t) result
