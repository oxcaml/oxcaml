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

(** [classify_recursive_binding ids e] checks the right-hand side [e] of a
    binding in a [let rec] group binding [ids]. Returns [Error (loc, id)] if
    the subexpression at [loc] refers to the recursively bound [id] where it
    may not. *)
val classify_recursive_binding :
  Ident.t list ->
  Typedtree.expression ->
  (Value_rec_types.recursive_binding_kind, Location.t * Ident.t) result

(** [check_class_expr ids ce] checks the definition [ce] of a class in a
    recursive group of classes [ids]. Returns [Error id] if a let-binding in
    the spine of [ce] refers to the class [id]. *)
val check_class_expr :
  Ident.t list -> Typedtree.class_expr -> (unit, Ident.t) result
