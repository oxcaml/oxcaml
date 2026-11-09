(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Vincent Laviron, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro, SAS                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types related to the compilation of recursive value bindings *)

(** How the definition of a member of a [let rec] group, after its preamble
    (see [preamble_item]), is compiled. *)
type definition =
| Function
  (** A syntactic function. It may refer to the group and is compiled as part
      of a [Lletrec]. *)
| Construction
  (** A block (constructor, tuple, record, polymorphic variant or reference
      cell) whose fields that refer to the group are constructions. The block
      is allocated before the functions with placeholders in these fields,
      which are assigned once the whole group is defined. *)
| Lazy
  (** A [lazy] expression that refers to the group. The lazy block is
      allocated before the functions with a placeholder instead of its
      thunk, which is assigned once the whole group is defined. *)
| Value
  (** An expression that does not refer to the group. It is evaluated
      before the functions. *)

(** The kind of a binding in a [let rec] group, as computed by
    [Value_rec_check.classify_recursive_bindings]. *)
type recursive_binding_kind =
| Not_recursive
  (** A binding that does not refer to the group at all. It is compiled as a
      regular let-binding, evaluated before the functions. *)
| Recursive of { preamble : preamble_item list; definition : definition }
  (** A binding that refers to the group. Its right-hand side consists of the
      items of the [preamble], in order (see
      [Value_rec_check.split_preamble]), followed by the [definition]. *)

(** An item of the preamble of a recursive binding, such as
    [let x = e in], [let open M in] or [e;]. *)
and preamble_item =
| Hoisted
  (** An item that does not refer to the group. It is evaluated before the
      group. *)
| Members of recursive_binding_kind list
  (** A [let] (recursive or not) whose bindings refer to the group. They
      join the group, with the given kinds. *)
