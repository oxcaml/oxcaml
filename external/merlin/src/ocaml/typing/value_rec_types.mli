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

(** The kind of a binding in a [let rec] group, as computed by
    [Value_rec_check.classify_recursive_binding]. *)
type recursive_binding_kind =
| Recursive_function
  (** A syntactic function. It may refer to the other bindings of the group
      and is compiled as part of a [Lletrec]. *)
| Not_recursive
  (** A binding that does not refer to any binding of the group. It is
      compiled as a regular let-binding, evaluated before the functions. *)
| Recursive_record of int list
  (** A record literal, possibly the inline record of a constructor, whose
      fields at the given positions are mutable and refer to the group. The
      record is allocated before the functions with placeholders in these
      fields, which are assigned once the whole group is defined. *)
| Recursive_lazy
  (** A [lazy] expression that refers to the group. The lazy block is
      allocated before the functions with a placeholder instead of its
      thunk, which is assigned once the whole group is defined. *)
| Recursive_ref
  (** An application of the [%makemutable] primitive, such as [ref], whose
      argument refers to the group. The cell is allocated before the
      functions with a placeholder, which is replaced by the argument once
      the whole group is defined. *)
