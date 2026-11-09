(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compilation of recursive definitions to Lambda. See the implementation
    for the compilation scheme. *)

open Lambda

(** A member of a recursive group, or an item of the preamble of a member,
    already translated to Lambda. The items of a group are listed in
    evaluation order. *)
type item =
  | Preamble of (lambda -> lambda)
      (** Code that does not refer to the group, such as [let x = e in] or
          [e;], as a context for the rest of the group. *)
  | Value of
      { id : Ident.t; debug_uid : debug_uid; layout : layout; def : lambda }
      (** A definition that does not refer to the group. *)
  | Function of { wrap : lambda -> lambda; binding : rec_binding }
      (** A syntactic function. [wrap] binds the class tables of the
          immediate objects of the function; it is closed. *)
  | Lazy of
      { id : Ident.t; debug_uid : debug_uid; thunk : lambda;
        loc : Debuginfo.Scoped_location.t }
      (** [lazy e], where [thunk] is [fun () -> e]. *)
  | Block of
      { id : Ident.t; debug_uid : debug_uid; layout : layout; def : lambda }
      (** A block, translated to an allocation [def] whose arguments that
          refer to the group only allocate. *)

(** [compile_letrec items body] defines the items of a recursive group
    around [body]. *)
val compile_letrec : item list -> lambda -> lambda

(** The kind of the definition of a class in a recursive group of classes,
    as computed by [Translclass.transl_class]. *)
type class_kind =
  | Static
      (** A definition that evaluates to a class block of size
          [class_block_size] and may refer to the group. *)
  | Dynamic
      (** A definition that does not refer to the group. *)

(** The size of a class block. *)
val class_block_size : int

(** [compile_class_bindings bindings body] defines a recursive group of
    classes around [body]. *)
val compile_class_bindings :
  (Ident.t * debug_uid * class_kind * lambda) list -> lambda -> lambda
