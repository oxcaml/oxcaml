(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Jane Street Group LLC                              *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Conversion of specification expressions back to the parse tree, for
    printing and for generating code. Types, sorts and modes are dropped. *)

val lident_of_path : Path.t -> Longident.t

(** [lident_of_path] converts the global paths of the expression to long
    identifiers. [annotate] gives a type constraint for the constructors,
    records and field accesses from their types, so that the result types
    again with the same resolution of constructors and labels. *)
val expression :
  lident_of_path:(Path.t -> Longident.t) ->
  annotate:('ty -> Parsetree.core_type option) ->
  'ty Spec.expression -> Parsetree.expression

(** An [annotate] function: the head type constructor applied to
    wildcards. Nothing for predefined types and the types of inline
    records, which cannot be written. *)
val head_type_annotation :
  lident_of_path:(Path.t -> Longident.t) ->
  Types.type_expr -> Parsetree.core_type option
