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
    identifiers. *)
val expression :
  lident_of_path:(Path.t -> Longident.t) ->
  'ty Spec.expression -> Parsetree.expression
