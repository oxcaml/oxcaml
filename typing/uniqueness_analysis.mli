(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree

(* CR-soon zqian: rename the following so others can call
[Uniqueness_analysis.xxx] instead of
[Uniqueness_analysis.check_uniqueness_xxx] *)

(* Check that idents which are used more than once, are not used with mode
   unique. *)
val check_uniqueness_exp : expression -> unit

(* Check that idents which are used more than once, are not used with mode
   unique. *)
val check_uniqueness_value_bindings : value_binding list -> unit

(** Check a module expression that is not part of any other checked context,
    such as the body of [module type of struct ... end]. No-op when the unique
    extension is disabled. *)
val check_uniqueness_module_expr : module_expr -> unit

(** State threaded through the type-checking of the structure items of a
    compilation unit (or toplevel phrase), accumulating usage across items so
    that module components can be tracked precisely. *)
type structure_state

val new_structure_state : unit -> structure_state

(** Check a freshly type-checked structure item, sequenced after the items
    checked so far. No-op when the unique extension is disabled. *)
val check_structure_item : structure_state -> structure_item -> unit

(** Mark the components exported by the given signature as aliased: they are
    accessible to other compilation units, so the defining unit may not consume
    them uniquely. Call this once all items have been checked. *)
val mark_exported : structure_state -> Types.signature -> unit

(** Like [mark_exported], but for contexts that export everything, such as a
    toplevel phrase or a unit without an interface. *)
val mark_all_exported : structure_state -> unit

(* These definitions are just to allow printing in the debugger *)
module type P := sig
  type t

  val print : Format.formatter -> t -> unit
end

module Maybe_unique : P

module Maybe_aliased : P

module Aliased : P

module Usage : P

module Tag : P

module Projection : P

module Usage_tree : P

module Usage_forest : P

module Paths : P

module Value : P

module Ienv : sig
  include P

  module Extension : P
end
