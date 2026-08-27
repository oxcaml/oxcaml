(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Either an assembly label or an assembly symbol. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Label of Asm_label.t
  | Symbol of Asm_symbol.t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val print : Format.formatter -> t -> unit
