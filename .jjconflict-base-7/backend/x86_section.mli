(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

module Section_name : sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val compare : t -> t -> int

  val make : string list -> string option -> string list -> t

  val of_string : string -> t

  val to_string : t -> string

  val flags : t -> string option

  val alignment : t -> int64

  val is_text_like : t -> bool

  val is_data_like : t -> bool

  val is_note_like : t -> bool

  module Map : Map.S with type key = t

  module Tbl : Hashtbl.S with type key = t
end
