(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Chambart, Nathanaëlle Courant, OCamlPro             *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** file sections cache *)

type t
type idx

val create : int array -> string -> in_channel -> first_section_offset:int -> t

val empty : t

val length : t -> int

val get : t -> idx -> Obj.t

val serialize : t -> string array * int array * int

val from_array : Obj.t array -> t

module Builder : sig
  type file_sections := t
  type t

  val create : int -> t
  val add : t -> Obj.t -> idx
  val build : t -> file_sections
end
