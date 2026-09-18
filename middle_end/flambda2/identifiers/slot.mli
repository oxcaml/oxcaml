(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  include Container_types.S

  module Lmap : Lmap.S with type key = t

  type payload

  val create : Compilation_unit.t -> name:string -> payload -> t

  val get_compilation_unit : t -> Compilation_unit.t

  val in_compilation_unit : t -> Compilation_unit.t -> bool

  val is_imported : t -> bool

  val to_string : t -> string

  val name : t -> string

  val canonical_name : t -> string

  val payload : t -> payload

  val rename : t -> t

  val export_stamp_counter : unit -> int
end

module Make (P : sig
  val colour : Format.formatter -> unit

  type payload

  val print_payload : Format.formatter -> payload -> unit
end) : S with type payload := P.payload
