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

  val create :
    Compilation_unit.t ->
    name:string ->
    is_always_immediate:bool ->
    Flambda_kind.t ->
    t

  val get_compilation_unit : t -> Compilation_unit.t

  val in_compilation_unit : t -> Compilation_unit.t -> bool

  val is_imported : t -> bool

  (** [to_string t] is [name t ^ "_" ^ string_of_int (stamp t)]. *)
  val to_string : t -> string

  val name : t -> string

  (** The stamp making this slot unique within its compilation unit. *)
  val stamp : t -> int

  val canonical_name : t -> string

  val kind : t -> Flambda_kind.t

  val is_always_immediate : t -> bool

  val rename : t -> t
end

module Make (_ : sig
  val colour : Format.formatter -> unit
end) : S
