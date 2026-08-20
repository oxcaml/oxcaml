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

  (** Creates a slot with the given (negative) stamp instead of drawing from the
      internal counter. Used to reconstruct, in an importing compilation unit, a
      slot identical to one created in the defining unit from lambda-level data
      (layout-polymorphism templates); negative stamps cannot collide with slots
      created by [create]. It is the caller's responsibility to ensure that
      equal stamps are only ever paired with equal names, kinds and immediacies.
  *)
  val create_deterministic :
    Compilation_unit.t ->
    name:string ->
    is_always_immediate:bool ->
    Flambda_kind.t ->
    stamp:int ->
    t

  (** Whether this slot was created by [create_deterministic]. Such slots may
      have uses that are invisible in the defining unit (inside marshalled
      layout-polymorphism templates), so they must not be removed as dead. *)
  val has_deterministic_stamp : t -> bool

  val get_compilation_unit : t -> Compilation_unit.t

  val in_compilation_unit : t -> Compilation_unit.t -> bool

  val is_imported : t -> bool

  val to_string : t -> string

  val name : t -> string

  val kind : t -> Flambda_kind.t

  val is_always_immediate : t -> bool

  val rename : t -> t
end

module Make (_ : sig
  val colour : Format.formatter -> unit
end) : S
