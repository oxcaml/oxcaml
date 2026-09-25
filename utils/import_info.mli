(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Mark Shinwell, Jane Street UK Partnership LLP              *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module CU := Compilation_unit

(* CR mshinwell: maybe there should be a phantom type allowing to distinguish
   the .cmx case from the others. Unclear it's worth it.

   xclerc: I also wonder whether it could be useful to have an abstract Array.t
   in this module. Indeed the import infos are now mutable; we could hide the
   mutability behind an abstract type. I reckon we use only a handful of array
   operations on such values, so it should not be too bad. If that happens, it
   should probably be in another PR.

   (We could also wait for immutable arrays.) *)

(* CR mshinwell/xclerc: maybe the reading and writing code should be put in
   here, or somewhere alongside, rather than being duplicated around the
   tree. *)

(** Either an interface (.cmi) or implementation (.cmo/x) import. Should be
    avoided in new code, in preference to [Intf.t] or [Impl.t]. *)
type t

val create : CU.t -> crc_with_unit:(CU.t * string) option -> t

val create_normal : CU.t -> crc:string option -> t

(** This function will cause a fatal error on a parameter import: a parameter
    is named by a [CU.Intf.t], not a [CU.t]. Use [Intf.view] to handle
    parameter imports. *)
val name : t -> CU.t

(** This function will cause a fatal error if a [CU.t] was not provided when the
    supplied value of type [t] was created. *)
val cu : t -> CU.t

val crc : t -> string option

val has_name : t -> name:CU.t -> bool

val dummy : t

val print : Format_doc.formatter -> t -> unit

(** The preferred API to use for interface imports. An interface import might be
    a parameter, in which case it has a CRC but no implementation (its [CU.t] is
    just its name). *)
module Intf : sig
  type nonrec t = t

  val create_normal : CU.t -> crc:Digest.t -> t

  val create_alias : CU.t -> t

  val create_parameter : CU.Intf.t -> crc:Digest.t -> t

  module Nonalias : sig
    module Kind : sig
      type t =
        | Normal of CU.t
        | Parameter of CU.Intf.t
    end

    (** The "non-alias part" of the import info for an interface. An [Intf.t] is
        equivalent to a [CU.t * Nonalias.t option] (use [create], [name],
        and [spec] to convert back and forth). *)
    type t = Kind.t * Digest.t
  end

  (** [create name nonalias] is [create_normal cu crc] if [nonalias] is
      [Some (Normal cu, crc)], [create_parameter intf crc] if [nonalias] is
      [Some (Parameter intf, crc)] (in both cases the unit must match [name]),
      and [create_alias] if [nonalias] is [None]. Useful when [nonalias] is
      coming out of [Consistbl]. *)
  val create : CU.t -> Nonalias.t option -> t

  type view =
    | Normal of CU.t * Digest.t
    | Alias of CU.t
    | Parameter of CU.Intf.t * Digest.t

  val view : t -> view

  (** The basename by which this import's artifact is sought on disk. *)
  val basename : t -> CU.Name.t

  (** This function will cause a fatal error on a parameter import: a
      parameter is named by a [CU.Intf.t], not a [CU.t]. Use [view] to handle
      parameter imports. *)
  val name : t -> CU.t

  val info : t -> Nonalias.t option

  val crc : t -> Digest.t option

  val has_name : t -> name:CU.t -> bool

  val dummy : t
end

module Impl : sig
  type nonrec t = t

  (** The import info for an implementation we depend on and whose .cmx we
      actually loaded. *)
  val create_loaded : CU.t -> crc:Digest.t -> t

  (** The import info for an implementation we depend on but for which we never
      loaded a .cmx (and thus have no CRC for). *)
  val create_unloaded : CU.t -> t

  (** [create cu ~crc] is [create_loaded] if [crc] is [Some] and
      [create_unloaded] if [crc] is [None]. Useful when [crc] is coming out of
      [Consistbl]. *)
  val create : CU.t -> crc:Digest.t option -> t

  val name : t -> CU.t

  val cu : t -> CU.t

  val crc : t -> Digest.t option

  val dummy : t
end
