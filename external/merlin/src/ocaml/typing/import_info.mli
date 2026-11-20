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

val create : CU.Name.t -> crc:string option -> t

val create_normal : CU.t -> crc:string option -> t

val name : t -> CU.Name.t

(** This function will cause a fatal error if the supplied value of type [t] is
    an interface import. *)
val cu : t -> CU.t

val crc : t -> string option

val has_name : t -> name:CU.Name.t -> bool

val dummy : t

val print : Format_doc.formatter -> t -> unit

(** The preferred API to use for interface imports. *)
module Intf : sig
  type nonrec t = t

  (** The import info for an interface whose .cmi we loaded. *)
  val create_loaded : CU.Name.t -> crc:Digest.t -> t

  (** The import info for an interface we mention but never loaded, such as the
      target of a module alias. *)
  val create_alias : CU.Name.t -> t

  (** [create name ~crc] is [create_loaded] if [crc] is [Some] and
      [create_alias] if [crc] is [None]. Useful when [crc] is coming out of
      [Consistbl]. *)
  val create : CU.Name.t -> crc:Digest.t option -> t

  val name : t -> CU.Name.t

  val crc : t -> Digest.t option

  val has_name : t -> name:CU.Name.t -> bool

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

  val name : t -> CU.Name.t

  val cu : t -> CU.t

  val crc : t -> Digest.t option

  val dummy : t
end
