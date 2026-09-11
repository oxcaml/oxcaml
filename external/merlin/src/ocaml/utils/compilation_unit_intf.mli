(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Zesen Qian, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of the names of compilation unit interfaces.

   While a compilation unit ([Compilation_unit.t]) represents a reference to
   an implementation (a .cmo or .cmx file), a compilation unit interface
   stands for a reference to a compiled interface (a .cmi file).

   Currently each file is not allowed to refer to prefixed modules.

   CR-soon zqian: add prefix.
*)

[@@@ocaml.warning "+a-9-40-41-42"]

(** The name of a compilation unit interface. *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

val of_string : string -> t

val to_string : t -> string

(** A placeholder for interfaces that do not have a valid name, as during
    initialisation of the compiler. *)
val dummy : t

(** The name of the distinguished interface for predefined exceptions. *)
val predef_exn : t

val print : Format_doc.formatter -> t -> unit

(** Print the name for use in error messages. Uses [Format_doc.formatter] and
    applies inline code styling. *)
val print_as_inline_code : Format_doc.formatter -> t -> unit

(** An interface together with the path to its .cmi file, when known. Most
    users should traffic in plain [t]; a [Found.t] additionally remembers
    where the .cmi was (or will be) located, so that later lookups can try
    that path before searching the load path. The path is advisory only: it is
    ignored by [compare], [equal] and [hash]. *)
module Found : sig
  type cui := t

  type t

  (** Create with the path of the interface's .cmi file. *)
  val create : cui -> cmi_path:Misc.filepath -> t

  (** Create when the path of the interface's .cmi file is not known. *)
  val without_cmi_path : cui -> t

  val intf : t -> cui

  (** Replace the attached .cmi path. *)
  val with_cmi_path : t -> Misc.filepath -> t

  (** The path of the .cmi file, if known. *)
  val cmi_path : t -> Misc.filepath option

  (** Printing, comparison, sets, maps, etc.; all ignore the path. *)
  include Identifiable.S with type t := t

  val print : Format_doc.formatter -> t -> unit
end
