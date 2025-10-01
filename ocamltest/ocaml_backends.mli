(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Backends of the OCaml compiler and their properties *)

type t = Native | Bytecode | Javascript

module BackendSet : Set.S with type elt = t

val is_bytecode : t -> bool

val is_native : t -> bool

val is_javascript : t -> bool

val string_of_backend : t -> string

val make_backend_function : 'a -> 'a -> 'a -> t -> 'a

val module_extension : t -> string

val library_extension : t -> string

val executable_extension : t -> string

(* Backend filtering *)
val parse_backends_string : string -> BackendSet.t

val set_enabled_backends : BackendSet.t -> unit

val is_backend_enabled : t -> bool
