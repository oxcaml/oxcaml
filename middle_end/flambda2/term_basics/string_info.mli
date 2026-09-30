(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** What is known about a value of type [string] or [bytes]. *)
type t =
  | Immutable of string  (** A constant string with the given contents. *)
  | Mutable of { length : Target_ocaml_int.t }
      (** A string or bytes of the given length allocated at runtime, whose
          contents are unknown and may change. It is never physically equal to a
          constant string, and may be locally allocated. *)

include Container_types.S with type t := t
