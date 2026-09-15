(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A symbol identifies a piece of statically-allocated data. The linkage name
    must be unique across the whole program. *)

include module type of struct
  include Int_ids.Symbol
end

val is_predefined_exception : t -> bool

(** Same as [create] but follows [Flambda_features.Expert.shorten_symbol_names]
    and uses a specific global counter. *)
val manufacture : Compilation_unit.t -> string -> t

(** The current value of the counter used by [manufacture]. *)
val export_manufacture_counter : unit -> int

(** Set the counter used by [manufacture]. This can only be called before any
    symbol has been manufactured in the current process, and will error
    otherwise. *)
val restore_manufacture_counter : int -> unit
