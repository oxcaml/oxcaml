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

type stamp_state

val export_stamp_state : unit -> stamp_state

(** Set the counter used by [manufacture]. This can only be called before any
    symbol has been manufactured in the current process, and will error
    otherwise. *)
val restore_stamp_state : stamp_state -> unit

val merge_stamp_state : stamp_state -> stamp_state -> stamp_state
