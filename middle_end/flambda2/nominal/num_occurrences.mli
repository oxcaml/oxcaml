(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019--2020 OCamlPro SAS                                    *)
(*   Copyright 2019--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Zero
  | One
  | More_than_one

val print : Format.formatter -> t -> unit

(** The numbers of occurrences of a bound variable within the scope of its
    binder, by name mode. (Occurrences at mode [In_types] do not arise in
    terms.) *)
module By_name_mode : sig
  type num_occurrences := t

  type t =
    { normal : num_occurrences;
      phantom : num_occurrences
    }

  val print : Format.formatter -> t -> unit

  (** Whether a variable with the given occurrence counts within its scope is
      needed by phantom lets: it has at least one occurrence at phantom name
      mode, and is of a kind whose values the defining expressions of phantom
      lets can evaluate (so variables of kind [Region] or [Rec_info] are never
      needed). Such a variable must remain locatable by the debugger. *)
  val needed_by_phantom_let : t -> Variable.t -> bool

  (** As for [needed_by_phantom_let], but looking up the occurrence counts in
      the given map (e.g. as provided by [Flambda.Let.pattern_match']); [false]
      if the variable is absent from the map. *)
  val needed_by_phantom_let_in_map : t Variable.Map.t -> Variable.t -> bool
end
