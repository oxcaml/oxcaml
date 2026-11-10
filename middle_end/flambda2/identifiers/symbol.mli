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
    and uses a specific global counter. Under the structured name-mangling
    scheme, the symbol is named [<name>.{data}] at the level of the compilation
    unit, followed by the counter as a stamp. *)
val manufacture : Compilation_unit.t -> string -> t

(** Like [manufacture], for data bound to [var]. The flat name is the variable's
    unique name. Under the structured scheme, the name is the variable's name
    without its stamp, which the counter makes redundant. *)
val manufacture_for_variable : Compilation_unit.t -> Variable.t -> t

(** Like [manufacture], for the statically-allocated closure of [function_slot].
    [name] is the flat name. Under the structured scheme, the symbol is named
    after the closure's code, [<path of code_id>.{closure}], or after the
    function slot when the code is not known. *)
val manufacture_for_closure :
  Compilation_unit.t ->
  name:string ->
  function_slot:Function_slot.t ->
  code_id:Code_id.t option ->
  t
