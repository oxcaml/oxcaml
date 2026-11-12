(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Simple = Int_ids.Simple

type t = private
  { symbols : Symbol.Set.t;
    variables : Variable.Set.t;
    simples : Simple.Set.t;
    consts : Reg_width_const.Set.t;
    code_ids : Code_id.Set.t;
    continuations : Continuation.Set.t;
    fields : Field.Set.t
  }

val empty : t

val create :
  ?symbols:Symbol.Set.t ->
  ?variables:Variable.Set.t ->
  ?simples:Simple.Set.t ->
  ?consts:Reg_width_const.Set.t ->
  ?code_ids:Code_id.Set.t ->
  ?continuations:Continuation.Set.t ->
  ?fields:Field.Set.t ->
  unit ->
  t

val singleton_variable : Variable.t -> t

val singleton_code_id : Code_id.t -> t

val singleton_continuation : Continuation.t -> t

val singleton_symbol : Symbol.t -> t

val singleton_field : Field.t -> t

val from_simple : Simple.t -> t

val add_variable : t -> Variable.t -> t

val add_symbol : t -> Symbol.t -> t

val add_name : t -> Name.t -> t

val add_const : t -> Reg_width_const.t -> t

val add_simple : t -> Simple.t -> t

val add_code_id : t -> Code_id.t -> t

val add_code_id_or_name : t -> Code_id_or_name.t -> t

val add_continuation : t -> Continuation.t -> t

val add_field : t -> Field.t -> t

val union : t -> t -> t

val union_list : t list -> t

val no_ids : 'a -> t

val ids_for_export_set :
  fold:(('key -> t -> t) -> 'set -> t -> t) ->
  add_ids_key:(t -> 'key -> t) ->
  'set ->
  t

val ids_for_export_map :
  fold:(('key -> 'value -> t -> t) -> 'map -> t -> t) ->
  add_ids_key:(t -> 'key -> t) ->
  ids_for_export_value:('value -> t) ->
  'map ->
  t

val ids_for_export_name_set : Name.Set.t -> t

val ids_for_export_code_id_set : Code_id.Set.t -> t

val ids_for_export_continuation_set : Continuation.Set.t -> t

val ids_for_export_variable_map : ('a -> t) -> 'a Variable.Map.t -> t

val ids_for_export_name_map : ('a -> t) -> 'a Name.Map.t -> t

val ids_for_export_code_id_map : ('a -> t) -> 'a Code_id.Map.t -> t

val ids_for_export_continuation_map : ('a -> t) -> 'a Continuation.Map.t -> t

val ids_for_export_code_id_or_name_map :
  ('a -> t) -> 'a Code_id_or_name.Map.t -> t

(** Tables of int_ids. *)
module Table_data : sig
  type ids := t

  type t

  val create : ids -> t

  val create_import_map :
    t ->
    used_value_slots:Value_slot.Set.t ->
    original_compilation_unit:Compilation_unit.t ->
    Renaming.t
end

(** Snapshots of the stamp counters used to create identifiers. *)
module Stamp_state : sig
  type t

  val save : unit -> t

  (** Restore all stamp counters to the values they had when [save] was called
      in the process that serialised this value. This can only be called once,
      before any stamps have been created, and will error otherwise. *)
  val restore : t -> unit

  val merge : t -> t -> t
end
