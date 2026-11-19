(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Handling of permutations and import freshening upon all kinds of bindable
    names and other identifiers (e.g. constants).

    We use permutations instead of substitutions because they cannot
    accidentally disturb the binding structure of terms. See [Name_abstraction].

    Unlike [Name_occurrences] this module does not segregate names according to
    where they occur (e.g. in terms or in types). *)

module Simple = Int_ids.Simple

type t

val empty : t

val print : Format.formatter -> t -> unit

val is_identity : t -> bool

val create_import_map :
  symbols:Symbol.importer ->
  variables:Variable.importer ->
  simples:Simple.importer ->
  consts:Reg_width_const.importer ->
  code_ids:Code_id.importer ->
  continuations:Continuation.importer ->
  fields:Field.importer ->
  used_value_slots:Value_slot.Set.t ->
  original_compilation_unit:Compilation_unit.t ->
  t

val has_import_map : t -> bool

(** Note that [compose] is not commutative on the permutation component. The
    permutation in the result of [compose ~second ~first] is that permutation
    acting initially like [first] then subsequently like [second]. [second] must
    not hold any import map. *)
val compose : second:t -> first:t -> t

val add_variable : t -> Variable.t -> Variable.t -> t

val add_fresh_variable : t -> Variable.t -> guaranteed_fresh:Variable.t -> t

val bind_variable : t -> Variable.t -> t * Variable.t

val apply_variable : t -> Variable.t -> Variable.t

val apply_variable_set : t -> Variable.Set.t -> Variable.Set.t

val apply_variable_map :
  ('a -> t -> 'a) -> t -> 'a Variable.Map.t -> 'a Variable.Map.t

(* This is only used by the importing code. We don't permute symbols. *)
val apply_symbol : t -> Symbol.t -> Symbol.t

val apply_symbol_set : t -> Symbol.Set.t -> Symbol.Set.t

val apply_name : t -> Name.t -> Name.t

val apply_name_set : t -> Name.Set.t -> Name.Set.t

val apply_name_map : ('a -> t -> 'a) -> t -> 'a Name.Map.t -> 'a Name.Map.t

val add_continuation : t -> Continuation.t -> Continuation.t -> t

val add_fresh_continuation :
  t -> Continuation.t -> guaranteed_fresh:Continuation.t -> t

val bind_continuation : t -> Continuation.t -> t * Continuation.t

val apply_continuation : t -> Continuation.t -> Continuation.t

val apply_continuation_set : t -> Continuation.Set.t -> Continuation.Set.t

val apply_continuation_map :
  ('a -> t -> 'a) -> t -> 'a Continuation.Map.t -> 'a Continuation.Map.t

(* This is only used by the importing code. We don't permute code ids. *)
val apply_code_id : t -> Code_id.t -> Code_id.t

val apply_code_id_set : t -> Code_id.Set.t -> Code_id.Set.t

val apply_code_id_map :
  ('a -> t -> 'a) -> t -> 'a Code_id.Map.t -> 'a Code_id.Map.t

val apply_code_id_or_name : t -> Code_id_or_name.t -> Code_id_or_name.t

val apply_code_id_or_name_map :
  ('a -> t -> 'a) -> t -> 'a Code_id_or_name.Map.t -> 'a Code_id_or_name.Map.t

(* This is only used by the importing code. We don't permute fields. *)
val apply_field : t -> Field.t -> Field.t

(* This is only used by the importing code. We don't permute constants. *)
val apply_const : t -> Reg_width_const.t -> Reg_width_const.t

val apply_simple : t -> Simple.t -> Simple.t

val value_slot_is_used : t -> Value_slot.t -> bool

val not_renamed : 'a -> t -> 'a

val apply_set :
  fold:(('key -> 'set -> 'set) -> 'set -> 'set -> 'set) ->
  add:('key -> 'set -> 'set) ->
  empty:'set ->
  rename_key:(t -> 'key -> 'key) ->
  t ->
  'set ->
  'set

val apply_map :
  fold:(('key -> 'value -> 'map -> 'map) -> 'map -> 'map -> 'map) ->
  add:('key -> 'value -> 'map -> 'map) ->
  empty:'map ->
  rename_key:(t -> 'key -> 'key) ->
  rename_value:('value -> t -> 'value) ->
  t ->
  'map ->
  'map
