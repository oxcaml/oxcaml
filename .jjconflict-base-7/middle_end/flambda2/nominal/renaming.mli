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

val apply_variable : t -> Variable.t -> Variable.t

val apply_variable_set : t -> Variable.Set.t -> Variable.Set.t

(* This is only used by the importing code. We don't permute symbols. *)
val apply_symbol : t -> Symbol.t -> Symbol.t

val apply_symbol_set : t -> Symbol.Set.t -> Symbol.Set.t

val apply_name : t -> Name.t -> Name.t

val add_continuation : t -> Continuation.t -> Continuation.t -> t

val add_fresh_continuation :
  t -> Continuation.t -> guaranteed_fresh:Continuation.t -> t

val apply_continuation : t -> Continuation.t -> Continuation.t

(* This is only used by the importing code. We don't permute code ids. *)
val apply_code_id : t -> Code_id.t -> Code_id.t

val apply_code_id_or_name : t -> Code_id_or_name.t -> Code_id_or_name.t

(* This is only used by the importing code. We don't permute constants. *)
val apply_const : t -> Reg_width_const.t -> Reg_width_const.t

val apply_simple : t -> Simple.t -> Simple.t

val value_slot_is_used : t -> Value_slot.t -> bool
