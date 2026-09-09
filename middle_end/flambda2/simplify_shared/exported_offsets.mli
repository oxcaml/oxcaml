(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Vincent Laviron and Guillaume Bury, OCamlPro              *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Public state to store the mapping from elements of a closure to offset. *)
type t

type words = int

type function_slot_info =
  | Dead_function_slot
  | Live_function_slot of
      { offset : words;
        size : words
            (* Number of fields taken for the function:

               2 fields (code pointer + arity) for function of arity one

               3 fields (caml_curry + arity + code pointer) otherwise *)
      }

type value_slot_info =
  | Dead_value_slot
  | Live_value_slot of
      { offset : words;
        size : words;
        is_scanned : bool
      }

(** The empty environment *)
val empty : t

(** Printing function for environment. *)
val print : Format.formatter -> t -> unit

(** Returns the offset computed for a value slot, in terms of target
    architecture words.

    If [None] is returned, there is no set of closures in the program containing
    the given value slot. *)
val value_slot_offset : t -> Value_slot.t -> value_slot_info option

(** Returns the offset computed for a function slot, in terms of target
    architecture words.

    This points to the first field of the representation of the function slot
    within the Closure_tag block. Notably, if the offset is not 0, an infix
    header must be placed just before the returned offset.

    If [None] is returned, there is no set of closures in the program containing
    the given function slot. *)
val function_slot_offset : t -> Function_slot.t -> function_slot_info option

(** Record the assignment of the given offset to the given function slot *)
val add_function_slot_offset : t -> Function_slot.t -> function_slot_info -> t

(** Record the assignment of the given offset to the given value slot *)
val add_value_slot_offset : t -> Value_slot.t -> value_slot_info -> t

val map_function_slot_offsets :
  t -> (Function_slot.t -> function_slot_info -> 'a) -> 'a Function_slot.Map.t

(** Build maps from the underlying data *)
val map_value_slot_offsets :
  t -> (Value_slot.t -> value_slot_info -> 'a) -> 'a Value_slot.Map.t

(** Take the offsets read from a cmx file and add them to the current state *)
val import_offsets : t -> unit

(** Return all the offsets read from cmx files so far *)
val imported_offsets : unit -> t

(** Merge the offsets from two files *)
val merge : t -> t -> t

(** Keep only the offsets of slots whose compilation unit satisfies [keep]. *)
val filter_by_compilation_unit : t -> keep:(Compilation_unit.t -> bool) -> t

(** Ensure the offsets for the given function slots are in the given exported
    offsets. [is_local] says whether slots of the given compilation unit have
    their offsets computed by the current process (rather than imported); such
    slots are skipped. *)
val reexport_function_slots :
  is_local:(Compilation_unit.t -> bool) -> Function_slot.Set.t -> t -> t

(** Ensure the offsets for the given value slots are in the given exported
    offsets. See [reexport_function_slots] regarding [is_local]. *)
val reexport_value_slots :
  is_local:(Compilation_unit.t -> bool) -> Value_slot.Set.t -> t -> t
