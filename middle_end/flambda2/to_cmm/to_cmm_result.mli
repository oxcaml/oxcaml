(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Result structure used during Flambda to Cmm translation. *)

(** An accumulator for the Cmm phrases and GC roots that result from translating
    statically-allocated blocks and function bodies.

    Values of type [t] contain both older, "archived" data and "current" data
    which is still being worked on. The [archive_data] function transfers
    current data into the archived part, clearing the current data area for
    subsequent use. *)
type t

(** Create a result structure.

    [reachable_names] specifies which names are reachable from outside the
    compilation unit (same terminology as used in [Flambda_cmx]). *)
val create : module_symbol:Symbol.t -> reachable_names:Name_occurrences.t -> t

(** Translate an existing [Symbol.t] to a Cmm symbol. *)
val symbol : t -> Symbol.t -> Cmm.symbol

(** Produce the Cmm function symbol for a piece of code. *)
val symbol_of_code_id :
  t -> Code_id.t -> currently_in_inlined_body:bool -> Cmm.symbol

(** Create a Cmm symbol, not arising from a [Symbol.t]. *)
val raw_symbol : t -> global:Cmm.is_global -> string -> t * Cmm.symbol

(** Archive the current data into the list of already-translated data. *)
val archive_data : t -> t

(** Add already-translated Cmm data items into the archived part of the result
    structure. *)
val add_archive_data_items : t -> Cmm.data_item list -> t

(** Update the current data part of the result structure. *)
val update_data : t -> (Cmm.data_item list -> Cmm.data_item list) -> t

(** Set the current data to the given list. Raises a fatal error if the current
    data is not empty, to avoid inadvertently losing data. *)
val set_data : t -> Cmm.data_item list -> t

(** Register one or more symbols as required GC roots. *)
val add_gc_roots : t -> Symbol.t list -> t

(** Register a function that has been translated to Cmm. *)
val add_function : t -> Cmm.fundecl -> t

(** {2 FDO counters without edges}

    Some FDO counters lose the edge they were attached to in Cmm, e.g. those of
    the arms of a switch that becomes a table lookup. As in Simplify (see
    [Fdo_region_counters]), they are attached instead to the edges into their
    region: the stretch of straight-line code they are in, which starts at a
    function entry, a switch arm or a continuation handler. A region that jumps
    to a handler gets the counters of the handler, which are known if the
    handler was translated before the jump. *)

(* CR yusumez: The jumps translated before their handler, such as the back edges
   of a loop, miss the counters of the handler, as in Simplify. *)

(** The counters of an enclosing region, put aside while translating a region in
    it. *)
type fdo_region

(** Start translating a region. *)
val enter_fdo_region : t -> t * fdo_region

(** Stop translating the region, returning the counters to attach to the edges
    into it. *)
val leave_fdo_region : t -> fdo_region -> t * Fdo_annotation.t

(** The given counters lost their edges in the region being translated. *)
val add_fdo_region_counters : t -> Fdo_annotation.t -> t

(** Record the counters to attach to the edges into the handler of the given
    continuation. *)
val set_fdo_handler_counters : t -> Continuation.t -> Fdo_annotation.t -> t

val fdo_handler_counters : t -> Continuation.t -> Fdo_annotation.t

(** Record the given symbol as having been defined. This is used to keep track
    of whether the module block symbol for the current unit has been defined. *)
val check_for_module_symbol : t -> Symbol.t -> t

(** Caching of symbols associated with [Invalid] messages. *)
val add_invalid_message_symbol : t -> Symbol.t -> message:string -> t

val invalid_message_symbol : t -> message:string -> Symbol.t option

type result = private
  { data_items : Cmm.phrase list;
    gc_roots : Cmm.symbol list;
    functions : Cmm.phrase list
  }

(** Archive the current data and then return the translated data present in the
    given result structure. *)
val to_cmm : t -> result
