(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Pierre Chambart and Vincent Laviron, OCamlPro               *)
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

(** Approximations used for cross-module inlining in Closure_conversion *)

type 'code t =
  | Unknown of Flambda_kind.t
  | Value_symbol of Symbol.t
  | Value_const of Reg_width_const.t
  | Closure_approximation of
      { code_id : Code_id.t;
        function_slot : Function_slot.t;
        code : 'code;
        symbol : Symbol.t option
      }
  | Block_approximation of
      Tag.Scannable.t
      * Flambda_kind.Scannable_block_shape.t
      * 'code t array
      * Alloc_mode.For_types.t

val print : Format.formatter -> 'a t -> unit

val is_unknown : 'a t -> bool

val free_names :
  code_free_names:('code -> Name_occurrences.t) -> 'code t -> Name_occurrences.t

(** The compilation units referenced by the given free names (via their symbols
    and code IDs), excluding the pseudo-units of predefined-exception and
    external symbols, which have no cmx data. Used with the free names of
    approximations (see [free_names]) to record which units' cmx data must
    accompany a reified ([%reify_approx]) approximation; see
    [Cmx_format.ui_quoted_cmx]. *)
val compilation_units_of_free_names :
  Name_occurrences.t -> Compilation_unit.Set.t

(** A self-contained ("standalone") form of approximations, which names none of
    the hash-consed types. Such values can be marshalled (e.g. into object
    files) and read back in a different compilation, or by another process
    entirely. (Types such as [Flambda_kind.t] that are ordinary immutable data
    structures are kept as-is; they marshal fine.)

    Compilation units are represented by their pack prefix and name; symbols by
    their linkage name plus compilation unit; code IDs by their name, linkage
    name and compilation unit (stamps are not preserved as such, but the linkage
    name incorporates the stamp and so permits exact lookup of exported code at
    demarshalling time); function slots by just their name (stamps not
    preserved). The ['code] payload of [Closure_approximation] is not saved: it
    is expected to be recovered at demarshalling time, e.g. by looking up the
    code from the symbol or the code ID's linkage name. *)
module Standalone : sig
  type 'code approx = 'code t

  type compilation_unit =
    { pack_prefix : string list;
      name : string
    }

  type symbol =
    { symbol_compilation_unit : compilation_unit;
      linkage_name : string
    }

  type code_id =
    { code_id_compilation_unit : compilation_unit;
      code_id_name : string;
      code_id_linkage_name : string
    }

  type t =
    | Unknown of Flambda_kind.t
    | Value_symbol of symbol
    | Value_const of Reg_width_const.Descr.t
    | Closure_approximation of
        { code_id : code_id;
          function_slot : string;
          symbol : symbol option;
          lookup_symbol : symbol option
              (** A symbol under which the closure's full approximation was
                  registered in its unit's cmx data (manufactured by
                  [Closure_conversion] for closures that have no symbol of their
                  own). Looking it up at demarshalling time recovers the
                  original code ID and function slot, whose stamps are not
                  preserved in this standalone form. *)
        }
    | Block_approximation of
        Tag.Scannable.t
        * Flambda_kind.Scannable_block_shape.t
        * t array
        * Alloc_mode.For_types.t

  (** Note that the ['code] payload of any [Closure_approximation] is discarded
      (see above). [closure_lookup_symbols] gives, per code ID, the symbol under
      which the corresponding closure approximation was registered in the
      current unit's cmx data (see [lookup_symbol] above). *)
  val create :
    closure_lookup_symbols:Symbol.t Code_id.Map.t -> 'code approx -> t

  type 'code closure_resolution =
    | Resolved of 'code approx
        (** A full replacement approximation, e.g. recovered wholesale from the
            original unit's cmx data via [lookup_symbol] or [symbol]; the code
            IDs and function slots therein are the original ones. *)
    | Code of Code_id.t * 'code
        (** Code (typically found by linkage name); the code ID given is used in
            place of the freshly created one, but the function slot remains
            freshly created (with a new stamp). *)
    | Unknown_code

  (** Reconstruct a normal approximation, using the usual creation functions for
      the various hash-consed types. Code IDs and function slots are created
      afresh, so their stamps will not match those of the original
      approximation; [find_code] resolves each [Closure_approximation],
      typically by consulting the original unit's cmx data via [lookup_symbol]
      or [symbol] (yielding [Resolved], with original IDs), or by looking the
      code up by [code_id_linkage_name] (the original code ID's full linkage
      name), yielding [Code]. [Unknown_code] degrades the closure approximation
      to [Unknown]. *)
  val to_approximation :
    t ->
    find_code:
      (code_id:Code_id.t ->
      code_id_linkage_name:Linkage_name.t ->
      function_slot:Function_slot.t ->
      symbol:Symbol.t option ->
      lookup_symbol:Symbol.t option ->
      'code closure_resolution) ->
    'code approx

  val to_marshalled_string : t -> string

  val of_marshalled_string : string -> t
end
