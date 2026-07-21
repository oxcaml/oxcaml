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

(** A self-contained ("standalone") form of approximations, which names none of
    the hash-consed types. Such values can be marshalled (e.g. into object
    files) and read back in a different compilation, or by another process
    entirely. (Types such as [Flambda_kind.t] that are ordinary immutable data
    structures are kept as-is; they marshal fine.)

    Compilation units are represented by their pack prefix and name; symbols by
    their linkage name plus compilation unit; code IDs by their name plus
    compilation unit (stamps are not preserved); function slots by just their
    name (stamps likewise not preserved). The ['code] payload of
    [Closure_approximation] is not saved: it is expected to be recovered at
    demarshalling time, e.g. by looking up the code from the symbol. *)
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
      code_id_name : string
    }

  type t =
    | Unknown of Flambda_kind.t
    | Value_symbol of symbol
    | Value_const of Reg_width_const.Descr.t
    | Closure_approximation of
        { code_id : code_id;
          function_slot : string;
          symbol : symbol option
        }
    | Block_approximation of
        Tag.Scannable.t
        * Flambda_kind.Scannable_block_shape.t
        * t array
        * Alloc_mode.For_types.t

  (** Note that the ['code] payload of any [Closure_approximation] is discarded
      (see above). *)
  val create : 'code approx -> t

  (** Reconstruct a normal approximation, using the usual creation functions for
      the various hash-consed types. Code IDs and function slots are created
      afresh, so their stamps will not match those of the original
      approximation. [find_code] is used to recover the ['code] payload of any
      [Closure_approximation] (typically by looking up the code from the
      symbol); if it returns [None], the closure approximation degrades to
      [Unknown]. *)
  val to_approximation :
    t ->
    find_code:
      (code_id:Code_id.t ->
      function_slot:Function_slot.t ->
      symbol:Symbol.t option ->
      'code option) ->
    'code approx

  val to_marshalled_string : t -> string

  val of_marshalled_string : string -> t
end
