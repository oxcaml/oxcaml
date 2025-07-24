(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Translate Lambda code to Cmm using Flambda 2.
    This function is not currently re-entrant. *)
val lambda_to_cmm :
  ppf_dump:Format.formatter ->
  prefixname:string ->
  keep_symbol_tables:bool ->
  Lambda.program ->
  Cmm.phrase list

type flambda_result =
  { flambda : Flambda_unit.t;
    all_code : Exported_code.t;
    offsets : Exported_offsets.t;
    reachable_names : Flambda2_nominal.Name_occurrences.t
  }

(** Translate Lambda code into (potentially Simplified) Flambda 2.
    This function is not currently re-entrant. *)
val lambda_to_flambda :
  ppf_dump:Format.formatter ->
  prefixname:string ->
  Lambda.program ->
  flambda_result

val reset_symbol_tables : unit -> unit

val get_module_info :
  Compilation_unit.t -> Flambda2_cmx.Flambda_cmx_format.t option
