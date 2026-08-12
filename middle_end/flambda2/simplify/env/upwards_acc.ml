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

module ART = Are_rebuilding_terms
module DA = Downwards_acc
module DE = Downwards_env
module LCS = Lifted_constant_state
module TE = Flambda2_types.Typing_env
module UE = Upwards_env

type t =
  { uenv : UE.t;
    creation_dacc : DA.t;
    lifted_constants : LCS.t;
    all_code : Exported_code.t;
    slot_offsets : Slot_offsets.t Or_unknown.t;
    flow_result : Flow_types.Flow_result.t;
    resimplify : bool
  }

let [@ocamlformat "disable"] print ppf
      { uenv; creation_dacc = _; lifted_constants;
        all_code = _; slot_offsets; flow_result; resimplify;
      } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(uenv@ %a)@]@ \
      @[<hov 1>(lifted_constants@ %a)@]@ \
      @[<hov 1>(slot_offsets@ %a@)@]@ \
      @[<hov 1>(flow_result@ %a)@]\
      %a\
      )@]"
    UE.print uenv
    LCS.print lifted_constants
    (Or_unknown.print Slot_offsets.print) slot_offsets
    Flow_types.Flow_result.print flow_result
    (if resimplify then
       (fun ppf () -> Format.fprintf ppf "@ @[<hov 1>(should_resimplify)@]")
     else
       (fun _ppf () -> ())) ()

let create ~flow_result ~compute_slot_offsets uenv dacc =
  let slot_offsets : _ Or_unknown.t =
    if compute_slot_offsets then Known Slot_offsets.empty else Unknown
  in
  { uenv;
    creation_dacc = dacc;
    lifted_constants = LCS.empty;
    all_code = Exported_code.empty;
    (* [used_value_slots] must be kept separate from the normal free names
       tracking in [name_occurrences], since it is always accumulated, and never
       saved and restored (like free name information is when dealing with a
       [Let_cont]). *)
    slot_offsets;
    flow_result;
    resimplify = false
  }

let creation_dacc t = t.creation_dacc

let uenv t = t.uenv

let code_age_relation t = TE.code_age_relation (DA.typing_env t.creation_dacc)

let lifted_constants t = t.lifted_constants

let get_and_clear_lifted_constants t =
  { t with lifted_constants = LCS.empty }, t.lifted_constants

let add_lifted_constant t const =
  { t with lifted_constants = LCS.add t.lifted_constants const }

let are_rebuilding_terms t = DE.are_rebuilding_terms (DA.denv t.creation_dacc)

let with_lifted_constants t lifted_constants = { t with lifted_constants }

let no_lifted_constants t = LCS.is_empty t.lifted_constants

let map_uenv t ~f = { t with uenv = f t.uenv }

let with_uenv t uenv = { t with uenv }

let remember_code_for_cmx t code =
  if ART.do_not_rebuild_terms (are_rebuilding_terms t)
  then t
  else
    let keep_code code_id =
      Code_id.Set.mem code_id (DA.code_ids_to_remember t.creation_dacc)
    in
    let all_code = Exported_code.add_code ~keep_code code t.all_code in
    { t with all_code }

let all_code t = t.all_code

let used_value_slots t = DA.used_value_slots t.creation_dacc

let shareable_constants t = DA.shareable_constants t.creation_dacc

let generate_phantom_lets t = DE.generate_phantom_lets (DA.denv t.creation_dacc)

let is_demoted_exn_handler t cont =
  Continuation.Set.mem cont (DA.demoted_exn_handlers t.creation_dacc)

let slot_offsets t = t.slot_offsets

let with_slot_offsets t slot_offsets = { t with slot_offsets }

let required_names t = t.flow_result.data_flow_result.required_names

let reachable_code_ids t = t.flow_result.data_flow_result.reachable_code_ids

let continuation_param_aliases t = t.flow_result.aliases_result

let mutable_unboxing_result t = t.flow_result.mutable_unboxing_result

let set_resimplify t = { t with resimplify = true }

let resimplify t = t.resimplify

let specialization_map t = DA.specialization_map (creation_dacc t)
