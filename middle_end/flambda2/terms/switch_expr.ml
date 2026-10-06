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

let scale_for_printing arms =
  let likelihoods =
    Target_ocaml_int.Map.fold
      (fun _ arm likelihoods -> Switch_arm.likelihood arm :: likelihoods)
      arms []
  in
  let uniform = Likelihood.is_uniform likelihoods in
  let total_likelihood = Likelihood.sum_list likelihoods in
  total_likelihood, ~uniform

type t =
  { condition_dbg : Debuginfo.t;
    scrutinee : Simple.t;
    arms : Switch_arm.t Target_ocaml_int.Map.t
  }

let fprintf = Format.fprintf

let print_probability ~uniform ppf (probability : Likelihood.classification) =
  match probability with
  | Cold -> Format.fprintf ppf "[cold]"
  | Weight p -> if not uniform then Format.fprintf ppf "[p = %f]" p

let print_arms ppf arms =
  let total, ~uniform = scale_for_printing arms in
  let arms =
    Target_ocaml_int.Map.fold
      (fun discr arm arms_inverse ->
        let probability =
          Likelihood.rescale ~total (Switch_arm.likelihood arm)
          |> Likelihood.classify
        in
        let action = Switch_arm.action arm in
        match Apply_cont_expr.Map.find action arms_inverse with
        | exception Not_found ->
          Apply_cont_expr.Map.add action
            (Target_ocaml_int.Map.singleton discr probability)
            arms_inverse
        | discrs ->
          Apply_cont_expr.Map.add action
            (Target_ocaml_int.Map.add discr probability discrs)
            arms_inverse)
      arms Apply_cont_expr.Map.empty
  in
  let spc = ref false in
  let arms =
    List.sort
      (fun (action1, discrs1) (action2, discrs2) ->
        let min1 = Target_ocaml_int.Map.min_binding_opt discrs1 in
        let min2 = Target_ocaml_int.Map.min_binding_opt discrs2 in
        match min1, min2 with
        | None, None -> Apply_cont_expr.compare action1 action2
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some (min1, _), Some (min2, _) -> Target_ocaml_int.compare min1 min2)
      (Apply_cont_expr.Map.bindings arms)
  in
  List.iter
    (fun (action, discrs) ->
      if !spc then fprintf ppf "@ " else spc := true;
      let discrs = Target_ocaml_int.Map.bindings discrs in
      fprintf ppf "@[<hov 2>@[<hov 0>| %a %t\u{21a6}%t@ @]%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ | ")
           (fun ppf (discr, probability) ->
             Format.fprintf ppf "%a%t%a%t" Target_ocaml_int.print discr
               Flambda_colours.continuation_annotation
               (print_probability ~uniform)
               probability Flambda_colours.pop))
        discrs Flambda_colours.elide Flambda_colours.pop Apply_cont_expr.print
        action)
    arms

let print ppf { condition_dbg; scrutinee; arms } =
  fprintf ppf "@[<v 0>(%tswitch%t %a%s%t%a%t@ @[<v 0>%a@])@]"
    Flambda_colours.expr_keyword Flambda_colours.pop Simple.print scrutinee
    (if Debuginfo.is_none condition_dbg then "" else " ")
    Flambda_colours.debuginfo Debuginfo.print_compact condition_dbg
    Flambda_colours.pop print_arms arms

let create ~condition_dbg ~scrutinee ~arms = { condition_dbg; scrutinee; arms }

let if_then_else ~machine_width ~condition_dbg ~scrutinee ~if_true ~if_false =
  let arms =
    Target_ocaml_int.Map.of_list
      [ Target_ocaml_int.bool_true machine_width, if_true;
        Target_ocaml_int.bool_false machine_width, if_false ]
  in
  create ~condition_dbg ~scrutinee ~arms

let iter t ~f = Target_ocaml_int.Map.iter f t.arms

let num_arms t = Target_ocaml_int.Map.cardinal t.arms

let condition_dbg t = t.condition_dbg

let scrutinee t = t.scrutinee

let arms t = t.arms

let free_names { condition_dbg = _; scrutinee; arms } =
  let free_names_of_scrutinee = Simple.free_names scrutinee in
  Target_ocaml_int.Map.fold
    (fun _discr arm free_names ->
      Name_occurrences.union (Switch_arm.free_names arm) free_names)
    arms free_names_of_scrutinee

let apply_renaming ({ condition_dbg; scrutinee; arms } as t) renaming =
  let scrutinee' = Simple.apply_renaming scrutinee renaming in
  let arms' =
    Target_ocaml_int.Map.map_sharing
      (fun arm -> Switch_arm.apply_renaming arm renaming)
      arms
  in
  if scrutinee == scrutinee' && arms == arms'
  then t
  else { condition_dbg; scrutinee = scrutinee'; arms = arms' }

let ids_for_export { condition_dbg = _; scrutinee; arms } =
  let scrutinee_ids = Ids_for_export.from_simple scrutinee in
  Target_ocaml_int.Map.fold
    (fun _discr arm ids ->
      Ids_for_export.union ids (Switch_arm.ids_for_export arm))
    arms scrutinee_ids
