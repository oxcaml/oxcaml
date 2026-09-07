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

module K = Flambda_kind
module TI = Targetint_32_64

type t =
  { condition_dbg : Debuginfo.t;
    scrutinee_kind : K.Standard_int.t;
    scrutinee : Simple.t;
    arms : Apply_cont_expr.t TI.Map.t
  }

let fprintf = Format.fprintf

let print_arms ppf arms =
  let arms =
    TI.Map.fold
      (fun discr action arms_inverse ->
        match Apply_cont_expr.Map.find action arms_inverse with
        | exception Not_found ->
          Apply_cont_expr.Map.add action (TI.Set.singleton discr) arms_inverse
        | discrs ->
          Apply_cont_expr.Map.add action (TI.Set.add discr discrs) arms_inverse)
      arms Apply_cont_expr.Map.empty
  in
  let spc = ref false in
  let arms =
    List.sort
      (fun (action1, discrs1) (action2, discrs2) ->
        let min1 = TI.Set.min_elt_opt discrs1 in
        let min2 = TI.Set.min_elt_opt discrs2 in
        match min1, min2 with
        | None, None -> Apply_cont_expr.compare action1 action2
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some min1, Some min2 -> TI.compare min1 min2)
      (Apply_cont_expr.Map.bindings arms)
  in
  List.iter
    (fun (action, discrs) ->
      if !spc then fprintf ppf "@ " else spc := true;
      let discrs = TI.Set.elements discrs in
      fprintf ppf "@[<hov 2>@[<hov 0>| %a %t\u{21a6}%t@ @]%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ | ")
           TI.print)
        discrs Flambda_colours.elide Flambda_colours.pop Apply_cont_expr.print
        action)
    arms

let print_scrutinee_kind ppf (kind : K.Standard_int.t) =
  (* Switches on naked immediates are by far the most common case, so the kind
     is not printed for them. *)
  match kind with
  | Naked_immediate -> ()
  | Tagged_immediate | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
  | Naked_nativeint ->
    fprintf ppf "%t%a%t " Flambda_colours.kind K.Standard_int.print_lowercase
      kind Flambda_colours.pop

let print ppf { condition_dbg; scrutinee_kind; scrutinee; arms } =
  fprintf ppf "@[<v 0>(%tswitch%t %a%a%s%t%a%t@ @[<v 0>%a@])@]"
    Flambda_colours.expr_keyword Flambda_colours.pop print_scrutinee_kind
    scrutinee_kind Simple.print scrutinee
    (if Debuginfo.is_none condition_dbg then "" else " ")
    Flambda_colours.debuginfo Debuginfo.print_compact condition_dbg
    Flambda_colours.pop print_arms arms

let discriminant_width (scrutinee_kind : K.Standard_int.t) ~machine_width :
    Target_system.Machine_width.t =
  match scrutinee_kind with
  | Naked_int64 -> Sixty_four
  | Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32
  | Naked_nativeint ->
    machine_width

let check_discriminant (scrutinee_kind : K.Standard_int.t) discr =
  let in_range ~min ~max =
    let discr = TI.to_int64 discr in
    Int64.compare min discr <= 0 && Int64.compare discr max <= 0
  in
  let ok =
    match scrutinee_kind with
    | Tagged_immediate | Naked_immediate | Naked_nativeint ->
      (* The range of these kinds depends on the machine width, which is not
         available here. *)
      true
    | Naked_int8 -> in_range ~min:(-128L) ~max:127L
    | Naked_int16 -> in_range ~min:(-32768L) ~max:32767L
    | Naked_int32 ->
      in_range
        ~min:(Int64.of_int32 Int32.min_int)
        ~max:(Int64.of_int32 Int32.max_int)
    | Naked_int64 -> (
      (* See [discriminant_width]. *)
      match TI.repr discr with
      | Int64 _ -> true
      | Int32 _ -> false)
  in
  if not ok
  then
    Misc.fatal_errorf "Switch discriminant %a is out of range for kind %a"
      TI.print discr K.Standard_int.print_lowercase scrutinee_kind

let create ~condition_dbg ~scrutinee_kind ~scrutinee ~arms =
  if Flambda_features.check_invariants ()
  then TI.Map.iter (fun discr _ -> check_discriminant scrutinee_kind discr) arms;
  { condition_dbg; scrutinee_kind; scrutinee; arms }

let if_then_else ~machine_width ~condition_dbg ~scrutinee ~if_true ~if_false =
  let arms =
    TI.Map.of_list
      [TI.one machine_width, if_true; TI.zero machine_width, if_false]
  in
  create ~condition_dbg ~scrutinee_kind:Naked_immediate ~scrutinee ~arms

let iter t ~f = TI.Map.iter f t.arms

let num_arms t = TI.Map.cardinal t.arms

let condition_dbg t = t.condition_dbg

let scrutinee t = t.scrutinee

let scrutinee_kind t = t.scrutinee_kind

let arms t = t.arms

let const_of_discriminant ~machine_width (scrutinee_kind : K.Standard_int.t)
    discr =
  check_discriminant scrutinee_kind discr;
  match scrutinee_kind with
  | Tagged_immediate ->
    Reg_width_const.tagged_immediate
      (Target_ocaml_int.of_targetint machine_width discr)
  | Naked_immediate ->
    Reg_width_const.naked_immediate
      (Target_ocaml_int.of_targetint machine_width discr)
  | Naked_int8 ->
    Reg_width_const.naked_int8 (Numeric_types.Int8.of_int_exn (TI.to_int discr))
  | Naked_int16 ->
    Reg_width_const.naked_int16
      (Numeric_types.Int16.of_int_exn (TI.to_int discr))
  | Naked_int32 -> Reg_width_const.naked_int32 (TI.to_int32 discr)
  | Naked_int64 -> Reg_width_const.naked_int64 (TI.to_int64 discr)
  | Naked_nativeint -> Reg_width_const.naked_nativeint discr

let free_names { condition_dbg = _; scrutinee_kind = _; scrutinee; arms } =
  let free_names_of_scrutinee = Simple.free_names scrutinee in
  TI.Map.fold
    (fun _discr action free_names ->
      Name_occurrences.union (Apply_cont_expr.free_names action) free_names)
    arms free_names_of_scrutinee

let apply_renaming ({ condition_dbg; scrutinee_kind; scrutinee; arms } as t)
    renaming =
  let scrutinee' = Simple.apply_renaming scrutinee renaming in
  let arms' =
    TI.Map.map_sharing
      (fun action -> Apply_cont_expr.apply_renaming action renaming)
      arms
  in
  if scrutinee == scrutinee' && arms == arms'
  then t
  else { condition_dbg; scrutinee_kind; scrutinee = scrutinee'; arms = arms' }

let ids_for_export { condition_dbg = _; scrutinee_kind = _; scrutinee; arms } =
  let scrutinee_ids = Ids_for_export.from_simple scrutinee in
  TI.Map.fold
    (fun _discr action ids ->
      Ids_for_export.union ids (Apply_cont_expr.ids_for_export action))
    arms scrutinee_ids
