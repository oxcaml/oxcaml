(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

<<<<<<< oxcaml
(* Trace-specific printing *)

(* A configuration type that controls which trace we print.  This could be
   exposed, but we instead expose three separate
   [report_{unification,equality,moregen}_error] functions.  This also lets us
   give the unification case an extra optional argument without adding it to the
   equality and moregen cases. *)
type 'variety trace_format =
  | Unification : Errortrace.unification trace_format
  | Equality    : Errortrace.comparison  trace_format
  | Moregen     : Errortrace.comparison  trace_format

let incompatibility_phrase (type variety) : variety trace_format -> string =
  function
  | Unification -> "is not compatible with type"
  | Equality    -> "is not equal to type"
  | Moregen     -> "is not compatible with type"

(* Print a unification error *)

let trees_of_trace mode =
  List.map (Errortrace.map_diff (trees_of_type_expansion mode))

let trees_of_type_path_expansion (tp,tp') =
  if Path.same tp tp' then Same(tree_of_path (Some Type) tp) else
    Diff(tree_of_path (Some Type) tp, tree_of_path (Some Type) tp')

let type_path_expansion ppf = function
  | Same p -> quoted_ident ppf p
  | Diff(p,p') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
       quoted_ident p
       quoted_ident p'

let rec trace fst txt ppf = function
  | {Errortrace.got; expected} :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@]%a"
       type_expansion got txt type_expansion expected
       (trace false txt) rem
  | _ -> ()

type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

let diff_printing_status Errortrace.{ got      = {ty = t1; expanded = t1'};
                                      expected = {ty = t2; expanded = t2'} } =
  if  is_constr_row ~allow_ident:true t1'
   || is_constr_row ~allow_ident:true t2'
  then Discard
  else if same_path t1 t1' && same_path t2 t2' then Optional_refinement
  else Keep

let printing_status = function
  | Errortrace.Diff d -> diff_printing_status d
  | Errortrace.Escape {kind = Constraint} -> Keep
  | _ -> Keep

(** Flatten the trace and remove elements that are always discarded
    during printing *)

(* Takes [printing_status] to change behavior for [Subtype] *)
let prepare_any_trace printing_status tr =
  let clean_trace x l = match printing_status x with
    | Keep -> x :: l
    | Optional_refinement when l = [] -> [x]
    | Optional_refinement | Discard -> l
  in
  match tr with
  | [] -> []
  | elt :: rem -> elt :: List.fold_right clean_trace rem []

let prepare_trace f tr =
  prepare_any_trace printing_status (Errortrace.map f tr)
(** Keep elements that are [Diff _ ] and split the the last element if it is
    optionally elidable, require a prepared trace *)
let rec filter_trace = function
  | [] -> [], None
  | [Errortrace.Diff d as elt]
    when printing_status elt = Optional_refinement -> [], Some d
  | Errortrace.Diff d :: rem ->
      let filtered, last = filter_trace rem in
      d :: filtered, last
  | _ :: rem -> filter_trace rem

let type_path_list ppf l =
  Fmt.pp_print_list ~pp_sep:(fun ppf () -> Fmt.pp_print_break ppf 2 0)
    type_path_expansion ppf l

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  match get_desc t with
  | Tvariant row ->
      let Row {fields; more; name; fixed; closed} = row_repr row in
      if name = None then t else
        (* CR rtjoa: Btype. ? *)
      newty2 ~level:(get_level t)
        (Tvariant
           (create_row ~fields ~fixed ~closed ~name:None
              ~more:(newvar2 (get_level more)
                       (Jkind.Builtin.value ~why:Row_variable))))
  | _ -> t

let prepare_expansion Errortrace.{ty; expanded} =
  let expanded = hide_variant_name expanded in
  Variable_names.reserve ty;
  if not (same_path ty expanded) then Variable_names.reserve expanded;
  Errortrace.{ty; expanded}

let may_prepare_expansion compact (Errortrace.{ty; expanded} as ty_exp) =
  match get_desc expanded with
    Tvariant _ | Tobject _ when compact ->
      reserve_names ty; Errortrace.{ty; expanded = ty}
  | _ -> prepare_expansion ty_exp

let print_path p =
  Fmt.dprintf "%a" !Oprint.out_ident (tree_of_path (Some Type) p)

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let print_tags ppf tags  =
  Fmt.(pp_print_list ~pp_sep:comma) print_tag ppf tags

let is_unit_arg env ty =
  let ty, vars = tpoly_get_poly ty in
  if vars <> [] then false
  else begin
    match get_desc (Ctype.expand_head env ty) with
    | Tconstr (p, _, _) -> Path.same p Predef.path_unit
    | _ -> false
  end

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explanation_diff env t3 t4 =
  match get_desc t3, get_desc t4 with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit_arg env ty1 && unifiable env ty2 t4 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to provide %a as argument?@]"
          Style.inline_code "()"
        )
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit_arg env ty1 && unifiable env t3 ty2 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to wrap the expression using \
           %a?@]"
          Style.inline_code "fun () ->"
        )
  | _ ->
      None

let explain_fixed_row_case = function
  | Errortrace.Cannot_be_closed -> doc_printf "it cannot be closed"
  | Errortrace.Cannot_add_tags tags ->
      doc_printf "it may not allow the tag(s) %a"
        print_tags tags

let explain_fixed_row pos expl = match expl with
  | Fixed_private ->
    doc_printf "The %a variant type is private" Errortrace.print_pos pos
  | Univar x ->
    reserve_names x;
    doc_printf "The %a variant type is bound to the universal type variable %a"
      Errortrace.print_pos pos
      (Style.as_inline_code type_expr_with_reserved_names) x
  | Reified p ->
    doc_printf "The %a variant type is bound to %a"
      Errortrace.print_pos pos
      (Style.as_inline_code
         (fun ppf p ->
           Internal_names.add p;
           print_path p ppf))
      p
  | Rigid -> Format_doc.Doc.empty
  | Fixed_existential -> Format_doc.Doc.empty

let explain_variant (type variety) : variety Errortrace.variant -> _ = function
  (* Common *)
  | Errortrace.Incompatible_types_for s ->
      Some(doc_printf "@,Types for tag %a are incompatible"
             print_tag s
          )
  (* Unification *)
  | Errortrace.No_intersection ->
      Some(doc_printf "@,These two variant types have no intersection")
  | Errortrace.No_tags(pos,fields) -> Some(
      doc_printf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        Errortrace.print_pos pos
        print_tags (List.map fst fields)
    )
  | Errortrace.Fixed_row (pos,
                          k,
                          (Univar _ | Reified _ | Fixed_private as e)) ->
      Some (
        doc_printf "@,@[%a,@ %a@]" pp_doc (explain_fixed_row pos e)
          pp_doc (explain_fixed_row_case k)
      )
  | Errortrace.Fixed_row (_,_, (Rigid | Fixed_existential)) ->
      (* this case never happens *)
      None
  (* Equality & Moregen *)
  | Errortrace.Presence_not_guaranteed_for (pos, s) -> Some(
      doc_printf
        "@,@[The tag %a is guaranteed to be present in the %a variant type,\
         @ but not in the %a@]"
        print_tag s
        Errortrace.print_pos (Errortrace.swap_position pos)
        Errortrace.print_pos pos
    )
  | Errortrace.Openness pos ->
      Some(doc_printf "@,The %a variant type is open and the %a is not"
             Errortrace.print_pos pos
             Errortrace.print_pos (Errortrace.swap_position pos))

let explain_escape pre = function
  | Errortrace.Univ u ->
      reserve_names u;
      Some(
        doc_printf "%a@,The universal variable %a would escape its scope"
          pp_doc pre
          (Style.as_inline_code type_expr_with_reserved_names) u
      )
  | Errortrace.Constructor p -> Some(
      doc_printf
        "%a@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        pp_doc pre (Style.as_inline_code path) p
    )
  | Errortrace.Module_type p -> Some(
      doc_printf
        "%a@,@[The module type@;<1 2>%a@ would escape its scope@]"
        pp_doc pre (Style.as_inline_code path) p
    )
  | Errortrace.Equation Errortrace.{ty = _; expanded = t} ->
      reserve_names t;
      Some(
        doc_printf "%a@ @[<hov>This instance of %a is ambiguous:@ %s@]"
          pp_doc pre
          (Style.as_inline_code type_expr_with_reserved_names) t
          "it would escape the scope of its equation"
      )
  | Errortrace.Self ->
      Some (doc_printf "%a@,Self type cannot escape its class" pp_doc pre)
  | Errortrace.Constraint ->
      None

let explain_object (type variety) : variety Errortrace.obj -> _ = function
  | Errortrace.Missing_field (pos,f) -> Some(
      doc_printf "@,@[The %a object type has no method %a@]"
        Errortrace.print_pos pos Style.inline_code f
    )
  | Errortrace.Abstract_row pos -> Some(
      doc_printf
        "@,@[The %a object type has an abstract row, it cannot be closed@]"
        Errortrace.print_pos pos
    )
  | Errortrace.Self_cannot_be_closed ->
      Some (doc_printf
              "@,Self type cannot be unified with a closed object type"
           )

let explain_incompatible_fields name (diff: Types.type_expr Errortrace.diff) =
  reserve_names diff.got;
  reserve_names diff.expected;
  doc_printf "@,@[The method %a has type@ %a,@ \
  but the expected method type was@ %a@]"
    Style.inline_code name
    (Style.as_inline_code type_expr_with_reserved_names) diff.got
    (Style.as_inline_code type_expr_with_reserved_names) diff.expected

let explanation (type variety) intro prev env
  : (Errortrace.expanded_type, variety) Errortrace.elt -> _ = function
  | Errortrace.Diff {got; expected} ->
    explanation_diff env got.expanded expected.expanded
  | Errortrace.Escape {kind; context} ->
    let pre =
      match context, kind, prev with
      | Some ctx, _, _ ->
        reserve_names ctx;
        doc_printf "@[%a@;<1 2>%a@]" pp_doc intro
          (Style.as_inline_code type_expr_with_reserved_names) ctx
      | None, Univ _, Some(Errortrace.Incompatible_fields {name; diff}) ->
        explain_incompatible_fields name diff
      | _ -> Format_doc.Doc.empty
    in
    explain_escape pre kind
  | Errortrace.Incompatible_fields { name; diff} ->
    Some(explain_incompatible_fields name diff)
  | Errortrace.Variant v ->
    explain_variant v
  | Errortrace.Obj o ->
    explain_object o
  | Errortrace.Rec_occur(x,y) ->
    reserve_names x;
    reserve_names y;
    begin match get_desc x with
    | Tvar _ | Tunivar _  ->
        mark_loops x;
        mark_loops y;
        Some(
          doc_printf "@,@[<hov>The type variable %a occurs inside@ %a@]"
            (Style.as_inline_code prepared_type_expr) x
            (Style.as_inline_code prepared_type_expr) y
        )
    | _ ->
        (* We had a delayed unification of the type variable with
           a non-variable after the occur check. *)
        Some Format_doc.Doc.empty
        (* There is no need to search further for an explanation, but
           we don't want to print a message of the form:
             {[ The type int occurs inside int list -> 'a |}
        *)
    end
  | Errortrace.Bad_jkind (t,e) ->
      Some (doc_printf "@ @[<hov>%a@]"
              (Jkind.Violation.report_with_offender
                 ~offender:(fun ppf -> type_expr ppf t)
                 ~level:(get_current_level ())) e)
  | Errortrace.Bad_jkind_sort (t,e) ->
      Some (doc_printf "@ @[<hov>%a@]"
              (Jkind.Violation.report_with_offender_sort
                 ~offender:(fun ppf -> type_expr ppf t)
                 ~level:(get_current_level ())) e)
  | Errortrace.Unequal_var_jkinds (t1,k1,t2,k2) ->
      let fmt_history t k ppf =
        Jkind.(format_history ~intro:(
          dprintf "The layout of %a is %a" prepared_type_expr t format k) ppf k)
      in
      Some (doc_printf "@ because the layouts of their variables are different.\
                     @ @[<v>%t@;%t@]"
              (fmt_history t1 k1) (fmt_history t2 k2))
  | Errortrace.Unequal_tof_kind_jkinds (k1, k2) ->
      let fmt_history which k ppf =
        Jkind.(format_history ~intro:(
          dprintf "The kind of %s is %a" which format k) ppf k)
      in
      Some (doc_printf "@ because their kinds are different.\
                     @ @[<v>%t@;%t@]"
              (fmt_history "the first" k1) (fmt_history "the second" k2))

let mismatch intro env trace =
  Errortrace.explain trace (fun ~prev h -> explanation intro prev env h)

let warn_on_missing_def env ppf t =
  match get_desc t with
  | Tconstr (p,_,_) ->
    begin match Env.find_type p env with
    | exception Not_found ->
        fprintf ppf
          "@,@[<hov>Type %a is abstract because@ no corresponding\
           @ cmi file@ was found@ in path.@]" (Style.as_inline_code path) p
    | { type_manifest = Some _; _ } -> ()
    | { type_manifest = None; _ } as decl ->
        match type_origin decl with
        | Rec_check_regularity ->
            fprintf ppf
              "@,@[<hov>Type %a was considered abstract@ when checking\
               @ constraints@ in this@ recursive type definition.@]"
              (Style.as_inline_code path) p
        | Definition | Existential _ -> ()
      end
  | _ -> ()

let prepare_expansion_head empty_tr = function
  | Errortrace.Diff d ->
      Some (Errortrace.map_diff (may_prepare_expansion empty_tr) d)
  | _ -> None

let head_error_printer ~var_jkinds mode txt_got txt_but = function
  | None -> Format_doc.Doc.empty
  | Some d ->
      let d =
        Errortrace.map_diff (trees_of_type_expansion' ~var_jkinds mode) d
      in
      doc_printf "%a@;<1 2>%a@ %a@;<1 2>%a"
        pp_doc txt_got type_expansion d.Errortrace.got
        pp_doc txt_but type_expansion d.Errortrace.expected

let warn_on_missing_defs env ppf = function
  | None -> ()
  | Some Errortrace.{got      = {ty=te1; expanded=_};
                     expected = {ty=te2; expanded=_} } ->
      warn_on_missing_def env ppf te1;
      warn_on_missing_def env ppf te2

(* [subst] comes out of equality, and is [[]] otherwise *)
let error trace_format mode subst env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  (* We want to substitute in the opposite order from [Eqtype] *)
  Names.add_subst (List.map (fun (ty1,ty2) -> ty2,ty1) subst);
  let tr =
    prepare_trace
      (fun ty_exp ->
         Errortrace.{ty_exp with expanded = hide_variant_name ty_exp.expanded})
      tr
  in
  let jkind_error = match Misc.last tr with
    | Some (Bad_jkind _ | Bad_jkind_sort _ | Unequal_var_jkinds _
           | Unequal_tof_kind_jkinds _) ->
        true
    | Some (Diff _ | Escape _ | Variant _ | Obj _ | Incompatible_fields _
           | Rec_occur _)
    | None ->
        false
  in
  match tr with
  | [] -> assert false
  | (elt :: tr) as full_trace ->
    try
      print_labels := not !Clflags.classic;
      let tr, last = filter_trace tr in
      let head = prepare_expansion_head (tr=[] && last=None) elt in
      let tr = List.map (Errortrace.map_diff prepare_expansion) tr in
      let last = Option.map (Errortrace.map_diff prepare_expansion) last in
      let head_error =
        head_error_printer ~var_jkinds:jkind_error mode txt1 txt2 head
      in
      let tr = trees_of_trace mode tr in
      let last =
        Option.map (Errortrace.map_diff (trees_of_type_expansion mode)) last in
      let mis = mismatch txt1 env full_trace in
      let tr = match mis, last with
        | None, Some elt -> tr @ [elt]
        | Some _, _ | _, None -> tr
       in
       fprintf ppf
        "@[<v>\
          @[%a%a@]%a%a\
         @]"
        pp_doc head_error
        pp_doc ty_expect_explanation
        (trace false (incompatibility_phrase trace_format)) tr
        (pp_print_option pp_doc) mis;
      if env <> Env.empty && not jkind_error
       (* the jkinds mechanism has its own way of reporting missing cmis *)
      then warn_on_missing_defs env ppf head;
      Internal_names.print_explanations env ppf;
      Conflicts.print_explanations ppf;
      print_labels := true
    with exn ->
      print_labels := true;
      raise exn

let report_error trace_format ppf mode env tr
      ?(subst = [])
      ?(type_expected_explanation = Fmt.Doc.empty)
      txt1 txt2 =
  wrap_printing_env ~error:true env (fun () ->
    error trace_format mode subst env tr txt1 ppf txt2
      type_expected_explanation)

let report_unification_error ?type_expected_explanation
      ppf env ({trace} : Errortrace.unification_error) =
  report_error ?type_expected_explanation Unification ppf Type env
    ?subst:None trace

let report_equality_error
      ppf mode env ({subst; trace} : Errortrace.equality_error) =
  report_error Equality ppf mode env
    ~subst ?type_expected_explanation:None trace

let report_moregen_error
      ppf mode env ({trace} : Errortrace.moregen_error) =
  report_error Moregen ppf mode env
    ?subst:None ?type_expected_explanation:None trace

let report_comparison_error ppf mode env = function
  | Errortrace.Equality_error error -> report_equality_error ppf mode env error
  | Errortrace.Moregen_error  error -> report_moregen_error  ppf mode env error

module Subtype = struct
  (* There's a frustrating amount of code duplication between this module and
     the outside code, particularly in [prepare_trace] and [filter_trace].
     Unfortunately, [Subtype] is *just* similar enough to have code duplication,
     while being *just* different enough (it's only [Diff]) for the abstraction
     to be nonobvious.  Someday, perhaps... *)

  let printing_status = function
    | Errortrace.Subtype.Diff d -> diff_printing_status d

  let prepare_unification_trace = prepare_trace

  let prepare_trace f tr =
    prepare_any_trace printing_status (Errortrace.Subtype.map f tr)

  let trace filter_trace get_diff fst keep_last txt ppf tr =
    print_labels := not !Clflags.classic;
    try match tr with
      | elt :: tr' ->
        let diffed_elt = get_diff elt in
        let tr, last = filter_trace tr' in
        let tr = match keep_last, last with
          | true, Some last -> tr @ [last]
          | _ -> tr
        in
        let tr =
          trees_of_trace Type
          @@ List.map (Errortrace.map_diff prepare_expansion) tr in
        let tr =
          match fst, diffed_elt with
          | true, Some elt -> elt :: tr
          | _, _ -> tr
        in
        trace fst txt ppf tr;
        print_labels := true
      | _ -> ()
    with exn ->
      print_labels := true;
      raise exn

  let rec filter_subtype_trace = function
    | [] -> [], None
    | [Errortrace.Subtype.Diff d as elt]
      when printing_status elt = Optional_refinement ->
        [], Some d
    | Errortrace.Subtype.Diff d :: rem ->
        let ftr, last = filter_subtype_trace rem in
        d :: ftr, last

  let unification_get_diff = function
    | Errortrace.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)
    | _ -> None

  let subtype_get_diff = function
    | Errortrace.Subtype.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)

  let report_error
        ppf
        env
        (Errortrace.Subtype.{trace = tr_sub; unification_trace = tr_unif})
        txt1 =
    wrap_printing_env ~error:true env (fun () ->
      reset ();
      let tr_sub = prepare_trace prepare_expansion tr_sub in
      let tr_unif = prepare_unification_trace prepare_expansion tr_unif in
      let keep_first = match tr_unif with
        | [Obj _ | Variant _ | Escape _ ] | [] -> true
        | _ -> false in
      fprintf ppf "@[<v>%a"
        (trace filter_subtype_trace subtype_get_diff true keep_first txt1)
        tr_sub;
      if tr_unif = [] then fprintf ppf "@]" else
        let mis = mismatch (doc_printf "Within this type") env tr_unif in
        fprintf ppf "%a%a%t@]"
          (trace filter_trace unification_get_diff false
             (mis = None) "is not compatible with type") tr_unif
          (pp_print_option pp_doc) mis
          Conflicts.print_explanations
    )
end

let report_ambiguous_type_error ppf env tp0 tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tp0 = trees_of_type_path_expansion tp0 in
      match tpl with
      [] -> assert false
    | [tp] ->
        fprintf ppf
          "@[%a@;<1 2>%a@ \
             %a@;<1 2>%a\
           @]"
          pp_doc txt1 type_path_expansion (trees_of_type_path_expansion tp)
          pp_doc txt3 type_path_expansion tp0
    | _ ->
        fprintf ppf
          "@[%a@;<1 2>@[<hv>%a@]\
             @ %a@;<1 2>%a\
           @]"
          pp_doc txt2 type_path_list (List.map trees_of_type_path_expansion tpl)
          pp_doc txt3 type_path_expansion tp0)
||||||| upstream-base
(* Trace-specific printing *)

(* A configuration type that controls which trace we print.  This could be
   exposed, but we instead expose three separate
   [report_{unification,equality,moregen}_error] functions.  This also lets us
   give the unification case an extra optional argument without adding it to the
   equality and moregen cases. *)
type 'variety trace_format =
  | Unification : Errortrace.unification trace_format
  | Equality    : Errortrace.comparison  trace_format
  | Moregen     : Errortrace.comparison  trace_format

let incompatibility_phrase (type variety) : variety trace_format -> string =
  function
  | Unification -> "is not compatible with type"
  | Equality    -> "is not equal to type"
  | Moregen     -> "is not compatible with type"

(* Print a unification error *)

let trees_of_trace mode =
  List.map (Errortrace.map_diff (trees_of_type_expansion mode))

let trees_of_type_path_expansion (tp,tp') =
  if Path.same tp tp' then Same(tree_of_path (Some Type) tp) else
    Diff(tree_of_path (Some Type) tp, tree_of_path (Some Type) tp')

let type_path_expansion ppf = function
  | Same p -> Style.as_inline_code !Oprint.out_ident ppf p
  | Diff(p,p') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
        (Style.as_inline_code !Oprint.out_ident) p
        (Style.as_inline_code !Oprint.out_ident) p'

let rec trace fst txt ppf = function
  | {Errortrace.got; expected} :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@]%a"
       type_expansion got txt type_expansion expected
       (trace false txt) rem
  | _ -> ()

type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

let diff_printing_status Errortrace.{ got      = {ty = t1; expanded = t1'};
                                      expected = {ty = t2; expanded = t2'} } =
  if  is_constr_row ~allow_ident:true t1'
   || is_constr_row ~allow_ident:true t2'
  then Discard
  else if same_path t1 t1' && same_path t2 t2' then Optional_refinement
  else Keep

let printing_status = function
  | Errortrace.Diff d -> diff_printing_status d
  | Errortrace.Escape {kind = Constraint} -> Keep
  | _ -> Keep

(** Flatten the trace and remove elements that are always discarded
    during printing *)

(* Takes [printing_status] to change behavior for [Subtype] *)
let prepare_any_trace printing_status tr =
  let clean_trace x l = match printing_status x with
    | Keep -> x :: l
    | Optional_refinement when l = [] -> [x]
    | Optional_refinement | Discard -> l
  in
  match tr with
  | [] -> []
  | elt :: rem -> elt :: List.fold_right clean_trace rem []

let prepare_trace f tr =
  prepare_any_trace printing_status (Errortrace.map f tr)

(** Keep elements that are [Diff _ ] and take the decision
    for the last element, require a prepared trace *)
let rec filter_trace keep_last = function
  | [] -> []
  | [Errortrace.Diff d as elt]
    when printing_status elt = Optional_refinement ->
    if keep_last then [d] else []
  | Errortrace.Diff d :: rem -> d :: filter_trace keep_last rem
  | _ :: rem -> filter_trace keep_last rem

let type_path_list =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.pp_print_break ppf 2 0)
    type_path_expansion

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  match get_desc t with
  | Tvariant row ->
      let Row {fields; more; name; fixed; closed} = row_repr row in
      if name = None then t else
      newty2 ~level:(get_level t)
        (Tvariant
           (create_row ~fields ~fixed ~closed ~name:None
              ~more:(newvar2 (get_level more))))
  | _ -> t

let prepare_expansion Errortrace.{ty; expanded} =
  let expanded = hide_variant_name expanded in
  reserve_names ty;
  if not (same_path ty expanded) then reserve_names expanded;
  Errortrace.{ty; expanded}

let may_prepare_expansion compact (Errortrace.{ty; expanded} as ty_exp) =
  match get_desc expanded with
    Tvariant _ | Tobject _ when compact ->
      reserve_names ty; Errortrace.{ty; expanded = ty}
  | _ -> prepare_expansion ty_exp

let print_path p =
  Format.dprintf "%a" !Oprint.out_ident (tree_of_path (Some Type) p)

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let print_tags =
  let comma ppf () = Format.fprintf ppf ",@ " in
  Format.pp_print_list ~pp_sep:comma print_tag

let is_unit env ty =
  match get_desc (Ctype.expand_head env ty) with
  | Tconstr (p, _, _) -> Path.same p Predef.path_unit
  | _ -> false

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explanation_diff env t3 t4 : (Format.formatter -> unit) option =
  match get_desc t3, get_desc t4 with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit env ty1 && unifiable env ty2 t4 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[@{<hint>Hint@}: Did you forget to provide %a as argument?@]"
          Style.inline_code "()"
        )
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit env ty1 && unifiable env t3 ty2 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[@{<hint>Hint@}: Did you forget to wrap the expression using \
           %a?@]"
          Style.inline_code "fun () ->"
        )
  | _ ->
      None

let explain_fixed_row_case ppf = function
  | Errortrace.Cannot_be_closed ->
      fprintf ppf "it cannot be closed"
  | Errortrace.Cannot_add_tags tags ->
      fprintf ppf "it may not allow the tag(s) %a"
        print_tags tags

let explain_fixed_row pos expl = match expl with
  | Fixed_private ->
    dprintf "The %a variant type is private" Errortrace.print_pos pos
  | Univar x ->
    reserve_names x;
    dprintf "The %a variant type is bound to the universal type variable %a"
      Errortrace.print_pos pos
      (Style.as_inline_code type_expr_with_reserved_names) x
  | Reified p ->
    dprintf "The %a variant type is bound to %a"
      Errortrace.print_pos pos
      (Style.as_inline_code
         (fun ppf p ->
           Internal_names.add p;
           print_path p ppf))
      p
  | Rigid -> ignore

let explain_variant (type variety) : variety Errortrace.variant -> _ = function
  (* Common *)
  | Errortrace.Incompatible_types_for s ->
      Some(dprintf "@,Types for tag %a are incompatible"
             print_tag s
          )
  (* Unification *)
  | Errortrace.No_intersection ->
      Some(dprintf "@,These two variant types have no intersection")
  | Errortrace.No_tags(pos,fields) -> Some(
      dprintf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        Errortrace.print_pos pos
        print_tags (List.map fst fields)
    )
  | Errortrace.Fixed_row (pos,
                          k,
                          (Univar _ | Reified _ | Fixed_private as e)) ->
      Some (
        dprintf "@,@[%t,@ %a@]" (explain_fixed_row pos e)
          explain_fixed_row_case k
      )
  | Errortrace.Fixed_row (_,_, Rigid) ->
      (* this case never happens *)
      None
  (* Equality & Moregen *)
  | Errortrace.Presence_not_guaranteed_for (pos, s) -> Some(
      dprintf
        "@,@[The tag %a is guaranteed to be present in the %a variant type,\
         @ but not in the %a@]"
        print_tag s
        Errortrace.print_pos (Errortrace.swap_position pos)
        Errortrace.print_pos pos
    )
  | Errortrace.Openness pos ->
      Some(dprintf "@,The %a variant type is open and the %a is not"
             Errortrace.print_pos pos
             Errortrace.print_pos (Errortrace.swap_position pos))

let explain_escape pre = function
  | Errortrace.Univ u ->
      reserve_names u;
      Some(
        dprintf "%t@,The universal variable %a would escape its scope"
          pre
          (Style.as_inline_code type_expr_with_reserved_names) u
      )
  | Errortrace.Constructor p -> Some(
      dprintf
        "%t@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        pre (Style.as_inline_code path) p
    )
  | Errortrace.Module_type p -> Some(
      dprintf
        "%t@,@[The module type@;<1 2>%a@ would escape its scope@]"
        pre (Style.as_inline_code path) p
    )
  | Errortrace.Equation Errortrace.{ty = _; expanded = t} ->
      reserve_names t;
      Some(
        dprintf "%t @,@[<hov>This instance of %a is ambiguous:@ %s@]"
          pre
          (Style.as_inline_code type_expr_with_reserved_names) t
          "it would escape the scope of its equation"
      )
  | Errortrace.Self ->
      Some (dprintf "%t@,Self type cannot escape its class" pre)
  | Errortrace.Constraint ->
      None

let explain_object (type variety) : variety Errortrace.obj -> _ = function
  | Errortrace.Missing_field (pos,f) -> Some(
      dprintf "@,@[The %a object type has no method %a@]"
        Errortrace.print_pos pos Style.inline_code f
    )
  | Errortrace.Abstract_row pos -> Some(
      dprintf
        "@,@[The %a object type has an abstract row, it cannot be closed@]"
        Errortrace.print_pos pos
    )
  | Errortrace.Self_cannot_be_closed ->
      Some (dprintf "@,Self type cannot be unified with a closed object type")

let explain_incompatible_fields name (diff: Types.type_expr Errortrace.diff) =
  reserve_names diff.got;
  reserve_names diff.expected;
  dprintf "@,@[The method %a has type@ %a,@ \
  but the expected method type was@ %a@]"
    Style.inline_code name
    (Style.as_inline_code type_expr_with_reserved_names) diff.got
    (Style.as_inline_code type_expr_with_reserved_names) diff.expected

let explanation (type variety) intro prev env
  : (Errortrace.expanded_type, variety) Errortrace.elt -> _ = function
  | Errortrace.Diff {got; expected} ->
    explanation_diff env got.expanded expected.expanded
  | Errortrace.Escape {kind; context} ->
    let pre =
      match context, kind, prev with
      | Some ctx, _, _ ->
        reserve_names ctx;
        dprintf "@[%t@;<1 2>%a@]" intro
          (Style.as_inline_code type_expr_with_reserved_names) ctx
      | None, Univ _, Some(Errortrace.Incompatible_fields {name; diff}) ->
        explain_incompatible_fields name diff
      | _ -> ignore
    in
    explain_escape pre kind
  | Errortrace.Incompatible_fields { name; diff} ->
    Some(explain_incompatible_fields name diff)
  | Errortrace.Variant v ->
    explain_variant v
  | Errortrace.Obj o ->
    explain_object o
  | Errortrace.Rec_occur(x,y) ->
    reserve_names x;
    reserve_names y;
    begin match get_desc x with
    | Tvar _ | Tunivar _  ->
        Some(fun ppf ->
          reset_loop_marks ();
          mark_loops x;
          mark_loops y;
          dprintf "@,@[<hov>The type variable %a occurs inside@ %a@]"
            (Style.as_inline_code prepared_type_expr) x
            (Style.as_inline_code prepared_type_expr) y
            ppf)
    | _ ->
        (* We had a delayed unification of the type variable with
           a non-variable after the occur check. *)
        Some ignore
        (* There is no need to search further for an explanation, but
           we don't want to print a message of the form:
             {[ The type int occurs inside int list -> 'a |}
        *)
    end

let mismatch intro env trace =
  Errortrace.explain trace (fun ~prev h -> explanation intro prev env h)

let explain mis ppf =
  match mis with
  | None -> ()
  | Some explain -> explain ppf

let warn_on_missing_def env ppf t =
  match get_desc t with
  | Tconstr (p,_,_) ->
    begin match Env.find_type p env with
    | exception Not_found ->
        fprintf ppf
          "@,@[<hov>Type %a is abstract because@ no corresponding\
           @ cmi file@ was found@ in path.@]" (Style.as_inline_code path) p
    | { type_manifest = Some _; _ } -> ()
    | { type_manifest = None; _ } as decl ->
        match type_origin decl with
        | Rec_check_regularity ->
            fprintf ppf
              "@,@[<hov>Type %a was considered abstract@ when checking\
               @ constraints@ in this@ recursive type definition.@]"
              (Style.as_inline_code path) p
        | Definition | Existential _ -> ()
      end
  | _ -> ()

let prepare_expansion_head empty_tr = function
  | Errortrace.Diff d ->
      Some (Errortrace.map_diff (may_prepare_expansion empty_tr) d)
  | _ -> None

let head_error_printer mode txt_got txt_but = function
  | None -> ignore
  | Some d ->
      let d = Errortrace.map_diff (trees_of_type_expansion mode) d in
      dprintf "%t@;<1 2>%a@ %t@;<1 2>%a"
        txt_got type_expansion d.Errortrace.got
        txt_but type_expansion d.Errortrace.expected

let warn_on_missing_defs env ppf = function
  | None -> ()
  | Some Errortrace.{got      = {ty=te1; expanded=_};
                     expected = {ty=te2; expanded=_} } ->
      warn_on_missing_def env ppf te1;
      warn_on_missing_def env ppf te2

(* [subst] comes out of equality, and is [[]] otherwise *)
let error trace_format mode subst env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  (* We want to substitute in the opposite order from [Eqtype] *)
  Names.add_subst (List.map (fun (ty1,ty2) -> ty2,ty1) subst);
  let tr =
    prepare_trace
      (fun ty_exp ->
         Errortrace.{ty_exp with expanded = hide_variant_name ty_exp.expanded})
      tr
  in
  let mis = mismatch txt1 env tr in
  match tr with
  | [] -> assert false
  | elt :: tr ->
    try
      print_labels := not !Clflags.classic;
      let tr = filter_trace (mis = None) tr in
      let head = prepare_expansion_head (tr=[]) elt in
      let tr = List.map (Errortrace.map_diff prepare_expansion) tr in
      let head_error = head_error_printer mode txt1 txt2 head in
      let tr = trees_of_trace mode tr in
      fprintf ppf
        "@[<v>\
          @[%t%t@]%a%t\
         @]"
        head_error
        ty_expect_explanation
        (trace false (incompatibility_phrase trace_format)) tr
        (explain mis);
      if env <> Env.empty
      then warn_on_missing_defs env ppf head;
      Internal_names.print_explanations env ppf;
      Conflicts.print_explanations ppf;
      print_labels := true
    with exn ->
      print_labels := true;
      raise exn

let report_error trace_format ppf mode env tr
      ?(subst = [])
      ?(type_expected_explanation = fun _ -> ())
      txt1 txt2 =
  wrap_printing_env ~error:true env (fun () ->
    error trace_format mode subst env tr txt1 ppf txt2
      type_expected_explanation)

let report_unification_error
      ppf env ({trace} : Errortrace.unification_error) =
  report_error Unification ppf Type env
    ?subst:None trace

let report_equality_error
      ppf mode env ({subst; trace} : Errortrace.equality_error) =
  report_error Equality ppf mode env
    ~subst ?type_expected_explanation:None trace

let report_moregen_error
      ppf mode env ({trace} : Errortrace.moregen_error) =
  report_error Moregen ppf mode env
    ?subst:None ?type_expected_explanation:None trace

let report_comparison_error ppf mode env = function
  | Errortrace.Equality_error error -> report_equality_error ppf mode env error
  | Errortrace.Moregen_error  error -> report_moregen_error  ppf mode env error

module Subtype = struct
  (* There's a frustrating amount of code duplication between this module and
     the outside code, particularly in [prepare_trace] and [filter_trace].
     Unfortunately, [Subtype] is *just* similar enough to have code duplication,
     while being *just* different enough (it's only [Diff]) for the abstraction
     to be nonobvious.  Someday, perhaps... *)

  let printing_status = function
    | Errortrace.Subtype.Diff d -> diff_printing_status d

  let prepare_unification_trace = prepare_trace

  let prepare_trace f tr =
    prepare_any_trace printing_status (Errortrace.Subtype.map f tr)

  let trace filter_trace get_diff fst keep_last txt ppf tr =
    print_labels := not !Clflags.classic;
    try match tr with
      | elt :: tr' ->
        let diffed_elt = get_diff elt in
        let tr =
          trees_of_trace Type
          @@ List.map (Errortrace.map_diff prepare_expansion)
          @@ filter_trace keep_last tr' in
        let tr =
          match fst, diffed_elt with
          | true, Some elt -> elt :: tr
          | _, _ -> tr
        in
        trace fst txt ppf tr;
        print_labels := true
      | _ -> ()
    with exn ->
      print_labels := true;
      raise exn

  let rec filter_subtype_trace keep_last = function
    | [] -> []
    | [Errortrace.Subtype.Diff d as elt]
      when printing_status elt = Optional_refinement ->
        if keep_last then [d] else []
    | Errortrace.Subtype.Diff d :: rem ->
        d :: filter_subtype_trace keep_last rem

  let unification_get_diff = function
    | Errortrace.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)
    | _ -> None

  let subtype_get_diff = function
    | Errortrace.Subtype.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)

  let report_error
        ppf
        env
        (Errortrace.Subtype.{trace = tr_sub; unification_trace = tr_unif})
        txt1 =
    wrap_printing_env ~error:true env (fun () ->
      reset ();
      let tr_sub = prepare_trace prepare_expansion tr_sub in
      let tr_unif = prepare_unification_trace prepare_expansion tr_unif in
      let keep_first = match tr_unif with
        | [Obj _ | Variant _ | Escape _ ] | [] -> true
        | _ -> false in
      fprintf ppf "@[<v>%a"
        (trace filter_subtype_trace subtype_get_diff true keep_first txt1)
        tr_sub;
      if tr_unif = [] then fprintf ppf "@]" else
        let mis = mismatch (dprintf "Within this type") env tr_unif in
        fprintf ppf "%a%t%t@]"
          (trace filter_trace unification_get_diff false
             (mis = None) "is not compatible with type") tr_unif
          (explain mis)
          Conflicts.print_explanations
    )
end

let report_ambiguous_type_error ppf env tp0 tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tp0 = trees_of_type_path_expansion tp0 in
      match tpl with
      [] -> assert false
    | [tp] ->
        fprintf ppf
          "@[%t@;<1 2>%a@ \
             %t@;<1 2>%a\
           @]"
          txt1 type_path_expansion (trees_of_type_path_expansion tp)
          txt3 type_path_expansion tp0
    | _ ->
        fprintf ppf
          "@[%t@;<1 2>@[<hv>%a@]\
             @ %t@;<1 2>%a\
           @]"
          txt2 type_path_list (List.map trees_of_type_path_expansion tpl)
          txt3 type_path_expansion tp0)
=======
(* Trace-specific printing *)

(* A configuration type that controls which trace we print.  This could be
   exposed, but we instead expose three separate
   [{unification,equality,moregen}] functions.  This also lets us
   give the unification case an extra optional argument without adding it to the
   equality and moregen cases. *)
type 'variety trace_format =
  | Unification : Errortrace.unification trace_format
  | Equality    : Errortrace.comparison  trace_format
  | Moregen     : Errortrace.comparison  trace_format

let incompatibility_phrase (type variety) : variety trace_format -> string =
  function
  | Unification -> "is not compatible with type"
  | Equality    -> "is not equal to type"
  | Moregen     -> "is not compatible with type"

(* Print a unification error *)
open Out_type
open Format_doc
module Fmt = Format_doc
module Style = Misc.Style

type 'a diff = 'a Out_type.diff = Same of 'a | Diff of 'a * 'a

let trees_of_trace mode =
  List.map (Errortrace.map_diff (trees_of_type_expansion mode))

let rec trace fst txt ppf = function
  | {Errortrace.got; expected} :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@]%a"
       pp_type_expansion got txt pp_type_expansion expected
       (trace false txt) rem
  | _ -> ()

type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

let diff_printing_status Errortrace.{ got      = {ty = t1; expanded = t1'};
                                      expected = {ty = t2; expanded = t2'} } =
  if  Btype.is_constr_row ~allow_ident:true t1'
   || Btype.is_constr_row ~allow_ident:true t2'
  then Discard
  else if same_path t1 t1' && same_path t2 t2' then Optional_refinement
  else Keep

let printing_status = function
  | Errortrace.Diff d -> diff_printing_status d
  | Errortrace.Escape {kind = Constraint} -> Keep
  | _ -> Keep

(** Flatten the trace and remove elements that are always discarded
    during printing *)

(* Takes [printing_status] to change behavior for [Subtype] *)
let prepare_any_trace printing_status tr =
  let clean_trace x l = match printing_status x with
    | Keep -> x :: l
    | Optional_refinement when l = [] -> [x]
    | Optional_refinement | Discard -> l
  in
  match tr with
  | [] -> []
  | elt :: rem -> elt :: List.fold_right clean_trace rem []

let prepare_trace f tr =
  prepare_any_trace printing_status (Errortrace.map f tr)

(** Keep elements that are [Diff _ ] and split the the last element if it is
    optionally elidable, require a prepared trace *)
let rec filter_trace = function
  | [] -> [], None
  | [Errortrace.Diff d as elt]
    when printing_status elt = Optional_refinement -> [], Some d
  | Errortrace.Diff d :: rem ->
      let filtered, last = filter_trace rem in
      d :: filtered, last
  | _ :: rem -> filter_trace rem

let may_prepare_expansion compact (Errortrace.{ty; expanded} as ty_exp) =
  match Types.get_desc expanded with
    Tvariant _ | Tobject _ when compact ->
      Variable_names.reserve ty; Errortrace.{ty; expanded = ty}
  | _ -> prepare_expansion ty_exp

let print_path p =
  Fmt.dprintf "%a" !Oprint.out_ident (namespaced_tree_of_path Type p)

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let print_tags ppf tags  =
  Fmt.(pp_print_list ~pp_sep:comma) print_tag ppf tags

let is_unit env ty =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Tconstr (p, _, _) -> Path.same p Predef.path_unit
  | _ -> false

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Ctype.Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explanation_diff env t3 t4 =
  match Types.get_desc t3, Types.get_desc t4 with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit env ty1 && unifiable env ty2 t4 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to provide %a as argument?@]"
          Style.inline_code "()"
        )
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit env ty1 && unifiable env t3 ty2 ->
      Some (doc_printf
          "@,@[@{<hint>Hint@}: Did you forget to wrap the expression using \
           %a?@]"
          Style.inline_code "fun () ->"
        )
  | _ ->
      None

let explain_fixed_row_case = function
  | Errortrace.Cannot_be_closed -> doc_printf "it cannot be closed"
  | Errortrace.Cannot_add_tags tags ->
      doc_printf "it may not allow the tag(s) %a"
        print_tags tags

let pp_path ppf p =
  Style.as_inline_code Printtyp.Doc.path ppf p

let explain_fixed_row pos expl = match expl with
  | Types.Fixed_private ->
    doc_printf "The %a variant type is private" Errortrace.print_pos pos
  | Types.Univar x ->
    Variable_names.reserve x;
    doc_printf "The %a variant type is bound to the universal type variable %a"
      Errortrace.print_pos pos
      (Style.as_inline_code type_expr_with_reserved_names) x
  | Types.Reified p ->
    doc_printf "The %a variant type is bound to %a"
      Errortrace.print_pos pos
      (Style.as_inline_code
         (fun ppf p ->
           Internal_names.add p;
           print_path p ppf))
      p
  | Types.Rigid -> Format_doc.Doc.empty

let explain_variant (type variety) : variety Errortrace.variant -> _ = function
  (* Common *)
  | Errortrace.Incompatible_types_for s ->
      Some(doc_printf "@,Types for tag %a are incompatible"
             print_tag s
          )
  (* Unification *)
  | Errortrace.No_intersection ->
      Some(doc_printf "@,These two variant types have no intersection")
  | Errortrace.No_tags(pos,fields) -> Some(
      doc_printf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        Errortrace.print_pos pos
        print_tags (List.map fst fields)
    )
  | Errortrace.Fixed_row (pos,
                          k,
                          (Univar _ | Reified _ | Fixed_private as e)) ->
      Some (
        doc_printf "@,@[%a,@ %a@]" pp_doc (explain_fixed_row pos e)
          pp_doc (explain_fixed_row_case k)
      )
  | Errortrace.Fixed_row (_,_, Rigid) ->
      (* this case never happens *)
      None
  (* Equality & Moregen *)
  | Errortrace.Presence_not_guaranteed_for (pos, s) -> Some(
      doc_printf
        "@,@[The tag %a is guaranteed to be present in the %a variant type,\
         @ but not in the %a@]"
        print_tag s
        Errortrace.print_pos (Errortrace.swap_position pos)
        Errortrace.print_pos pos
    )
  | Errortrace.Openness pos ->
      Some(doc_printf "@,The %a variant type is open and the %a is not"
             Errortrace.print_pos pos
             Errortrace.print_pos (Errortrace.swap_position pos))

let explain_escape pre = function
  | Errortrace.Univ u ->
      Variable_names.reserve u;
      Some(
        doc_printf "%a@,The universal variable %a would escape its scope"
          pp_doc pre
          (Style.as_inline_code type_expr_with_reserved_names) u
      )
  | Errortrace.Constructor p -> Some(
      doc_printf
        "%a@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        pp_doc pre pp_path p
    )
  | Errortrace.Module_type p -> Some(
      doc_printf
        "%a@,@[The module type@;<1 2>%a@ would escape its scope@]"
        pp_doc pre pp_path p
    )
  | Errortrace.Equation Errortrace.{ty = _; expanded = t} ->
      Variable_names.reserve t;
      Some(
        doc_printf "%a@ @[<hov>This instance of %a is ambiguous:@ %s@]"
          pp_doc pre
          (Style.as_inline_code type_expr_with_reserved_names) t
          "it would escape the scope of its equation"
      )
  | Errortrace.Self ->
      Some (doc_printf "%a@,Self type cannot escape its class" pp_doc pre)
  | Errortrace.Constraint ->
      None

let explain_object (type variety) : variety Errortrace.obj -> _ = function
  | Errortrace.Missing_field (pos,f) -> Some(
      doc_printf "@,@[The %a object type has no method %a@]"
        Errortrace.print_pos pos Style.inline_code f
    )
  | Errortrace.Abstract_row pos -> Some(
      doc_printf
        "@,@[The %a object type has an abstract row, it cannot be closed@]"
        Errortrace.print_pos pos
    )
  | Errortrace.Self_cannot_be_closed ->
      Some (doc_printf
              "@,Self type cannot be unified with a closed object type"
           )

let explain_incompatible_fields name (diff: Types.type_expr Errortrace.diff) =
  Variable_names.reserve diff.got;
  Variable_names.reserve diff.expected;
  doc_printf "@,@[The method %a has type@ %a,@ \
  but the expected method type was@ %a@]"
    Style.inline_code name
    (Style.as_inline_code type_expr_with_reserved_names) diff.got
    (Style.as_inline_code type_expr_with_reserved_names) diff.expected


let explain_label_mismatch ~missing_label_msg  {Errortrace.got;expected} =
  let quoted_label ppf l = Style.inline_code ppf (Asttypes.string_of_label l) in
  match got, expected with
  | Asttypes.Nolabel, Asttypes.(Labelled _ | Optional _ )  ->
      doc_printf "@,@[A label@ %a@ was expected@]"
        quoted_label expected
  | Asttypes.(Labelled _|Optional _), Asttypes.Nolabel  ->
      doc_printf missing_label_msg
        quoted_label got
 | Asttypes.Labelled g, Asttypes.Optional e when g = e ->
      doc_printf
        "@,@[The label@ %a@ was expected to be optional@]"
        quoted_label got
  | Asttypes.Optional g, Asttypes.Labelled e when g = e ->
      doc_printf
        "@,@[The label@ %a@ was expected to not be optional@]"
        quoted_label got
  | Asttypes.(Labelled _ | Optional _), Asttypes.(Labelled _ | Optional _) ->
      doc_printf "@,@[Labels %a@ and@ %a do not match@]"
        quoted_label got
        quoted_label expected
  | Asttypes.Nolabel, Asttypes.Nolabel ->
      (* Two empty labels cannot be mismatched*)
      assert false


let explain_first_class_module = function
  | Errortrace.Package_cannot_scrape p -> Some(
      doc_printf "@,@[The module alias %a could not be expanded@]"
        pp_path p
    )
  | Errortrace.Package_inclusion pr ->
      Some(doc_printf "@,@[%a@]" Fmt.pp_doc pr)
  | Errortrace.Package_coercion pr ->
      Some(doc_printf "@,@[%a@]" Fmt.pp_doc pr)

let explanation (type variety) intro prev env
  : (Errortrace.expanded_type, variety) Errortrace.elt -> _ = function
  | Errortrace.Diff {got; expected} ->
    explanation_diff env got.expanded expected.expanded
  | Errortrace.Escape {kind; context} ->
    let pre =
      match context, kind, prev with
      | Some ctx, _, _ ->
        Variable_names.reserve ctx;
        doc_printf "@[%a@;<1 2>%a@]" pp_doc intro
          (Style.as_inline_code type_expr_with_reserved_names) ctx
      | None, Univ _, Some(Errortrace.Incompatible_fields {name; diff}) ->
        explain_incompatible_fields name diff
      | _ -> Format_doc.Doc.empty
    in
    explain_escape pre kind
  | Errortrace.Incompatible_fields { name; diff} ->
    Some(explain_incompatible_fields name diff)
  | Errortrace.Function_label_mismatch diff ->
    let missing_label_msg =
      format_of_string
        "@,@[The first argument is labeled@ %a,@ \
         but an unlabeled argument was expected@]"
    in
    Some(explain_label_mismatch ~missing_label_msg diff)
  | Errortrace.Tuple_label_mismatch diff ->
    let ast_label = function
      | None -> Asttypes.Nolabel
      | Some x -> Asttypes.Labelled x
    in
    let diff = Errortrace.map_diff ast_label diff in
    let missing_label_msg =
      format_of_string
        "@,@[The first tuple element is labeled@ %a,@ \
         but an unlabeled element was expected@]"
    in
    Some(explain_label_mismatch ~missing_label_msg diff)
  | Errortrace.Variant v ->
    explain_variant v
  | Errortrace.Obj o ->
    explain_object o
  | Errortrace.First_class_module fm ->
    explain_first_class_module fm
  | Errortrace.Rec_occur(x,y) ->
    add_type_to_preparation x;
    add_type_to_preparation y;
    begin match Types.get_desc x with
    | Tvar _ | Tunivar _  ->
        Some(
          doc_printf "@,@[<hov>The type variable %a occurs inside@ %a@]"
            (Style.as_inline_code prepared_type_expr) x
            (Style.as_inline_code prepared_type_expr) y
        )
    | _ ->
        (* We had a delayed unification of the type variable with
           a non-variable after the occur check. *)
        Some Format_doc.Doc.empty
        (* There is no need to search further for an explanation, but
           we don't want to print a message of the form:
             {[ The type int occurs inside int list -> 'a |}
        *)
    end

let mismatch intro env trace =
  Errortrace.explain trace (fun ~prev h -> explanation intro prev env h)

let warn_on_missing_def env ppf t =
  match Types.get_desc t with
  | Tconstr (p,_,_) ->
    begin match Env.find_type p env with
    | exception Not_found ->
        fprintf ppf
          "@,@[<hov>Type %a is abstract because@ no corresponding\
           @ cmi file@ was found@ in path.@]" pp_path p
    | { type_manifest = Some _; _ } -> ()
    | { type_manifest = None; _ } as decl ->
        match Btype.type_origin decl with
        | Rec_check_regularity ->
            fprintf ppf
              "@,@[<hov>Type %a was considered abstract@ when checking\
               @ constraints@ in this@ recursive type definition.@]"
              pp_path p
        | Definition | Existential _ -> ()
      end
  | _ -> ()

let prepare_expansion_head empty_tr = function
  | Errortrace.Diff d ->
      Some (Errortrace.map_diff (may_prepare_expansion empty_tr) d)
  | _ -> None

let head_error_printer mode txt_got txt_but = function
  | None -> Format_doc.Doc.empty
  | Some d ->
      let d = Errortrace.map_diff (trees_of_type_expansion mode) d in
      doc_printf "%a@;<1 2>%a@ %a@;<1 2>%a"
        pp_doc txt_got pp_type_expansion d.Errortrace.got
        pp_doc txt_but pp_type_expansion d.Errortrace.expected

let warn_on_missing_defs env ppf = function
  | None -> ()
  | Some Errortrace.{got      = {ty=te1; expanded=_};
                     expected = {ty=te2; expanded=_} } ->
      warn_on_missing_def env ppf te1;
      warn_on_missing_def env ppf te2

(* [subst] comes out of equality, and is [[]] otherwise *)
let error trace_format mode subst env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  (* We want to substitute in the opposite order from [Eqtype] *)
  Variable_names.add_subst (List.map (fun (ty1,ty2) -> ty2,ty1) subst);
  let tr =
    prepare_trace
      (fun ty_exp ->
         Errortrace.{ty_exp with expanded = hide_variant_name ty_exp.expanded})
      tr
  in
  match tr with
  | [] -> assert false
  | (elt :: tr) as full_trace ->
      with_labels (not !Clflags.classic) (fun () ->
      let tr, last = filter_trace tr in
      let head = prepare_expansion_head (tr=[] && last=None) elt in
      let tr = List.map (Errortrace.map_diff prepare_expansion) tr in
      let last = Option.map (Errortrace.map_diff prepare_expansion) last in
      let head_error = head_error_printer mode txt1 txt2 head in
      let tr = trees_of_trace mode tr in
      let last =
        Option.map (Errortrace.map_diff (trees_of_type_expansion mode)) last in
      let mis = mismatch txt1 env full_trace in
      let tr = match mis, last with
        | None, Some elt -> tr @ [elt]
        | Some _, _ | _, None -> tr
       in
       fprintf ppf
        "@[<v>\
          @[%a%a@]%a%a\
         @]"
        pp_doc head_error
        pp_doc ty_expect_explanation
        (trace false (incompatibility_phrase trace_format)) tr
        (pp_print_option pp_doc) mis;
      if env <> Env.empty
      then warn_on_missing_defs env ppf head;
       Internal_names.print_explanations env ppf;
       Ident_conflicts.err_print ppf
    )

let report_error trace_format ppf mode env tr
      ?(subst = [])
      ?(type_expected_explanation = Fmt.Doc.empty)
      txt1 txt2 =
  wrap_printing_env ~error:true env (fun () ->
    error trace_format mode subst env tr txt1 ppf txt2
      type_expected_explanation)

let unification
      ppf env ({trace} : Errortrace.unification_error) =
  report_error Unification ppf Type env
    ?subst:None trace

let equality
      ppf mode env ({subst; trace} : Errortrace.equality_error) =
  report_error Equality ppf mode env
    ~subst ?type_expected_explanation:None trace

let moregen
      ppf mode env ({trace} : Errortrace.moregen_error) =
  report_error Moregen ppf mode env
    ?subst:None ?type_expected_explanation:None trace

let comparison ppf mode env = function
  | Errortrace.Equality_error error -> equality ppf mode env error
  | Errortrace.Moregen_error  error -> moregen  ppf mode env error

module Subtype = struct
  (* There's a frustrating amount of code duplication between this module and
     the outside code, particularly in [prepare_trace] and [filter_trace].
     Unfortunately, [Subtype] is *just* similar enough to have code duplication,
     while being *just* different enough (it's only [Diff]) for the abstraction
     to be nonobvious.  Someday, perhaps... *)

  let printing_status = function
    | Errortrace.Subtype.Diff d -> diff_printing_status d

  let prepare_unification_trace = prepare_trace

  let prepare_trace f tr =
    prepare_any_trace printing_status (Errortrace.Subtype.map f tr)

  let trace filter_trace get_diff fst keep_last txt ppf tr =
    with_labels (not !Clflags.classic) (fun () ->
      match tr with
      | elt :: tr' ->
        let diffed_elt = get_diff elt in
        let tr, last = filter_trace tr' in
        let tr = match keep_last, last with
          | true, Some last -> tr @ [last]
          | _ -> tr
        in
        let tr =
          trees_of_trace Type
          @@ List.map (Errortrace.map_diff prepare_expansion) tr in
        let tr =
          match fst, diffed_elt with
          | true, Some elt -> elt :: tr
          | _, _ -> tr
        in
        trace fst txt ppf tr
      | _ -> ()
    )

  let rec filter_subtype_trace = function
    | [] -> [], None
    | [Errortrace.Subtype.Diff d as elt]
      when printing_status elt = Optional_refinement ->
        [], Some d
    | Errortrace.Subtype.Diff d :: rem ->
        let ftr, last = filter_subtype_trace rem in
        d :: ftr, last

  let unification_get_diff = function
    | Errortrace.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)
    | _ -> None

  let subtype_get_diff = function
    | Errortrace.Subtype.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)

  let error
        ppf
        env
        (Errortrace.Subtype.{trace = tr_sub; unification_trace = tr_unif})
        txt1 =
    wrap_printing_env ~error:true env (fun () ->
      reset ();
      let tr_sub = prepare_trace prepare_expansion tr_sub in
      let tr_unif = prepare_unification_trace prepare_expansion tr_unif in
      let keep_first = match tr_unif with
        | [Obj _ | Variant _ | Escape _ ] | [] -> true
        | _ -> false in
      fprintf ppf "@[<v>%a"
        (trace filter_subtype_trace subtype_get_diff true keep_first txt1)
        tr_sub;
      if tr_unif = [] then fprintf ppf "@]" else
        let mis = mismatch (doc_printf "Within this type") env tr_unif in
        fprintf ppf "%a%a%t@]"
          (trace filter_trace unification_get_diff false
             (mis = None) "is not compatible with type") tr_unif
          (pp_print_option pp_doc) mis
          Ident_conflicts.err_print
    )
end

let subtype = Subtype.error

let quoted_ident ppf t =
  Style.as_inline_code !Oprint.out_ident ppf t

let type_path_expansion ppf = function
  | Same p -> quoted_ident ppf p
  | Diff(p,p') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
       quoted_ident p
       quoted_ident p'

let trees_of_type_path_expansion (tp,tp') =
  let path_tree = namespaced_tree_of_path Type in
  if Path.same tp tp' then Same(path_tree tp) else
    Diff(path_tree tp, path_tree tp)

let type_path_list ppf l =
  Fmt.pp_print_list ~pp_sep:(fun ppf () -> Fmt.pp_print_break ppf 2 0)
    type_path_expansion ppf l

let ambiguous_type ppf env tp0 tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tp0 = trees_of_type_path_expansion tp0 in
      match tpl with
      [] -> assert false
    | [tp] ->
        fprintf ppf
          "@[%a@;<1 2>%a@ \
             %a@;<1 2>%a\
           @]"
          pp_doc txt1 type_path_expansion (trees_of_type_path_expansion tp)
          pp_doc txt3 type_path_expansion tp0
    | _ ->
        fprintf ppf
          "@[%a@;<1 2>@[<hv>%a@]\
             @ %a@;<1 2>%a\
           @]"
          pp_doc txt2 type_path_list (List.map trees_of_type_path_expansion tpl)
          pp_doc txt3 type_path_expansion tp0)
>>>>>>> upstream-incoming
