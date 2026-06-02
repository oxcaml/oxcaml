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

open Out_type
module Fmt = Format_doc

let namespaced_ident namespace  id =
  Out_name.print (ident_name (Some namespace) id)

module Doc = struct
  let wrap_printing_env = wrap_printing_env

  let longident = Pprintast.Doc.longident

  let ident ppf id = Fmt.pp_print_string ppf
      (Out_name.print (ident_name None id))



  let typexp mode ppf ty =
    !Oprint.out_type ppf (tree_of_typexp mode ty)

  let modality ?(id = fun _ppf () -> ()) ax ppf modality =
    if Mode.Modality.Per_axis.is_id ax modality then
      id ppf ()
    else
      Fmt.asprintf "%a" (Mode.Modality.Per_axis.print ax) modality
      |> !Oprint.out_modality ppf

  let type_expansion k ppf e =
    pp_type_expansion ppf (trees_of_type_expansion k e)

  let type_declaration id ppf decl =
    !Oprint.out_sig_item ppf (tree_of_type_declaration id decl Trec_first)

  let type_expr ppf ty =
    (* [type_expr] is used directly by error message printers,
       we mark eventual loops ourself to avoid any misuse and stack overflow *)
    prepare_for_printing [ty];
    prepared_type_expr ppf ty

  let shared_type_scheme ppf ty =
    add_type_to_preparation ty;
    typexp Type_scheme ppf ty

  let type_scheme ppf ty =
    prepare_for_printing [ty];
    prepared_type_scheme ppf ty

  let path ppf p =
    !Oprint.out_ident ppf (tree_of_path p)

  let () = Env.print_path := path
  let () = Env.print_type_expr := type_expr
  let () =
    Env.report_jkind_violation_with_offender :=
      Jkind.Violation.report_with_offender

  let type_path ppf p = !Oprint.out_ident ppf (tree_of_type_path p)

  let value_description id ppf decl =
    !Oprint.out_sig_item ppf (tree_of_value_description id decl)

  let class_type ppf cty =
    reset ();
    prepare_class_type cty;
    !Oprint.out_class_type ppf (tree_of_class_type Type cty)

  let class_declaration id ppf cl =
    !Oprint.out_sig_item ppf (tree_of_class_declaration id cl Trec_first)

  let cltype_declaration id ppf cl =
    !Oprint.out_sig_item ppf (tree_of_cltype_declaration id cl Trec_first)

  let modtype ppf mty = !Oprint.out_module_type ppf (tree_of_modtype mty)
  let modtype_declaration id ppf decl =
    !Oprint.out_sig_item ppf (tree_of_modtype_declaration id decl)

  let constructor ppf c =
    reset_except_conflicts ();
    add_constructor_to_preparation c;
    prepared_constructor ppf c

  let constructor_arguments ppf a =
    !Oprint.out_constr_args ppf (tree_of_constructor_arguments a)

  let label ppf l =
    prepare_for_printing [l.Types.ld_type];
    !Oprint.out_label ppf (tree_of_label l)

  let extension_constructor id ppf ext =
    !Oprint.out_sig_item ppf (tree_of_extension_constructor id ext Text_first)

  let extension_only_constructor id ppf (ext:Types.extension_constructor) =
    reset_except_conflicts ();
    prepare_type_constructor_arguments ext.ext_args;
    Option.iter add_type_to_preparation ext.ext_ret_type;
    let name = Ident.name id in
    let args, ret =
      extension_constructor_args_and_ret_type_subtree
        ext.ext_args
        ext.ext_ret_type
    in
    Fmt.fprintf ppf "@[<hv>%a@]"
      !Oprint.out_constr {
      Outcometree.ocstr_name = name;
      ocstr_args = args;
      ocstr_return_type = ret;
    }

  (* Print a signature body (used by -i when compiling a .ml) *)

  let print_signature ppf tree =
    Fmt.fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

  let signature ppf sg =
    Fmt.fprintf ppf "%a" print_signature (tree_of_signature sg)

end
open Doc
let string_of_path p = Fmt.asprintf "%a" path p

let strings_of_paths namespace p =
  let trees = List.map (namespaced_tree_of_path namespace) p in
  List.map (Fmt.asprintf "%a" !Oprint.out_ident) trees

let wrap_printing_env = wrap_printing_env
let ident = Fmt.compat ident
let longident = Fmt.compat longident
let path = Fmt.compat path
let type_path = Fmt.compat type_path
let type_expr = Fmt.compat type_expr

let modality ?id ax =
  Fmt.compat (modality ?id:(Option.map Fmt.deprecated id) ax)

let type_scheme = Fmt.compat type_scheme
let shared_type_scheme = Fmt.compat shared_type_scheme

let type_declaration  = Fmt.compat1 type_declaration
let type_expansion = Fmt.compat1 type_expansion
let value_description = Fmt.compat1 value_description
let label = Fmt.compat label
let constructor = Fmt.compat constructor
let constructor_arguments = Fmt.compat constructor_arguments
let extension_constructor = Fmt.compat1 extension_constructor
let extension_only_constructor = Fmt.compat1 extension_only_constructor

let modtype = Fmt.compat modtype
let modtype_declaration = Fmt.compat1 modtype_declaration
let signature = Fmt.compat signature

let class_declaration = Fmt.compat1 class_declaration
let class_type = Fmt.compat class_type
let cltype_declaration = Fmt.compat1 cltype_declaration

(* Print a signature body (used by -i when compiling a .ml) *)
let printed_signature sourcefile ppf sg =
  (* we are tracking any collision event for warning 63 *)
  Ident_conflicts.reset ();
  let t = tree_of_signature sg in
  if Warnings.(is_active @@ Erroneous_printed_signature "") then
    begin match Ident_conflicts.err_msg () with
    | None -> ()
    | Some msg ->
        let conflicts = Fmt.asprintf "%a" Fmt.pp_doc msg in
        Location.prerr_warning (Location.in_file sourcefile)
          (Warnings.Erroneous_printed_signature conflicts);
        Warnings.check_fatal ()
    end;
  Fmt.compat print_signature ppf t

let string_of_label : Types.arg_label -> string = function
  | Nolabel -> ""
  | Labelled s | Position s -> s
  | Optional s -> "?" ^ s

<<<<<<< HEAD
let () = Jkind.set_printtyp_path Doc.path
let () = Mode.print_longident := Doc.longident
let () =
  Jkind.set_outcometrees_of_types (fun tys ->
    prepare_for_printing tys;
    List.map (tree_of_typexp Type) tys);
  Jkind.set_outcometree_of_modalities tree_of_modalities;
  Jkind.set_print_type_expr Doc.type_expr
||||||| eb63e0e418
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

let same_path t t' =
  eq_type t t' ||
  match get_desc t, get_desc t' with
    Tconstr(p,tl,_), Tconstr(p',tl',_) ->
      let (p1, s1) = best_type_path p and (p2, s2)  = best_type_path p' in
      begin match s1, s2 with
        Nth n1, Nth n2 when n1 = n2 -> true
      | (Id | Map _), (Id | Map _) when Path.same p1 p2 ->
          let tl = apply_subst s1 tl and tl' = apply_subst s2 tl' in
          List.length tl = List.length tl' &&
          List.for_all2 eq_type tl tl'
      | _ -> false
      end
  | _ ->
      false

type 'a diff = Same of 'a | Diff of 'a * 'a

let trees_of_type_expansion'
      ~var_jkinds mode Errortrace.{ty = t; expanded = t'} =
  let tree_of_typexp' ty =
    let out = tree_of_typexp mode ty in
    if var_jkinds then
      match get_desc ty with
      | Tvar { jkind; _ } | Tunivar { jkind; _ } ->
          let okind = out_jkind_of_desc !printing_env (Jkind.get jkind) in
          Otyp_jkind_annot (out, okind)
      | _ ->
          out
    else
      out
  in
  reset_loop_marks ();
  mark_loops t;
  if same_path t t'
  then begin add_delayed (proxy t); Same (tree_of_typexp' t) end
  else begin
    mark_loops t';
    let t' = if proxy t == proxy t' then unalias t' else t' in
    (* beware order matter due to side effect,
       e.g. when printing object types *)
    print_reduced_evals := false; (* preserve unreduced eval in types *)
    let first = tree_of_typexp' t in
    print_reduced_evals := true;
    let second = tree_of_typexp' t' in
    if first = second then Same first
    else Diff(first,second)
  end

let trees_of_type_expansion =
  trees_of_type_expansion' ~var_jkinds:false

let pp_type ppf t =
  Style.as_inline_code !Oprint.out_type ppf t

let quoted_ident ppf t =
  Style.as_inline_code !Oprint.out_ident ppf t

let type_expansion ppf = function
  | Same t -> pp_type ppf t
  | Diff(t,t') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
        pp_type t
        pp_type t'

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
      newty2 ~level:(get_level t)
        (Tvariant
           (create_row ~fields ~fixed ~closed ~name:None
              ~more:(newvar2 (get_level more)
                       (Jkind.Builtin.value ~why:Row_variable))))
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
                 env) e)
  | Errortrace.Bad_jkind_sort (t,e) ->
      Some (doc_printf "@ @[<hov>%a@]"
              (Jkind.Violation.report_with_offender_sort
                 ~offender:(fun ppf -> type_expr ppf t)
                 env) e)
  | Errortrace.Unequal_var_jkinds (t1,k1,t2,k2) ->
      let fmt_history t k ppf =
        Jkind.(format_history env ~intro:(
          dprintf "The layout of %a is %a" prepared_type_expr t
            (format env) k) ppf k)
      in
      Some (doc_printf "@ because the layouts of their variables are different.\
                     @ @[<v>%t@;%t@]"
              (fmt_history t1 k1) (fmt_history t2 k2))
  | Errortrace.Unequal_tof_kind_jkinds (k1, k2) ->
      let fmt_history which k ppf =
        Jkind.(format_history env ~intro:(
          dprintf "The kind of %s is %a" which (format env) k) ppf k)
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
      print_reduced_evals := true;
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

(* Adapt functions to exposed interface *)
let abbreviate ~abbrev f =
  f ?abbrev:(if abbrev then Some (Abbrev.abbrev ()) else None)

let tree_of_path = tree_of_path None
let tree_of_module ident ?(ellipsis = false) =
  tree_of_module ident ?abbrev:(if ellipsis then Some (Abbrev.ellipsis ()) else None)
let tree_of_signature sg = tree_of_signature sg
let tree_of_modtype ?(abbrev = false) ty =
  abbreviate ~abbrev tree_of_modtype ty
let tree_of_modtype_declaration ?(abbrev = false) id md =
  abbreviate ~abbrev tree_of_modtype_declaration id md
let type_expansion mode ppf ty_exp =
  type_expansion ppf (trees_of_type_expansion mode ty_exp)
let tree_of_type_declaration ident td rs =
  with_hidden_items [{hide=true; ident}]
    (fun () -> tree_of_type_declaration ident td rs)

(** Compatibility module for Format printers *)
module Compat = struct
  let longident = Fmt.compat longident
  let path = Fmt.compat path
  let type_expr = Fmt.compat type_expr
  let shared_type_scheme = Fmt.compat shared_type_scheme
  let signature = Fmt.compat signature
  let class_type = Fmt.compat class_type
  let modtype = Fmt.compat modtype
  let string_of_label (lbl : Asttypes.arg_label) =
    let lbl : Types.arg_label = match lbl with
      | Nolabel -> Nolabel
      | Labelled s -> Labelled s
      | Optional s -> Optional s
    in
    string_of_label lbl
end
=======
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

let same_path t t' =
  eq_type t t' ||
  match get_desc t, get_desc t' with
    Tconstr(p,tl,_), Tconstr(p',tl',_) ->
      let (p1, s1) = best_type_path p and (p2, s2)  = best_type_path p' in
      begin match s1, s2 with
        Nth n1, Nth n2 when n1 = n2 -> true
      | (Id | Map _), (Id | Map _) when Path.same p1 p2 ->
          let tl = apply_subst s1 tl and tl' = apply_subst s2 tl' in
          List.length tl = List.length tl' &&
          List.for_all2 eq_type tl tl'
      | _ -> false
      end
  | _ ->
      false

type 'a diff = Same of 'a | Diff of 'a * 'a

let trees_of_type_expansion'
      ~var_jkinds mode Errortrace.{ty = t; expanded = t'} =
  let tree_of_typexp' ty =
    let out = tree_of_typexp mode ty in
    if var_jkinds then
      match get_desc ty with
      | Tvar { jkind; _ } | Tunivar { jkind; _ } ->
          let okind = out_jkind_of_desc !printing_env (Jkind.get jkind) in
          Otyp_jkind_annot (out, okind)
      | _ ->
          out
    else
      out
  in
  reset_loop_marks ();
  mark_loops t;
  if same_path t t'
  then begin add_delayed (proxy t); Same (tree_of_typexp' t) end
  else begin
    mark_loops t';
    let t' = if proxy t == proxy t' then unalias t' else t' in
    (* beware order matter due to side effect,
       e.g. when printing object types *)
    print_reduced_evals := false; (* preserve unreduced eval in types *)
    let first = tree_of_typexp' t in
    print_reduced_evals := true;
    let second = tree_of_typexp' t' in
    if first = second then Same first
    else Diff(first,second)
  end

let trees_of_type_expansion =
  trees_of_type_expansion' ~var_jkinds:false

let pp_type ppf t =
  Style.as_inline_code !Oprint.out_type ppf t

let quoted_ident ppf t =
  Style.as_inline_code !Oprint.out_ident ppf t

let type_expansion ppf = function
  | Same t -> pp_type ppf t
  | Diff(t,t') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
        pp_type t
        pp_type t'

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
      newty2 ~level:(get_level t)
        (Tvariant
           (create_row ~fields ~fixed ~closed ~name:None
              ~more:(newvar2 (get_level more)
                       (Jkind.Builtin.value ~why:Row_variable))))
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
  Fmt.dprintf "%a" !Oprint.out_ident (tree_of_path (Some Type) p)

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let print_tags ppf tags  =
  Fmt.(pp_print_list ~pp_sep:comma) print_tag ppf tags

let is_unit_arg env ty =
  let ty, vars = tpoly_get_poly ty in
  if vars <> [] then false
  else begin
    (* CR metaprogramming jbachurski: Remove [contains_toplevel_splice] and
       track the stage in errors so we don't need this. See ticket 6726. *)
    let env =
      if Ctype.contains_toplevel_splice (Env.stage env :> int) ty
      then Env.enter_future env
      else env
    in
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
                 env) e)
  | Errortrace.Bad_jkind_sort (t,e) ->
      Some (doc_printf "@ @[<hov>%a@]"
              (Jkind.Violation.report_with_offender_sort
                 ~offender:(fun ppf -> type_expr ppf t)
                 env) e)
  | Errortrace.Unequal_var_jkinds (t1,k1,t2,k2) ->
      let fmt_history t k ppf =
        Jkind.(format_history env ~intro:(
          dprintf "The layout of %a is %a" prepared_type_expr t
            (format env) k) ppf k)
      in
      Some (doc_printf "@ because the layouts of their variables are different.\
                     @ @[<v>%t@;%t@]"
              (fmt_history t1 k1) (fmt_history t2 k2))
  | Errortrace.Unequal_tof_kind_jkinds (k1, k2) ->
      let fmt_history which k ppf =
        Jkind.(format_history env ~intro:(
          dprintf "The kind of %s is %a" which (format env) k) ppf k)
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
      print_reduced_evals := true;
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

(* Adapt functions to exposed interface *)
let abbreviate ~abbrev f =
  f ?abbrev:(if abbrev then Some (Abbrev.abbrev ()) else None)

let tree_of_path = tree_of_path None
let tree_of_module ident ?(ellipsis = false) =
  tree_of_module ident ?abbrev:(if ellipsis then Some (Abbrev.ellipsis ()) else None)
let tree_of_signature sg = tree_of_signature sg
let tree_of_modtype ?(abbrev = false) ty =
  abbreviate ~abbrev tree_of_modtype ty
let tree_of_modtype_declaration ?(abbrev = false) id md =
  abbreviate ~abbrev tree_of_modtype_declaration id md
let type_expansion mode ppf ty_exp =
  type_expansion ppf (trees_of_type_expansion mode ty_exp)
let tree_of_type_declaration ident td rs =
  with_hidden_items [{hide=true; ident}]
    (fun () -> tree_of_type_declaration ident td rs)

(** Compatibility module for Format printers *)
module Compat = struct
  let longident = Fmt.compat longident
  let path = Fmt.compat path
  let type_expr = Fmt.compat type_expr
  let shared_type_scheme = Fmt.compat shared_type_scheme
  let signature = Fmt.compat signature
  let class_type = Fmt.compat class_type
  let modtype = Fmt.compat modtype
  let string_of_label (lbl : Asttypes.arg_label) =
    let lbl : Types.arg_label = match lbl with
      | Nolabel -> Nolabel
      | Labelled s -> Labelled s
      | Optional s -> Optional s
    in
    string_of_label lbl
end
>>>>>>> dd4e8507373d22fb295422eb6dd3d997c76c47cb
