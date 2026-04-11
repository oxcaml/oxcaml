open! Import

(* Converts a pair of [pattern] and [expression] for value_binding to
   the proper triple [pattern], [expression] and [pvb_constraint]. *)
let to_pvb_constraint ~pvb_pat ~pvb_expr =
  (* Copied and adapted from OCaml 5.0 Ast_helper *)
  let varify_constructors var_names t =
    let var_names = List.map ~f:(fun v -> v.Location.txt) var_names in
    let rec loop t =
      let desc =
        match t.ptyp_desc with
        | Ptyp_any -> Ptyp_any
        | Ptyp_var x -> Ptyp_var x
        | Ptyp_arrow (label, core_type, core_type') ->
            Ptyp_arrow (label, loop core_type, loop core_type')
        | Ptyp_tuple lst -> Ptyp_tuple (List.map ~f:loop lst)
        | Ptyp_constr ({ txt = Longident.Lident s; _ }, [])
          when List.mem s ~set:var_names ->
            Ptyp_var s
        | Ptyp_constr (longident, lst) ->
            Ptyp_constr (longident, List.map ~f:loop lst)
        | Ptyp_object (lst, o) ->
            Ptyp_object (List.map ~f:loop_object_field lst, o)
        | Ptyp_class (longident, lst) ->
            Ptyp_class (longident, List.map ~f:loop lst)
        | Ptyp_alias (core_type, string) -> Ptyp_alias (loop core_type, string)
        | Ptyp_variant (row_field_list, flag, lbl_lst_option) ->
            Ptyp_variant
              (List.map ~f:loop_row_field row_field_list, flag, lbl_lst_option)
        | Ptyp_poly (string_lst, core_type) ->
            Ptyp_poly (string_lst, loop core_type)
        | Ptyp_package (longident, lst) ->
            Ptyp_package
              (longident, List.map ~f:(fun (n, typ) -> (n, loop typ)) lst)
        | Ptyp_open (longident, typ) -> Ptyp_open (longident, loop typ)
        | Ptyp_extension (s, arg) -> Ptyp_extension (s, arg)
      in
      { t with ptyp_desc = desc }
    and loop_row_field field =
      let prf_desc =
        match field.prf_desc with
        | Rtag (label, flag, lst) -> Rtag (label, flag, List.map ~f:loop lst)
        | Rinherit t -> Rinherit (loop t)
      in
      { field with prf_desc }
    and loop_object_field field =
      let pof_desc =
        match field.pof_desc with
        | Otag (label, t) -> Otag (label, loop t)
        | Oinherit t -> Oinherit (loop t)
      in
      { field with pof_desc }
    in
    loop t
  in
  (* Match the form of the expr and pattern to decide the value of
     [pvb_constraint]. Adapted from OCaml 5.0 PPrinter. *)
  let tyvars_str tyvars = List.map ~f:(fun v -> v.Location.txt) tyvars in
  let resugarable_value_binding p e =
    let value_pattern =
      match p with
      | {
       ppat_desc =
         Ppat_constraint
           ( ({ ppat_desc = Ppat_var _; _ } as pat),
             ({ ptyp_desc = Ptyp_poly (args_tyvars, rt); _ } as ty_ext) );
       ppat_attributes = [];
       _;
      } ->
          assert (match rt.ptyp_desc with Ptyp_poly _ -> false | _ -> true);
          let ty = match args_tyvars with [] -> rt | _ -> ty_ext in
          `Var (pat, args_tyvars, rt, ty)
      | { ppat_desc = Ppat_constraint (pat, rt); ppat_attributes = []; _ } ->
          `NonVar (pat, rt)
      | _ -> `None
    in
    let rec value_exp tyvars e =
      match e with
      | { pexp_desc = Pexp_newtype (tyvar, e); pexp_attributes = []; _ } ->
          value_exp (tyvar :: tyvars) e
      | { pexp_desc = Pexp_constraint (e, ct); pexp_attributes = []; _ } ->
          Some (List.rev tyvars, e, ct)
      | _ -> None
    in
    let value_exp = value_exp [] e in
    match (value_pattern, value_exp) with
    | `Var (p, pt_tyvars, pt_ct, extern_ct), Some (e_tyvars, inner_e, e_ct)
      when List.equal ~eq:String.equal (tyvars_str pt_tyvars)
             (tyvars_str e_tyvars) ->
        let ety = varify_constructors e_tyvars e_ct in
        if Poly.(ety = pt_ct) then
          `Desugared_locally_abstract (p, pt_tyvars, e_ct, inner_e)
        else
          (* the expression constraint and the pattern constraint,
           don't match, but we still have a Ptyp_poly pattern constraint that
           should be resugared to a value binding *)
          `Univars (p, pt_tyvars, extern_ct, e)
    | `Var (p, pt_tyvars, _pt_ct, extern_ct), _ ->
        `Univars (p, pt_tyvars, extern_ct, e)
    | `NonVar (pat, ct), _ -> `NonVar (pat, ct, e)
    | _ -> `None
  in
  let with_constraint ty_vars typ =
    Some (Pvc_constraint { locally_abstract_univars = ty_vars; typ })
  in
  match resugarable_value_binding pvb_pat pvb_expr with
  | `Desugared_locally_abstract (p, ty_vars, typ, e) ->
      (p, e, with_constraint ty_vars typ)
  | `Univars (pat, [], ct, expr) -> (
      (* check if we are in the [let x : ty? :> coer = expr ] case *)
      match expr with
      | {
       pexp_desc = Pexp_coerce (expr, ground, coercion);
       pexp_attributes = [];
       _;
      } ->
          let pvb_constraint = Some (Pvc_coercion { ground; coercion }) in
          (pat, expr, pvb_constraint)
      | _ -> (pat, expr, with_constraint [] ct))
  | `Univars (pat, _, ct, expr) -> (pat, expr, with_constraint [] ct)
  | `NonVar (p, typ, e) -> (p, e, with_constraint [] typ)
  | `None -> (pvb_pat, pvb_expr, None)

module Default = struct
  module Located = struct
    type 'a t = 'a Loc.t

    let loc (x : _ t) = x.loc
    let mk ~loc x = { loc; txt = x }
    let map f t = { t with txt = f t.txt }
    let map_lident x = map (fun x -> Longident.Lident x) x
    let lident ~loc x = mk ~loc (Longident.parse x)
  end

  include Ast_builder_generated.M

  module Latest = struct
    let ppat_construct = ppat_construct
    let pexp_function = pexp_function

    let value_binding ?constraint_ ~loc ~pat ~expr () =
      value_binding ~constraint_ ~loc ~pat ~expr

    let constructor_declaration ~loc ~name ~vars ~args ~res () =
      constructor_declaration ~loc ~name ~vars ~args ~res
  end

  (*------ stable layer above Ast_builder_generated.M -----*)
  let ppat_construct ~loc lid p =
    {
      ppat_loc_stack = [];
      ppat_attributes = [];
      ppat_loc = loc;
      ppat_desc = Ppat_construct (lid, Option.map p ~f:(fun p -> ([], p)));
    }

  let pexp_function_cases ~loc cases =
    {
      pexp_loc_stack = [];
      pexp_attributes = [];
      pexp_loc = loc;
      pexp_desc = Pexp_function ([], None, Pfunction_cases (cases, loc, []));
    }

  let add_fun_params return_constraint ~loc params body =
    match params with
    | [] -> body
    | _ -> (
        match body.pexp_desc with
        | Pexp_function (more_params, constraint_, func_body) ->
            pexp_function ~loc (params @ more_params) constraint_ func_body
        | _ ->
            assert (match params with [] -> false | _ -> true);
            pexp_function ~loc params return_constraint (Pfunction_body body))

  let pexp_fun ~loc (label : arg_label) expr p e =
    let param : function_param =
      { pparam_desc = Pparam_val (label, expr, p); pparam_loc = loc }
    in
    add_fun_params ~loc None [ param ] e

  let value_binding ~loc ~pat ~expr =
    let pat, expr, constraint_ =
      to_pvb_constraint ~pvb_pat:pat ~pvb_expr:expr
    in
    value_binding ~loc ~pat ~expr ~constraint_

  let constructor_declaration ~loc ~name ~args ~res =
    {
      pcd_name = name;
      pcd_vars = [];
      pcd_args = args;
      pcd_res = res;
      pcd_loc = loc;
      pcd_attributes = [];
    }

  (*-------------------------------------------------------*)

  let coalesce_arity e =
    match e.pexp_desc with
    (* We stop coalescing parameters if there is a constraint on the result of a function
       (i.e [fun x y : T -> ...] or the body is a function_case. *)
    | Pexp_function (_, Some _, _) | Pexp_function (_, _, Pfunction_cases _) ->
        e
    | Pexp_function
        (params1, None, Pfunction_body ({ pexp_attributes = []; _ } as body1))
      -> (
        match body1.pexp_desc with
        | Pexp_function (params2, constraint_, body2) ->
            Latest.pexp_function ~loc:e.pexp_loc (params1 @ params2) constraint_
              body2
        | _ -> e)
    | _ -> e

  let pstr_value_list ~loc rec_flag = function
    | [] -> []
    | vbs -> [ pstr_value ~loc rec_flag vbs ]

  let nonrec_type_declaration ~loc:_ ~name:_ ~params:_ ~cstrs:_ ~kind:_
      ~private_:_ ~manifest:_ =
    failwith
      "Ppxlib.Ast_builder.nonrec_type_declaration: don't use this function"

  let eint ~loc t = pexp_constant ~loc (Pconst_integer (Int.to_string t, None))
  let echar ~loc t = pexp_constant ~loc (Pconst_char t)
  let estring ~loc t = pexp_constant ~loc (Pconst_string (t, loc, None))
  let efloat ~loc t = pexp_constant ~loc (Pconst_float (t, None))

  let eint32 ~loc t =
    pexp_constant ~loc (Pconst_integer (Int32.to_string t, Some 'l'))

  let eint64 ~loc t =
    pexp_constant ~loc (Pconst_integer (Int64.to_string t, Some 'L'))

  let enativeint ~loc t =
    pexp_constant ~loc (Pconst_integer (Nativeint.to_string t, Some 'n'))

  let pint ~loc t = ppat_constant ~loc (Pconst_integer (Int.to_string t, None))
  let pchar ~loc t = ppat_constant ~loc (Pconst_char t)
  let pstring ~loc t = ppat_constant ~loc (Pconst_string (t, loc, None))
  let pfloat ~loc t = ppat_constant ~loc (Pconst_float (t, None))

  let pint32 ~loc t =
    ppat_constant ~loc (Pconst_integer (Int32.to_string t, Some 'l'))

  let pint64 ~loc t =
    ppat_constant ~loc (Pconst_integer (Int64.to_string t, Some 'L'))

  let pnativeint ~loc t =
    ppat_constant ~loc (Pconst_integer (Nativeint.to_string t, Some 'n'))

  let ebool ~loc t =
    pexp_construct ~loc (Located.lident ~loc (Bool.to_string t)) None

  let pbool ~loc t =
    ppat_construct ~loc (Located.lident ~loc (Bool.to_string t)) None

  let evar ~loc v = pexp_ident ~loc (Located.mk ~loc (Longident.parse v))
  let pvar ~loc v = ppat_var ~loc (Located.mk ~loc v)
  let eunit ~loc = pexp_construct ~loc (Located.lident ~loc "()") None
  let punit ~loc = ppat_construct ~loc (Located.lident ~loc "()") None
  let pexp_tuple ~loc l = match l with [ x ] -> x | _ -> pexp_tuple ~loc l
  let ppat_tuple ~loc l = match l with [ x ] -> x | _ -> ppat_tuple ~loc l
  let ptyp_tuple ~loc l = match l with [ x ] -> x | _ -> ptyp_tuple ~loc l

  let ppat_effect ~loc effect_ k =
    let ppat_desc =
      Astlib__.Encoding_503.To_502.encode_ppat_effect ~loc ~effect_ ~k
    in
    { ppat_desc; ppat_loc = loc; ppat_attributes = []; ppat_loc_stack = [] }

  let ptyp_labeled_tuple ~loc l =
    let ptyp_desc =
      Astlib__.Encoding_504.To_502.encode_ptyp_labeled_tuple ~loc l
    in
    { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

  let pexp_labeled_tuple ~loc l =
    let pexp_desc =
      Astlib__.Encoding_504.To_502.encode_pexp_labeled_tuple ~loc l
    in
    { pexp_desc; pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [] }

  let ppat_labeled_tuple ~loc l flag =
    let ppat_desc =
      Astlib__.Encoding_504.To_502.encode_ppat_labeled_tuple ~loc l flag
    in
    { ppat_desc; ppat_loc = loc; ppat_attributes = []; ppat_loc_stack = [] }

  let pexp_tuple_opt ~loc l =
    match l with [] -> None | _ :: _ -> Some (pexp_tuple ~loc l)

  let ppat_tuple_opt ~loc l =
    match l with [] -> None | _ :: _ -> Some (ppat_tuple ~loc l)

  let ptyp_poly ~loc vars ty =
    match vars with [] -> ty | _ -> ptyp_poly ~loc vars ty

  let pexp_apply ~loc e el =
    match (e, el) with
    | _, [] -> e
    | { pexp_desc = Pexp_apply (e, args); pexp_attributes = []; _ }, _ ->
        { e with pexp_desc = Pexp_apply (e, args @ el) }
    | _ -> pexp_apply ~loc e el

  let eapply ~loc e el =
    pexp_apply ~loc e (List.map el ~f:(fun e -> (Asttypes.Nolabel, e)))

  let eabstract ~loc ps e =
    List.fold_right ps ~init:e ~f:(fun p e ->
        pexp_fun ~loc Asttypes.Nolabel None p e)

  let esequence ~loc el =
    match List.rev el with
    | [] -> eunit ~loc
    | hd :: tl ->
        List.fold_left tl ~init:hd ~f:(fun acc e -> pexp_sequence ~loc e acc)

  let pconstruct cd arg =
    ppat_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg

  let econstruct cd arg =
    pexp_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg

  let rec elist_tail ~loc l tail =
    match l with
    | [] -> tail
    | x :: l ->
        pexp_construct ~loc
          (Located.mk ~loc (Longident.Lident "::"))
          (Some (pexp_tuple ~loc [ x; elist_tail ~loc l tail ]))

  let elist ~loc l =
    let tail =
      pexp_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    in
    elist_tail ~loc l tail

  let rec plist_tail ~loc l tail =
    match l with
    | [] -> tail
    | x :: l ->
        ppat_construct ~loc
          (Located.mk ~loc (Longident.Lident "::"))
          (Some (ppat_tuple ~loc [ x; plist_tail ~loc l tail ]))

  let plist ~loc l =
    let tail =
      ppat_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    in
    plist_tail ~loc l tail

  let unapplied_type_constr_conv_without_apply ~loc (ident : Longident.t) ~f =
    match ident with
    | Lident n -> pexp_ident ~loc { txt = Lident (f n); loc }
    | Ldot (path, n) -> pexp_ident ~loc { txt = Ldot (path, f n); loc }
    | Lapply _ ->
        Location.raise_errorf ~loc "unexpected applicative functor type"

  let type_constr_conv ~loc:apply_loc { Loc.loc; txt = longident } ~f args =
    let loc = { loc with loc_ghost = true } in
    match (longident : Longident.t) with
    | Lident _ | Ldot ((Lident _ | Ldot _), _) | Lapply _ -> (
        let ident =
          unapplied_type_constr_conv_without_apply longident ~loc ~f
        in
        match args with
        | [] -> ident
        | _ :: _ -> eapply ~loc:apply_loc ident args)
    | Ldot ((Lapply _ as module_path), n) ->
        let suffix_n functor_ = String.uncapitalize_ascii functor_ ^ "__" ^ n in
        let rec gather_lapply functor_args : Longident.t -> Longident.t * _ =
          function
          | Lapply (rest, arg) -> gather_lapply (arg :: functor_args) rest
          | Lident functor_ -> (Lident (suffix_n functor_), functor_args)
          | Ldot (functor_path, functor_) ->
              (Ldot (functor_path, suffix_n functor_), functor_args)
        in
        let ident, functor_args = gather_lapply [] module_path in
        eapply ~loc:apply_loc
          (unapplied_type_constr_conv_without_apply ident ~loc ~f)
          (List.map functor_args ~f:(fun path ->
               pexp_pack ~loc (pmod_ident ~loc { txt = path; loc }))
          @ args)

  let unapplied_type_constr_conv ~loc longident ~f =
    type_constr_conv longident ~loc ~f []

  let eta_reduce =
    let rec gather_params acc expr =
      match expr with
      | {
       pexp_desc =
         Pexp_function
           ( [ { pparam_loc = _; pparam_desc = Pparam_val (label, _, subpat) } ],
             _constraint,
             Pfunction_body body );
       pexp_attributes = [];
       pexp_loc = _;
       pexp_loc_stack = _;
      } -> (
          match subpat with
          | {
           ppat_desc = Ppat_var name;
           ppat_attributes = [];
           ppat_loc = _;
           ppat_loc_stack = _;
          } ->
              gather_params ((label, name, None) :: acc) body
          | {
           ppat_desc =
             Ppat_constraint
               ( {
                   ppat_desc = Ppat_var name;
                   ppat_attributes = [];
                   ppat_loc = _;
                   ppat_loc_stack = _;
                 },
                 ty );
           ppat_attributes = [];
           ppat_loc = _;
           ppat_loc_stack = _;
          } ->
              (* We reduce [fun (x : ty) -> f x] by rewriting it [(f : ty -> _)]. *)
              gather_params ((label, name, Some ty) :: acc) body
          | _ -> (List.rev acc, expr))
      | _ -> (List.rev acc, expr)
    in
    let annotate ~loc expr params =
      if List.exists params ~f:(fun (_, _, ty) -> Option.is_some ty) then
        let ty =
          List.fold_right params ~init:(ptyp_any ~loc)
            ~f:(fun (param_label, param, ty_opt) acc ->
              let loc = param.loc in
              let ty =
                match ty_opt with None -> ptyp_any ~loc | Some ty -> ty
              in
              ptyp_arrow ~loc param_label ty acc)
        in
        pexp_constraint ~loc expr ty
      else expr
    in
    let rec gather_args n x =
      if n = 0 then Some (x, [])
      else
        match x with
        | {
         pexp_desc = Pexp_apply (body, args);
         pexp_attributes = [];
         pexp_loc = _;
         pexp_loc_stack = _;
        } ->
            if List.length args <= n then
              match gather_args (n - List.length args) body with
              | None -> None
              | Some (body, args') -> Some (body, args' @ args)
            else None
        | _ -> None
    in
    fun expr ->
      let params, body = gather_params [] expr in
      match gather_args (List.length params) body with
      | None -> None
      | Some (({ pexp_desc = Pexp_ident _; _ } as f_ident), args) -> (
          match
            List.for_all2 args params
              ~f:(fun (arg_label, arg) (param_label, param, _) ->
                Poly.( = ) (arg_label : arg_label) param_label
                &&
                match arg with
                | {
                 pexp_desc = Pexp_ident { txt = Lident name'; _ };
                 pexp_attributes = [];
                 pexp_loc = _;
                 pexp_loc_stack = _;
                } ->
                    String.( = ) name' param.txt
                | _ -> false)
          with
          | false -> None
          | true -> Some (annotate ~loc:expr.pexp_loc f_ident params))
      | _ -> None

  let eta_reduce_if_possible expr = Option.value (eta_reduce expr) ~default:expr

  let eta_reduce_if_possible_and_nonrec expr ~rec_flag =
    match rec_flag with
    | Recursive -> expr
    | Nonrecursive -> eta_reduce_if_possible expr
end

module type Loc = Ast_builder_intf.Loc

module type S = sig
  include Ast_builder_intf.S

  module Latest : sig
    val ppat_construct :
      longident loc -> (label loc list * pattern) option -> pattern

    val value_binding :
      ?constraint_:value_constraint ->
      pat:pattern ->
      expr:expression ->
      unit ->
      value_binding

    val constructor_declaration :
      name:label loc ->
      vars:label loc list ->
      args:constructor_arguments ->
      res:core_type option ->
      unit ->
      constructor_declaration
  end

  val ppat_construct : longident loc -> pattern option -> pattern
  val value_binding : pat:pattern -> expr:expression -> value_binding

  val constructor_declaration :
    name:label loc ->
    args:constructor_arguments ->
    res:core_type option ->
    constructor_declaration
end

module Make (Loc : sig
  val loc : Location.t
end) : S = struct
  include Ast_builder_generated.Make (Loc)

  module Latest = struct
    let ppat_construct = ppat_construct

    let value_binding ?constraint_ ~pat ~expr () =
      value_binding ~constraint_ ~pat ~expr

    let constructor_declaration ~name ~vars ~args ~res () =
      constructor_declaration ~name ~vars ~args ~res
  end

  (*----- stable layer above Ast_builder_generated.Make (Loc) -----*)

  let ppat_construct lid p =
    {
      ppat_loc_stack = [];
      ppat_attributes = [];
      ppat_loc = loc;
      ppat_desc = Ppat_construct (lid, Option.map p ~f:(fun p -> ([], p)));
    }

  let constructor_declaration ~name ~args ~res =
    {
      pcd_name = name;
      pcd_vars = [];
      pcd_args = args;
      pcd_res = res;
      pcd_loc = loc;
      pcd_attributes = [];
    }

  (*---------------------------------------------------------------*)

  let pstr_value_list = Default.pstr_value_list

  let nonrec_type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest =
    Default.nonrec_type_declaration ~loc ~name ~params ~cstrs ~kind ~private_
      ~manifest

  module Located = struct
    include Default.Located

    let loc _ = Loc.loc
    let mk x = mk ~loc:Loc.loc x
    let lident x = lident ~loc:Loc.loc x
  end

  let pexp_tuple l = Default.pexp_tuple ~loc l
  let ppat_tuple l = Default.ppat_tuple ~loc l
  let ptyp_tuple l = Default.ptyp_tuple ~loc l
  let ppat_effect effect_ k = Default.ppat_effect ~loc effect_ k
  let ptyp_labeled_tuple l = Default.ptyp_labeled_tuple ~loc l
  let pexp_labeled_tuple l = Default.pexp_labeled_tuple ~loc l
  let ppat_labeled_tuple l flag = Default.ppat_labeled_tuple ~loc l flag
  let pexp_tuple_opt l = Default.pexp_tuple_opt ~loc l
  let ppat_tuple_opt l = Default.ppat_tuple_opt ~loc l
  let ptyp_poly vars ty = Default.ptyp_poly ~loc vars ty
  let pexp_apply e el = Default.pexp_apply ~loc e el
  let pexp_fun lbl e1 p e2 = Default.pexp_fun ~loc lbl e1 p e2
  let pexp_function_cases cases = Default.pexp_function_cases ~loc cases
  let eint t = Default.eint ~loc t
  let echar t = Default.echar ~loc t
  let estring t = Default.estring ~loc t
  let efloat t = Default.efloat ~loc t
  let eint32 t = Default.eint32 ~loc t
  let eint64 t = Default.eint64 ~loc t
  let enativeint t = Default.enativeint ~loc t
  let ebool t = Default.ebool ~loc t
  let evar t = Default.evar ~loc t
  let pint t = Default.pint ~loc t
  let pchar t = Default.pchar ~loc t
  let pstring t = Default.pstring ~loc t
  let pfloat t = Default.pfloat ~loc t
  let pint32 t = Default.pint32 ~loc t
  let pint64 t = Default.pint64 ~loc t
  let pnativeint t = Default.pnativeint ~loc t
  let pbool t = Default.pbool ~loc t
  let pvar t = Default.pvar ~loc t
  let eunit = Default.eunit ~loc
  let punit = Default.punit ~loc
  let econstruct = Default.econstruct
  let pconstruct = Default.pconstruct
  let eapply e el = Default.eapply ~loc e el
  let eabstract ps e = Default.eabstract ~loc ps e
  let esequence el = Default.esequence ~loc el
  let elist_tail l tail = Default.elist_tail ~loc l tail
  let plist_tail l tail = Default.plist_tail ~loc l tail
  let elist l = Default.elist ~loc l
  let plist l = Default.plist ~loc l
  let value_binding = Default.value_binding ~loc

  let type_constr_conv ident ~f args =
    Default.type_constr_conv ~loc ident ~f args

  let unapplied_type_constr_conv ident ~f =
    Default.unapplied_type_constr_conv ~loc ident ~f

  let eta_reduce = Default.eta_reduce
  let eta_reduce_if_possible = Default.eta_reduce_if_possible

  let eta_reduce_if_possible_and_nonrec =
    Default.eta_reduce_if_possible_and_nonrec
end

let make loc =
  (module Make (struct
    let loc = loc
  end) : S)
