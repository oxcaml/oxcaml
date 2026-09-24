open! Import

(* In OxCaml, shadow the auto-generated [pexp_function] binding. The
   auto-generated version exposes our compiler's [function_constraint] record, which
   includes mode annotations; we only want to export a version with upstream ppxlib's
   signature. Ppxes that need modes should use [Ppxlib_jane]'s builders. *)
(* Also shadow nodes than our compiler has changed, since ppxes should use [Ppxlib_jane]'s
   version so as to stay upstream compatible *)
module Bindings_to_shadow = struct
  let pexp_fun = `Shadowed
  let pexp_function = `Shadowed
  let label_declaration = `Shadowed
  let value_description = `Shadowed
  let ptyp_arrow = `Shadowed
  let ppat_constraint = `Shadowed
  let pexp_constraint = `Shadowed
  let pexp_let = `Shadowed
  let value_binding = `Shadowed
  let include_infos = `Shadowed
  let psig_include = `Shadowed
  let pcstr_tuple = `Shadowed
  let module_declaration = `Shadowed
  let pmty_functor = `Shadowed
  let pmod_constraint = `Shadowed

  (* [signature] is a bit different as we don't explicitly shadow it.
     Instead, we encourage people use the version that's automatically
     compatible with open-source code. *)
  let signature = `Use_Ppxlib_jane

  let () = ignore (
    pexp_fun,
    pexp_function,
    label_declaration,
    value_description,
    ptyp_arrow,
    ppat_constraint,
    pexp_constraint,
    pexp_let,
    value_binding,
    include_infos,
    psig_include,
    signature,
    pcstr_tuple,
    module_declaration,
    pmty_functor,
    pmod_constraint
  )
end

module Ast_builder_generated = struct
  include Ast_builder_generated

  module M = struct
    include Ast_builder_generated.M
    include Bindings_to_shadow
  end

  module Make (Loc : Ast_builder_intf.Loc) = struct
    include Make (Loc)
    include Bindings_to_shadow
  end
end

(* Converts a pair of [pattern] and [expression] for value_binding to
   the proper triple [pattern], [expression] and [pvb_constraint]. *)
let to_pvb_constraint ~pvb_pat ~pvb_expr =
  (* Copied and adapted from OCaml 5.0 Ast_helper
     Replaces [Ptyp_constr]s naming one of [vars] with the corresponding [Ptyp_var]. *)
  let varify_constructors var_names t =
    let var_names = List.map ~f:(fun (v, _) -> v.Location.txt) var_names in
    let rec loop t =
      let desc =
        match t.ptyp_desc with
        | Ptyp_any x -> Ptyp_any x
        | Ptyp_var (x1, x2) -> Ptyp_var (x1, x2)
        | Ptyp_arrow (label, core_type, core_type', m1, m2) ->
            Ptyp_arrow (label, loop core_type, loop core_type', m1, m2)
        | Ptyp_tuple lst ->
            Ptyp_tuple (List.map ~f:(fun (l, t) -> (l, loop t)) lst)
        | Ptyp_unboxed_tuple lst ->
            Ptyp_unboxed_tuple (List.map ~f:(fun (l, t) -> (l, loop t)) lst)
        | Ptyp_constr ({ txt = Astlib.Longident.Lident s; _ }, [])
          when List.mem s ~set:var_names ->
            Ptyp_var (s, None)
        | Ptyp_constr (longident, lst) ->
            Ptyp_constr (longident, List.map ~f:loop lst)
        | Ptyp_object (lst, o) ->
            Ptyp_object (List.map ~f:loop_object_field lst, o)
        | Ptyp_class (longident, lst) ->
            Ptyp_class (longident, List.map ~f:loop lst)
        | Ptyp_alias (core_type, string, j) ->
            Ptyp_alias (loop core_type, string, j)
        | Ptyp_variant (row_field_list, flag, lbl_lst_option) ->
            Ptyp_variant
              (List.map ~f:loop_row_field row_field_list, flag, lbl_lst_option)
        | Ptyp_poly (string_lst, core_type) ->
            Ptyp_poly (string_lst, loop core_type)
        | Ptyp_package (longident, lst) ->
            Ptyp_package
              (longident, List.map ~f:(fun (n, typ) -> (n, loop typ)) lst)
        | Ptyp_quote core_type -> Ptyp_quote (loop core_type)
        | Ptyp_splice core_type -> Ptyp_splice (loop core_type)
        | Ptyp_of_kind x1 -> Ptyp_of_kind x1
        | Ptyp_repr (vars, core_type) -> Ptyp_repr (vars, loop core_type)
        | Ptyp_newlayout (vars, core_type) -> Ptyp_newlayout (vars, loop core_type)
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
  let no_jkinds tyvars =
    List.for_all tyvars ~f:(fun (_, jkind) -> not (Option.is_some jkind))
  in
  let resugarable_value_binding p e =
    let value_pattern =
      match p with
      | {
       ppat_desc =
         Ppat_constraint
           ( ({ ppat_desc = Ppat_var _; _ } as pat),
             Some ({ ptyp_desc = Ptyp_poly (args_tyvars, rt); _ } as ty_ext),
             [] );
       ppat_attributes = [];
       _;
      }
        when (match rt.ptyp_desc with Ptyp_poly _ -> false | _ -> true) ->
          let ty = match args_tyvars with [] -> rt | _ -> ty_ext in
          `Var (pat, args_tyvars, rt, ty)
      | { ppat_desc = Ppat_constraint (pat, Some rt, []); ppat_attributes = []; _ } ->
          `NonVar (pat, rt)
      | _ -> `None
    in
    let rec value_exp tyvars e =
      match e with
      | { pexp_desc = Pexp_newtype (tyvar, jkind, e); pexp_attributes = []; _ } ->
          value_exp ((tyvar, jkind) :: tyvars) e
      | { pexp_desc = Pexp_constraint (e, Some ct, []); pexp_attributes = []; _ } ->
          Some (List.rev tyvars, e, ct)
      | _ -> None
    in
    let value_exp = value_exp [] e in
    match (value_pattern, value_exp) with
    | `Var (p, pt_tyvars, pt_ct, extern_ct), Some (e_tyvars, inner_e, e_ct)
    (* Note that this comparison takes locations (and jkind annotations) into account:
        we only resugar the [Pexp_newtype] encoding when it was produced by mechanical
        desugaring of [let x : type a. ... = ...] (in which case the pattern and
        expression share the type variables), not when the pattern and expression were
        independently annotated. *)
      when Poly.equal pt_tyvars e_tyvars ->
        let ety = varify_constructors e_tyvars e_ct in
        if no_jkinds pt_tyvars && Poly.(ety = pt_ct) then
          `Desugared_locally_abstract
            (p, List.map pt_tyvars ~f:(fun (v, _) -> v), e_ct, inner_e)
        else
          (* the expression constraint and the pattern constraint either have jkinds or
           don't match, but we still have a Ptyp_poly pattern constraint that
           should be resugared to a value binding *)
          `Univars (p, pt_tyvars, extern_ct, e)
    | `Var (p, pt_tyvars, _pt_ct, extern_ct), _ ->
        `Univars (p, pt_tyvars, extern_ct, e)
    | `NonVar (p, pt_ct), Some ([], e, e_ct) when Poly.equal pt_ct e_ct ->
        `NonVar (p, pt_ct, e)
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
    let ppat_construct ~loc lid p =
      {
        ppat_loc_stack = [];
        ppat_attributes = [];
        ppat_loc = loc;
        ppat_desc = Ppat_construct (lid, p);
      }

    let constructor_declaration ~loc ~name ~vars ~args ~res () =
      let vars = List.map vars ~f:(fun var -> var, None) in
      constructor_declaration ~loc ~name ~vars ~args ~res

    let pmty_signature = pmty_signature
    let signature = Ppxlib_jane.Ast_builder.Default.signature

    let label_declaration =
      Ppxlib_jane.Ast_builder.Default.label_declaration

  end

  (*------ stable layer above Ast_builder_generated.M -----*)
  let ppat_construct ~loc lid p =
    {
      ppat_loc_stack = [];
      ppat_attributes = [];
      ppat_loc = loc;
      ppat_desc = Ppat_construct (lid, Option.map p ~f:(fun p -> ([], p)));
    }

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
    match Ppxlib_jane.Shim.Pexp_function.of_parsetree e.pexp_desc ~loc:e.pexp_loc with
    | None | Some (_, _, Pfunction_cases _) -> e
    | Some (params, constraint_, body)
      when not (Ppxlib_jane.Shim.Pexp_function.Function_constraint.is_none constraint_) ->
      Ppxlib_jane.Ast_builder.Default.Latest.pexp_function
        params
        constraint_
        body
        ~loc:e.pexp_loc
        ~attrs:e.pexp_attributes
    | Some (params1, _, Pfunction_body ({ pexp_attributes = []; _ } as outer_body)) ->
      (match
         Ppxlib_jane.Shim.Pexp_function.of_parsetree outer_body.pexp_desc ~loc:outer_body.pexp_loc
       with
       | Some (params2, constraint_, body) ->
           Ppxlib_jane.Ast_builder.Default.Latest.pexp_function
             (params1 @ params2)
             constraint_
             body
             ~loc:e.pexp_loc
             ~attrs:e.pexp_attributes
       | None -> e)
    | Some _ -> e

  (* override changed nodes to use [Ppxlib_jane] interface *)
  let label_declaration =
    Ppxlib_jane.Ast_builder.Default.label_declaration ~modalities:[]

  let value_description =
    Ppxlib_jane.Ast_builder.Default.value_description ~modalities:[]

  let pmty_signature ~loc x =
    pmty_signature ~loc (Ppxlib_jane.Ast_builder.Default.signature ~loc ~modalities:[] x)

  let ptyp_arrow ~loc arg_label arg_type result_type =
    Ppxlib_jane.Ast_builder.Default.ptyp_arrow ~loc
      { arg_label; arg_type; arg_modes = [] }
      { result_type; result_modes = [] }

  let pexp_let ~loc a b c =
    Ppxlib_jane.Ast_builder.Default.pexp_let ~loc Immutable a b c

  let pexp_constraint ~loc a b =
    Ppxlib_jane.Ast_builder.Default.pexp_constraint ~loc a (Some b) []

  let ppat_constraint ~loc a b =
    Ppxlib_jane.Ast_builder.Default.ppat_constraint ~loc a (Some b) []

  let value_binding ~loc ~pat ~expr =
    let pat, expr, constraint_ =
      to_pvb_constraint ~pvb_pat:pat ~pvb_expr:expr
    in
    Ppxlib_jane.Ast_builder.Default.value_binding ~loc ~pat ~expr ~constraint_ ~modes:[]

  let include_infos = Ppxlib_jane.Ast_builder.Default.include_infos ~kind:Structure

  let psig_include ~loc a =
    Ppxlib_jane.Ast_builder.Default.psig_include ~loc ~modalities:[] a

  let module_declaration ~loc ~name ~type_ =
    Ppxlib_jane.Ast_builder.Default.module_declaration ~loc name type_

  let pmty_functor ~loc param mty =
    Ppxlib_jane.Ast_builder.Default.pmty_functor ~loc param mty

  let pmod_constraint ~loc expr mty =
    Ppxlib_jane.Ast_builder.Default.pmod_constraint ~loc expr (Some mty) []

  (* ----------------------------------------------------- *)

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
  let pexp_tuple ~loc l = match l with [ x ] -> x | _ -> pexp_tuple ~loc (List.map ~f:(fun e -> None, e) l)
  let ppat_tuple ~loc l = match l with [ x ] -> x | _ -> ppat_tuple ~loc (List.map ~f:(fun p -> None, p) l) Closed
  let ptyp_tuple ~loc l = match l with [ x ] -> x | _ -> ptyp_tuple ~loc (List.map ~f:(fun t -> None, t) l)

  let pexp_tuple_opt ~loc l =
    match l with [] -> None | _ :: _ -> Some (pexp_tuple ~loc l)

  let ppat_tuple_opt ~loc l =
    match l with [] -> None | _ :: _ -> Some (ppat_tuple ~loc l)

  let pexp_array ~loc l = pexp_array ~loc Mutable l
  let ppat_array ~loc l = ppat_array ~loc Mutable l

  let ptyp_poly ~loc vars ty =
    match vars with [] -> ty | _ -> ptyp_poly ~loc (List.map vars ~f:(fun v -> v, None)) ty

  let pexp_apply ~loc e el =
    match (e, el) with
    | _, [] -> e
    | { pexp_desc = Pexp_apply (func, args); pexp_attributes = []; _ }, _ ->
        { e with pexp_desc = Pexp_apply (func, args @ el) }
    | _ -> pexp_apply ~loc e el

  let eapply ~loc e el =
    pexp_apply ~loc e (List.map el ~f:(fun e -> (Asttypes.Nolabel, e)))

  let pexp_function ~loc params return_constraint body : expression =
    Ppxlib_jane.Ast_builder.Default.Latest.pexp_function
      ~loc
      params
      { Ppxlib_jane.Shim.Pexp_function.Function_constraint.none with
        ret_type_constraint = return_constraint
      }
      body

  let pexp_function_cases ~loc cases : expression =
    Ppxlib_jane.Ast_builder.Default.pexp_function_cases ~loc cases

  let pexp_fun ~loc a b c d : expression =
    Ppxlib_jane.Ast_builder.Default.add_fun_param ~loc a b c d

  let eabstract ~loc a b : expression =
    Ppxlib_jane.Ast_builder.Default.eabstract ~loc a b

  let ptyp_any ~loc = ptyp_any ~loc None
  let ptyp_var ~loc a = ptyp_var ~loc a None
  let ptyp_alias ~loc a b = ptyp_alias ~loc a (Some b) None
  let pexp_newtype ~loc a b = pexp_newtype ~loc a None b

  let type_declaration ~loc ~name ~params ~cstrs ~kind ~private_ ~manifest =
    type_declaration ~loc ~name ~params ~cstrs ~kind ~private_ ~manifest
      ~jkind_annotation:None

  let esequence ~loc el =
    match List.rev el with
    | [] -> eunit ~loc
    | hd :: tl ->
        List.fold_left tl ~init:hd ~f:(fun acc e -> pexp_sequence ~loc e acc)

  let pconstruct cd arg =
    ppat_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg

  let econstruct cd arg =
    pexp_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg

  let rec elist ~loc l =
    match l with
    | [] -> pexp_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    | x :: l ->
        pexp_construct ~loc
          (Located.mk ~loc (Longident.Lident "::"))
          (Some (pexp_tuple ~loc [ x; elist ~loc l ]))

  let rec plist ~loc l =
    match l with
    | [] -> ppat_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    | x :: l ->
        ppat_construct ~loc
          (Located.mk ~loc (Longident.Lident "::"))
          (Some (ppat_tuple ~loc [ x; plist ~loc l ]))

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
    let rec split_params rev_prefix suffix =
      match suffix with
      | { pparam_desc = Pparam_val (label, None, subpat); pparam_loc = _ } :: suffix ->
         (match subpat with
          | { ppat_desc = Ppat_var name;
              ppat_attributes = [];
              ppat_loc = _;
              ppat_loc_stack = _;
            } ->
            split_params ((label, name, None, []) :: rev_prefix) suffix
          | { ppat_desc =
                Ppat_constraint
                  ( {
                    ppat_desc = Ppat_var name;
                    ppat_attributes = [];
                    ppat_loc = _;
                    ppat_loc_stack = _;
                  },
                    ty,
                    modes );
              ppat_attributes = [];
              ppat_loc = _;
              ppat_loc_stack = _;
            } ->
            (* We reduce [fun (x : ty) -> f x] by rewriting it [(f : ty -> _)]. *)
            split_params ((label, name, ty, modes) :: rev_prefix) suffix
          | _ -> List.rev rev_prefix, suffix)
      | _ -> List.rev rev_prefix, suffix
    in
    let gather_params expr =
      match expr with
      | { pexp_desc = Pexp_function (params, constraint_, Pfunction_body body);
          pexp_attributes = [];
          pexp_loc = _;
          pexp_loc_stack = _;
        } when Ppxlib_jane.Shim.Pexp_function.Function_constraint.is_none constraint_ ->
         let gathered_prefix, suffix = split_params [] params in
         (match suffix with
          | [] -> gathered_prefix, body
          | _ -> [], expr)
      | _ -> [], expr
    in
    let annotate ~loc expr params =
      if List.exists params
           ~f:(fun (_, _, ty, modes) -> Option.is_some ty || not (List.is_empty modes))
      then
        let ty =
          List.fold_right params ~init:(ptyp_any ~loc)
            ~f:(fun (arg_label, param, ty_opt, arg_modes) acc ->
              let loc = param.loc in
              let ty =
                match ty_opt with None -> ptyp_any ~loc | Some ty -> ty
              in
              Ppxlib_jane.Ast_builder.Default.ptyp_arrow ~loc
                { arg_label; arg_type = ty; arg_modes }
                { result_type = acc; result_modes = [] })
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
      let params, body = gather_params expr in
      match gather_args (List.length params) body with
      | None -> None
      | Some (({ pexp_desc = Pexp_ident _; _ } as f_ident), args) -> (
          match
            List.for_all2 args params
              ~f:(fun (arg_label, arg) (param_label, param, _, _) ->
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
      longident loc -> ((label loc * jkind_annotation option) list * pattern) option -> pattern

    val constructor_declaration :
      name:label loc ->
      vars:label loc list ->
      args:constructor_arguments ->
      res:core_type option ->
      unit ->
      constructor_declaration

    val pmty_signature : signature -> module_type
    val signature : ?modalities:modalities -> signature_item list -> signature
    val label_declaration :
    name:string loc ->
    mutable_:mutable_flag ->
    modalities:modalities ->
    type_:core_type ->
    label_declaration

  end

  val ppat_construct : longident loc -> pattern option -> pattern

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

    let constructor_declaration ~name ~vars ~args ~res () =
      let vars = List.map vars ~f:(fun var -> var, None) in
      constructor_declaration ~name ~vars ~args ~res

    let pmty_signature = pmty_signature
    let signature = Ppxlib_jane.Ast_builder.Default.signature ~loc

    let label_declaration = Ppxlib_jane.Ast_builder.Default.label_declaration ~loc
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

  (* override changed nodes to use [Ppxlib_jane] interface *)
  let label_declaration ~name ~mutable_ ~type_ =
    Default.label_declaration ~loc ~name ~mutable_ ~type_

  let value_description ~name ~type_ ~prim =
    Default.value_description ~loc ~name ~type_ ~prim

  let ptyp_arrow a b c = Default.ptyp_arrow ~loc a b c
  let pexp_let a b c = Default.pexp_let ~loc a b c
  let pexp_constraint a b = Default.pexp_constraint ~loc a b
  let ppat_constraint a b = Default.ppat_constraint ~loc a b

  let value_binding ~pat ~expr =
    Default.value_binding ~loc ~pat ~expr

  let include_infos ?attrs a = Default.include_infos ~loc ?attrs a

  let psig_include a = Default.psig_include ~loc a

  let module_declaration = Default.module_declaration ~loc

  let pmty_functor = Default.pmty_functor ~loc

  let pmod_constraint = Default.pmod_constraint ~loc

  (* ----------------------------------------------------- *)

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
  let pexp_tuple_opt l = Default.pexp_tuple_opt ~loc l
  let ppat_tuple_opt l = Default.ppat_tuple_opt ~loc l
  let pexp_array l = Default.pexp_array ~loc l
  let ppat_array l = Default.ppat_array ~loc l
  let ptyp_poly vars ty = Default.ptyp_poly ~loc vars ty
  let pexp_apply e el = Default.pexp_apply ~loc e el
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
  let elist l = Default.elist ~loc l
  let plist l = Default.plist ~loc l

  let ptyp_any = Default.ptyp_any ~loc
  let ptyp_var a = Default.ptyp_var ~loc a
  let ptyp_alias a b = Default.ptyp_alias ~loc a b
  let pexp_newtype a b = Default.pexp_newtype ~loc a b

  let pexp_fun a b c d : expression = Default.pexp_fun ~loc a b c d
  let pexp_function a b c : expression = Default.pexp_function ~loc a b c
  let pexp_function_cases t : expression = Default.pexp_function_cases ~loc t

  let type_constr_conv ident ~f args =
    Default.type_constr_conv ~loc ident ~f args

  let unapplied_type_constr_conv ident ~f =
    Default.unapplied_type_constr_conv ~loc ident ~f

  let type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest =
    Default.type_declaration ~loc ~name ~params ~cstrs ~kind ~private_ ~manifest

  let eta_reduce = Default.eta_reduce
  let eta_reduce_if_possible = Default.eta_reduce_if_possible

  let eta_reduce_if_possible_and_nonrec =
    Default.eta_reduce_if_possible_and_nonrec

  let pmty_signature xs = Default.pmty_signature ~loc xs
end

let make loc =
  (module Make (struct
    let loc = loc
  end) : S)
