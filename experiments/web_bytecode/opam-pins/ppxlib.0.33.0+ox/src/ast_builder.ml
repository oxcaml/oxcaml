open! Import

(* Internally to Jane Street, shadow the auto-generated [pexp_fun] and [pexp_function]
   bindings. We only want to export versions of these functions that know how to add
   arity-related attributes.

   This code can go away when we update to a version of the OCaml compiler that
   includes https://github.com/ocaml/ocaml/pull/12236.
*)
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

  let value_binding = Ppxlib_jane.Ast_builder.Default.value_binding ~modes:[]

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
    | { pexp_desc = Pexp_apply (e, args); pexp_attributes = []; _ }, _ ->
        { e with pexp_desc = Pexp_apply (e, args @ el) }
    | _ -> pexp_apply ~loc e el

  let eapply ~loc e el =
    pexp_apply ~loc e (List.map el ~f:(fun e -> (Asttypes.Nolabel, e)))

  let pexp_function ~loc a : expression =
    Ppxlib_jane.Ast_builder.Default.unary_function ~loc a

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
  let pexp_function t : expression = Default.pexp_function ~loc t

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
