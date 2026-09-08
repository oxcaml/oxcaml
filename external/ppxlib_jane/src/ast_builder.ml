open Astlib
open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree
open Stdppx
include Ast_builder_intf
include Shim

module Default = struct
  include Shim

  type function_param = Shim.Pexp_function.function_param
  type function_constraint = Shim.Pexp_function.Function_constraint.t
  type function_body = Shim.Pexp_function.function_body

  let mktyp ~loc ?(attrs = []) ptyp_desc =
    { ptyp_loc_stack = []; ptyp_attributes = attrs; ptyp_loc = loc; ptyp_desc }
  ;;

  let mkexp ~loc ?(attrs = []) pexp_desc =
    { pexp_loc_stack = []; pexp_attributes = attrs; pexp_loc = loc; pexp_desc }
  ;;

  let mkpat ~loc ?(attrs = []) ppat_desc =
    { ppat_loc_stack = []; ppat_attributes = attrs; ppat_loc = loc; ppat_desc }
  ;;

  let ptyp_arrow ~loc { arg_label; arg_modes; arg_type } { result_modes; result_type } =
    let ptyp_desc =
      Ptyp_arrow (arg_label, arg_type, result_type, arg_modes, result_modes)
      |> Shim.Core_type_desc.to_parsetree
    in
    mktyp ~loc ptyp_desc
  ;;

  let tarrow ~loc args result =
    match args with
    | [] ->
      raise
        (Invalid_argument
           "tarrow: Can't construct a 0-ary arrow, argument list must be nonempty")
    | _ :: _ ->
      let { result_type; _ } =
        List.fold_right args ~init:result ~f:(fun arg result ->
          { result_type = ptyp_arrow ~loc arg result; result_modes = [] })
      in
      result_type
  ;;

  let tarrow_maybe ~loc args result_type =
    match args with
    | [] -> result_type
    | _ :: _ -> tarrow ~loc args { result_modes = []; result_type }
  ;;

  let pexp_let ~loc a b c d =
    let pexp_desc = Pexp_let (a, b, c, d) |> Shim.Expression_desc.to_parsetree ~loc in
    mkexp ~loc pexp_desc
  ;;

  let pexp_constraint ~loc a b c =
    let pexp_desc = Pexp_constraint (a, b, c) |> Shim.Expression_desc.to_parsetree ~loc in
    mkexp ~loc pexp_desc
  ;;

  let ppat_constraint ~loc a b c =
    let ppat_desc = Ppat_constraint (a, b, c) |> Shim.Pattern_desc.to_parsetree ~loc in
    mkpat ~loc ppat_desc
  ;;

  let type_declaration
    ~loc
    ?attrs
    ?jkind_annotation
    ~name
    ~params
    ~cstrs
    ~kind
    ~private_
    ~manifest
    ()
    : type_declaration
    =
    { ptype_loc = loc
    ; ptype_name = name
    ; ptype_params = params
    ; ptype_cstrs = cstrs
    ; ptype_kind = kind
    ; ptype_private = private_
    ; ptype_manifest = manifest
    ; ptype_jkind_annotation = jkind_annotation
    ; ptype_attributes = Option.value attrs ~default:[]
    }
    |> Shim.Type_declaration.to_parsetree
  ;;

  let value_binding = Shim.Value_binding.create

  let pcstr_tuple ~loc modalities_tys =
    Pcstr_tuple
      (List.map modalities_tys ~f:(fun (modalities, type_) ->
         Shim.Pcstr_tuple_arg.create ~loc ~modalities ~type_))
  ;;

  let psig_include ~loc ~modalities a =
    let psig_desc =
      Psig_include (a, modalities) |> Shim.Signature_item_desc.to_parsetree
    in
    { psig_loc = loc; psig_desc }
  ;;

  let signature ~loc ?(modalities = []) psg_items =
    Shim.Signature.to_parsetree { psg_items; psg_modalities = modalities; psg_loc = loc }
  ;;

  let module_declaration ~loc ?(attrs = []) ?(modalities = []) name type_ =
    Shim.Module_declaration.to_parsetree
      { pmd_name = name
      ; pmd_type = type_
      ; pmd_modalities = modalities
      ; pmd_attributes = attrs
      ; pmd_loc = loc
      }
  ;;

  let pmty_functor ~loc ?(attrs = []) ?(modes = []) param mty =
    let pmty_desc =
      Pmty_functor (param, mty, modes) |> Shim.Module_type_desc.to_parsetree ~loc
    in
    { pmty_desc; pmty_attributes = attrs; pmty_loc = loc }
  ;;

  let pmod_constraint ~loc ?(attrs = []) expr mty modes =
    let pmod_desc =
      Pmod_constraint (expr, mty, modes) |> Shim.Module_expr_desc.to_parsetree ~loc
    in
    { pmod_desc; pmod_attributes = attrs; pmod_loc = loc }
  ;;

  let pmty_signature ~loc signature =
    { pmty_desc = Pmty_signature signature; pmty_loc = loc; pmty_attributes = [] }
  ;;

  let get_tuple_field_modalities = Shim.Pcstr_tuple_arg.extract_modalities
  let get_label_declaration_modalities = Shim.Label_declaration.extract_modalities
  let label_declaration = Shim.Label_declaration.create
  let get_value_description_modalities = Shim.Value_description.extract_modalities
  let value_description = Shim.Value_description.create
  let pcstr_tuple_arg = Shim.Pcstr_tuple_arg.create

  let include_infos ~loc ?(attrs = []) ~kind x =
    Include_infos.to_parsetree
      { pincl_kind = kind; pincl_mod = x; pincl_loc = loc; pincl_attributes = attrs }
  ;;

  let pexp_function ~loc ~attrs ~params ~constraint_ ~body =
    { pexp_desc = Shim.Pexp_function.to_parsetree ~params ~constraint_ ~body
    ; pexp_loc = loc
    ; pexp_attributes = attrs
    ; pexp_loc_stack = []
    }
  ;;

  let unary_function ~loc ?(attrs = []) cases =
    pexp_function
      ~attrs
      ~params:[]
      ~constraint_:Shim.Pexp_function.Function_constraint.none
      ~body:(Pfunction_cases (cases, loc, []))
      ~loc
  ;;

  let fun_param ~loc arg_label pattern : function_param =
    { pparam_desc = Pparam_val (arg_label, None, pattern); pparam_loc = loc }
  ;;

  let function_constraint type_constraint : function_constraint =
    { Shim.Pexp_function.Function_constraint.none with
      ret_type_constraint =
        Option.map ~f:(fun x -> Pexp_function.Pconstraint x) type_constraint
    }
  ;;

  let maybe_constrain body return_constraint ~loc =
    match return_constraint with
    | None -> body
    | Some return_constraint ->
      Ppxlib_ast.Ast_helper.Exp.constraint_ ~loc body return_constraint
  ;;

  let add_fun_params ~loc ?(attrs = []) ?return_constraint new_params body =
    match new_params with
    | [] -> maybe_constrain body return_constraint ~loc
    | _ :: _ ->
      (* If the body is already a function, extend its arity rather than creating a new
         function.
      *)
      (match Shim.Pexp_function.of_parsetree body.pexp_desc ~loc:body.pexp_loc with
       | Some (params, constraint_, function_body) ->
         let existing_attrs =
           List.filter body.pexp_attributes ~f:(fun attr ->
             (* We drop "merlin.loc" attributes inserted by merlin's parser.

                These attributes are always fine to drop -- they're a best-effort attempt
                to encode extra location information -- and usually not fine to move.
                That's because merlin expects certain invariants to hold between the
                location encoded by the "merlin.loc" and locations of sub-ASTs.
             *)
             String.( <> ) attr.attr_name.txt "merlin.loc")
         in
         let fun_ =
           pexp_function
             ~params:(new_params @ params)
             ~constraint_
             ~body:function_body
             ~loc
             ~attrs:(existing_attrs @ attrs)
         in
         maybe_constrain fun_ return_constraint ~loc
       | None ->
         pexp_function
           ~params:new_params
           ~constraint_:(function_constraint return_constraint)
           ~body:(Pfunction_body body)
           ~loc
           ~attrs)
  ;;

  let add_fun_param ~loc ?attrs ?return_constraint lbl def pat body =
    add_fun_params
      ?attrs
      ?return_constraint
      ~loc
      [ { pparam_desc = Pparam_val (lbl, def, pat); pparam_loc = pat.ppat_loc } ]
      body
  ;;

  let coalesce_fun_arity ast =
    match Shim.Pexp_function.of_parsetree ast.pexp_desc ~loc:ast.pexp_loc with
    | None | Some (_, _, Pfunction_cases _) -> ast
    | Some (params, constraint_, body)
      when not (Shim.Pexp_function.Function_constraint.is_none constraint_) ->
      pexp_function
        ~params
        ~constraint_
        ~body
        ~loc:ast.pexp_loc
        ~attrs:ast.pexp_attributes
    | Some (params1, _, Pfunction_body ({ pexp_attributes = []; _ } as outer_body)) ->
      (match
         Shim.Pexp_function.of_parsetree outer_body.pexp_desc ~loc:outer_body.pexp_loc
       with
       | Some (params2, constraint_, inner_body) ->
         pexp_function
           ~params:(params1 @ params2)
           ~constraint_
           ~body:inner_body
           ~loc:ast.pexp_loc
           ~attrs:ast.pexp_attributes
       | None -> ast)
    | Some _ -> ast
  ;;

  let eabstract ~loc ?(coalesce_fun_arity = true) ?return_constraint pats body =
    let params = List.map pats ~f:(fun pat -> fun_param ~loc:pat.ppat_loc Nolabel pat) in
    if coalesce_fun_arity
    then add_fun_params ~loc ?return_constraint params body
    else
      pexp_function
        ~loc
        ~params
        ~constraint_:(return_constraint |> function_constraint)
        ~body:(Pfunction_body body)
        ~attrs:[]
  ;;

  module Latest = struct
    let ptyp_any ~loc a =
      let ptyp_desc = Shim.Core_type_desc.to_parsetree (Ptyp_any a) in
      { ptyp_loc_stack = []; ptyp_attributes = []; ptyp_loc = loc; ptyp_desc }
    ;;

    let ptyp_var ~loc a b =
      let ptyp_desc = Shim.Core_type_desc.to_parsetree (Ptyp_var (a, b)) in
      { ptyp_loc_stack = []; ptyp_attributes = []; ptyp_loc = loc; ptyp_desc }
    ;;

    let pexp_function ~loc ?(attrs = []) params constraint_ body =
      pexp_function ~loc ~attrs ~params ~constraint_ ~body
    ;;
  end

  let ptyp_unboxed_tuple ~loc a =
    let ptyp_desc = Shim.Core_type_desc.to_parsetree (Ptyp_unboxed_tuple a) in
    { ptyp_loc_stack = []; ptyp_attributes = []; ptyp_loc = loc; ptyp_desc }
  ;;

  let pexp_unboxed_tuple ~loc ?attrs a =
    let pexp_desc = Shim.Expression_desc.to_parsetree ~loc (Pexp_unboxed_tuple a) in
    mkexp ?attrs ~loc pexp_desc
  ;;

  let ppat_unboxed_tuple ~loc ?attrs a closed =
    let ppat_desc =
      Shim.Pattern_desc.to_parsetree ~loc (Ppat_unboxed_tuple (a, closed))
    in
    mkpat ?attrs ~loc ppat_desc
  ;;

  let exp_constant ~loc c =
    let c = Shim.Constant.to_parsetree c in
    let pexp_desc = Shim.Expression_desc.to_parsetree ~loc (Pexp_constant c) in
    mkexp ~loc pexp_desc
  ;;

  let pat_constant ~loc c =
    let c = Shim.Constant.to_parsetree c in
    let ppat_desc = Shim.Pattern_desc.to_parsetree ~loc (Ppat_constant c) in
    mkpat ~loc ppat_desc
  ;;

  let exp_unboxed_int_constant ~loc str suffix =
    exp_constant ~loc (Pconst_unboxed_integer (str, suffix))
  ;;

  let pat_unboxed_int_constant ~loc str suffix =
    pat_constant ~loc (Pconst_unboxed_integer (str, suffix))
  ;;

  let eint64_u ~loc i = exp_unboxed_int_constant ~loc (Int64.to_string i) 'L'
  let eint32_u ~loc i = exp_unboxed_int_constant ~loc (Int32.to_string i) 'l'
  let enativeint_u ~loc i = exp_unboxed_int_constant ~loc (Nativeint.to_string i) 'n'
  let efloat_u ~loc f = exp_constant ~loc (Pconst_unboxed_float (f, None))
  let pint64_u ~loc i = pat_unboxed_int_constant ~loc (Int64.to_string i) 'L'
  let pint32_u ~loc i = pat_unboxed_int_constant ~loc (Int32.to_string i) 'l'
  let pnativeint_u ~loc i = pat_unboxed_int_constant ~loc (Nativeint.to_string i) 'n'
  let pfloat_u ~loc f = pat_constant ~loc (Pconst_unboxed_float (f, None))

  let ptyp_tuple ~loc ?attrs a =
    match a with
    | [] -> raise (Invalid_argument "ptyp_tuple: cannot construct a 0-ary tuple")
    | _ :: _ :: _ ->
      let ptyp_desc = Shim.Core_type_desc.to_parsetree (Ptyp_tuple a) in
      mktyp ?attrs ~loc ptyp_desc
    | [ (name, x) ] ->
      (match name with
       | Some _ ->
         raise (Invalid_argument "ptyp_tuple: cannot construct a 1-ary named tuple")
       | None ->
         (match attrs with
          | None -> x
          | Some attrs ->
            { x with ptyp_loc = loc; ptyp_attributes = x.ptyp_attributes @ attrs }))
  ;;

  let pexp_tuple ~loc ?attrs a =
    match a with
    | [] -> raise (Invalid_argument "pexp_tuple: cannot construct a 0-ary tuple")
    | _ :: _ :: _ ->
      let pexp_desc = Shim.Expression_desc.to_parsetree ~loc (Pexp_tuple a) in
      mkexp ?attrs ~loc pexp_desc
    | [ (name, x) ] ->
      (match name with
       | Some _ ->
         raise (Invalid_argument "pexp_tuple: cannot construct a 1-ary named tuple")
       | None ->
         (match attrs with
          | None -> x
          | Some attrs ->
            { x with pexp_loc = loc; pexp_attributes = x.pexp_attributes @ attrs }))
  ;;

  let ppat_tuple ~loc ?attrs a closed =
    match a, closed with
    | [], _ -> raise (Invalid_argument "ppat_tuple: cannot construct a 0-ary tuple")
    | _ :: _ :: _, _ | [ _ ], Open ->
      let ppat_desc = Shim.Pattern_desc.to_parsetree ~loc (Ppat_tuple (a, closed)) in
      mkpat ?attrs ~loc ppat_desc
    | [ (name, x) ], Closed ->
      (match name with
       | Some _ ->
         raise (Invalid_argument "ppat_tuple: cannot construct a 1-ary named tuple")
       | None ->
         (match attrs with
          | None -> x
          | Some attrs ->
            { x with ppat_loc = loc; ppat_attributes = x.ppat_attributes @ attrs }))
  ;;

  let pexp_record_unboxed_product ~loc ?attrs a b =
    let pexp_desc =
      Shim.Expression_desc.to_parsetree ~loc (Pexp_record_unboxed_product (a, b))
    in
    mkexp ?attrs ~loc pexp_desc
  ;;

  let ppat_record_unboxed_product ~loc ?attrs a b =
    let ppat_desc =
      Shim.Pattern_desc.to_parsetree ~loc (Ppat_record_unboxed_product (a, b))
    in
    mkpat ?attrs ~loc ppat_desc
  ;;

  let pexp_unboxed_field ~loc ?attrs a b =
    let pexp_desc = Shim.Expression_desc.to_parsetree ~loc (Pexp_unboxed_field (a, b)) in
    mkexp ?attrs ~loc pexp_desc
  ;;

  let pexp_idx ~loc ?attrs a b =
    let pexp_desc = Shim.Expression_desc.to_parsetree ~loc (Pexp_idx (a, b)) in
    mkexp ?attrs ~loc pexp_desc
  ;;

  let ptyp_alias ~loc ?(attrs = []) a b c =
    let ptyp_desc = Shim.Core_type_desc.to_parsetree (Ptyp_alias (a, b, c)) in
    { ptyp_loc_stack = []; ptyp_attributes = attrs; ptyp_loc = loc; ptyp_desc }
  ;;

  let ptyp_poly ~loc ?(attrs = []) vars body =
    match vars, attrs with
    | [], [] -> body
    | [], _ ->
      { body with
        ptyp_loc = loc
      ; ptyp_loc_stack = body.ptyp_loc :: body.ptyp_loc_stack
      ; ptyp_attributes = body.ptyp_attributes @ attrs
      }
    | _ ->
      let desc = Shim.Core_type_desc.to_parsetree (Ptyp_poly (vars, body)) in
      mktyp ~attrs ~loc desc
  ;;

  let ptyp_quote ~loc ?attrs core_type =
    let desc = Shim.Core_type_desc.to_parsetree (Ptyp_quote core_type) in
    mktyp ?attrs ~loc desc
  ;;

  let ptyp_splice ~loc ?attrs core_type =
    let desc = Shim.Core_type_desc.to_parsetree (Ptyp_splice core_type) in
    mktyp ?attrs ~loc desc
  ;;

  let pexp_newtype ~loc ?attrs a b c =
    let desc = Shim.Expression_desc.to_parsetree ~loc (Pexp_newtype (a, b, c)) in
    mkexp ?attrs ~loc desc
  ;;

  let ppat_array ~loc ?attrs a b =
    let desc = Shim.Pattern_desc.to_parsetree ~loc (Ppat_array (a, b)) in
    mkpat ?attrs ~loc desc
  ;;

  let pexp_array ~loc ?attrs a b =
    let desc = Shim.Expression_desc.to_parsetree ~loc (Pexp_array (a, b)) in
    mkexp ?attrs ~loc desc
  ;;
end

module Make (Loc : sig
    val loc : Location.t
  end) =
struct
  include Default

  let loc = Loc.loc
  let ptyp_arrow arg res : core_type = ptyp_arrow ~loc arg res
  let tarrow args res : core_type = tarrow ~loc args res
  let tarrow_maybe args res : core_type = tarrow_maybe ~loc args res
  let pexp_let a b c d : expression = pexp_let ~loc a b c d
  let pexp_constraint a b c : expression = pexp_constraint ~loc a b c
  let ppat_constraint a b c : pattern = ppat_constraint ~loc a b c

  let type_declaration
    ?attrs
    ?jkind_annotation
    ~name
    ~params
    ~cstrs
    ~kind
    ~private_
    ~manifest
    ()
    =
    type_declaration
      ~loc
      ?attrs
      ?jkind_annotation
      ~name
      ~params
      ~cstrs
      ~kind
      ~private_
      ~manifest
      ()
  ;;

  let value_binding ~pat ~expr ~modes : value_binding =
    value_binding ~loc ~pat ~expr ~modes
  ;;

  let pcstr_tuple fields : constructor_arguments = pcstr_tuple ~loc fields

  let label_declaration ~name ~mutable_ ~modalities ~type_ : label_declaration =
    label_declaration ~loc ~name ~mutable_ ~modalities ~type_
  ;;

  let value_description ~name ~type_ ~modalities ~prim : value_description =
    value_description ~loc ~name ~type_ ~modalities ~prim
  ;;

  let pcstr_tuple_arg ~modalities ~type_ : Pcstr_tuple_arg.t =
    pcstr_tuple_arg ~loc ~modalities ~type_
  ;;

  let include_infos ?attrs ~kind x = include_infos ~loc ?attrs ~kind x
  let psig_include ~modalities a : signature_item = psig_include ~loc ~modalities a
  let signature ?modalities a : signature = signature ~loc ?modalities a

  let module_declaration ?attrs ?modalities name type_ =
    module_declaration ~loc ?attrs ?modalities name type_
  ;;

  let pmty_functor ?attrs ?modes param mty = pmty_functor ~loc ?attrs ?modes param mty
  let pmod_constraint ?attrs expr mty modes = pmod_constraint ~loc ?attrs expr mty modes

  module Latest = struct
    let ptyp_any a : core_type = Latest.ptyp_any ~loc a
    let ptyp_var a b : core_type = Latest.ptyp_var ~loc a b
    let pexp_function ?attrs a b c : expression = Latest.pexp_function ~loc ?attrs a b c
  end

  let eabstract ?coalesce_fun_arity ?return_constraint a b : expression =
    eabstract ~loc ?coalesce_fun_arity ?return_constraint a b
  ;;

  let fun_param a b : function_param = fun_param ~loc a b
  let unary_function ?attrs a : expression = unary_function ~loc ?attrs a

  let add_fun_param ?attrs ?return_constraint a b c d : expression =
    add_fun_param ~loc ?attrs ?return_constraint a b c d
  ;;

  let add_fun_params ?attrs ?return_constraint a b : expression =
    add_fun_params ~loc ?attrs ?return_constraint a b
  ;;

  let ptyp_unboxed_tuple a : core_type = ptyp_unboxed_tuple ~loc a
  let pexp_unboxed_tuple ?attrs a : expression = pexp_unboxed_tuple ~loc ?attrs a

  let pexp_record_unboxed_product ?attrs a b : expression =
    pexp_record_unboxed_product ~loc ?attrs a b
  ;;

  let ppat_record_unboxed_product ?attrs a b : pattern =
    ppat_record_unboxed_product ~loc ?attrs a b
  ;;

  let pexp_unboxed_field ?attrs a b : expression = pexp_unboxed_field ~loc ?attrs a b
  let pexp_idx ?attrs a b : expression = pexp_idx ~loc ?attrs a b
  let ppat_unboxed_tuple ?attrs a b : pattern = ppat_unboxed_tuple ~loc ?attrs a b
  let eint64_u c : expression = eint64_u ~loc c
  let eint32_u c : expression = eint32_u ~loc c
  let enativeint_u c : expression = enativeint_u ~loc c
  let efloat_u c : expression = efloat_u ~loc c
  let pint64_u c : pattern = pint64_u ~loc c
  let pint32_u c : pattern = pint32_u ~loc c
  let pnativeint_u c : pattern = pnativeint_u ~loc c
  let pfloat_u c : pattern = pfloat_u ~loc c
  let ptyp_tuple ?attrs a : core_type = ptyp_tuple ~loc ?attrs a
  let ptyp_quote ?attrs a : core_type = ptyp_quote ~loc ?attrs a
  let ptyp_splice ?attrs a : core_type = ptyp_splice ~loc ?attrs a
  let pexp_tuple ?attrs a : expression = pexp_tuple ~loc ?attrs a
  let ppat_tuple ?attrs a b : pattern = ppat_tuple ~loc ?attrs a b
  let pmty_signature a : module_type = pmty_signature ~loc a
  let ptyp_poly ?attrs a b : core_type = ptyp_poly ~loc ?attrs a b
  let ptyp_alias ?attrs a b c : core_type = ptyp_alias ~loc ?attrs a b c
  let pexp_newtype ?attrs a b c : expression = pexp_newtype ~loc ?attrs a b c
  let pexp_array ?attrs a b : expression = pexp_array ~loc ?attrs a b
  let ppat_array ?attrs a b : pattern = ppat_array ~loc ?attrs a b
end

let make loc : (module S_with_implicit_loc) =
  (module Make (struct
      let loc = loc
    end))
;;
