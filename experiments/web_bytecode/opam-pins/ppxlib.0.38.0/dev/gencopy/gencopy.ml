open Ppxlib

module Ast_builder = Ast_builder.Make (struct
  let loc = Location.none
end)

let loc txt = Ast_builder.Located.mk txt
let ident_loc s = loc (Longident.parse s)

module Ast_io = Ppxlib__.Utils.Ast_io

module Copy = struct
  type context = {
    from : string; (* Module name of the origin AST *)
    to_ : string; (* Module name of the destination AST *)
    inside : string; (* Sub-module in Ast_x, e.g. Parsetree or Asttypes *)
  }

  let mk_ident ~root ~sub name =
    ident_loc (Printf.sprintf "%s.%s.%s" root sub name)

  let from_ident ~context name =
    mk_ident ~root:context.from ~sub:context.inside name

  let to_ident ~context name =
    mk_ident ~root:context.to_ ~sub:context.inside name

  let from_var_name i = Printf.sprintf "f%d" i
  let to_var_name i = Printf.sprintf "g%d" i
  let from_var i = Ast_builder.ptyp_var (from_var_name i)
  let to_var i = Ast_builder.ptyp_var (to_var_name i)

  let from_type ~context { ptype_name; ptype_params; _ } =
    let longident = from_ident ~context ptype_name.txt in
    let params = List.mapi (fun i _ -> from_var i) ptype_params in
    Ast_builder.ptyp_constr longident params

  let to_type ~context { ptype_name; ptype_params; _ } =
    let longident = to_ident ~context ptype_name.txt in
    let params = List.mapi (fun i _ -> to_var i) ptype_params in
    Ast_builder.ptyp_constr longident params

  let fun_name ~context ptype_name =
    let name =
      match ptype_name with
      | "t" -> String.lowercase_ascii context.inside
      | _ -> ptype_name
    in
    Printf.sprintf "copy_%s" name

  let arrow_type args ret =
    List.fold_right (Ast_builder.ptyp_arrow Nolabel) args ret

  let arg_type_from_param i =
    let arg = from_var i in
    let ret = to_var i in
    Ast_builder.ptyp_arrow Nolabel arg ret

  let existentials_from_params l =
    List.mapi (fun i _ -> [ loc (from_var_name i); loc (to_var_name i) ]) l
    |> List.flatten

  let fun_type ~context decl =
    (* For [type ('a, 'b) x] we generate the type:
       ['f0 'g0 'f1 'g1. ('f0 -> 'g0) -> ('f1 -> 'g1) ->
         ('f0, 'f1) Ast_From.Submodule.x ->
         ('g0, 'g1) Ast_to.Submodule.x] *)
    let extra_params =
      List.mapi (fun i _ -> arg_type_from_param i) decl.ptype_params
    in
    let from_type = from_type ~context decl in
    let to_type = to_type ~context decl in
    let arrow = arrow_type (extra_params @ [ from_type ]) to_type in
    match extra_params with
    | [] -> arrow
    | _ ->
        Ast_builder.ptyp_poly (existentials_from_params decl.ptype_params) arrow

  let mk_extra_arg i =
    let var = Ast_builder.ppat_var (loc (from_var_name i)) in
    Ast_builder.pparam_val Nolabel None var

  let mk_evar name = Ast_builder.pexp_ident (loc (Lident name))
  let mk_pvar name = Ast_builder.ppat_var (loc name)
  let tuple_var_name i = Printf.sprintf "x%d" i
  let etuple_var i = mk_evar (tuple_var_name i)
  let ptuple_var i = mk_pvar (tuple_var_name i)
  let extra_args decl = List.mapi (fun i _ -> mk_extra_arg i) decl.ptype_params

  let ptyp_desc_name typ =
    match typ.ptyp_desc with
    | Ptyp_any -> "Ptyp_any"
    | Ptyp_var _ -> "Ptyp_var"
    | Ptyp_arrow _ -> "Ptyp_arrow"
    | Ptyp_tuple _ -> "Ptyp_tuple"
    | Ptyp_constr _ -> "Ptyp_constr"
    | Ptyp_object _ -> "Ptyp_object"
    | Ptyp_class _ -> "Ptyp_class"
    | Ptyp_alias _ -> "Ptyp_alias"
    | Ptyp_variant _ -> "Ptyp_variant"
    | Ptyp_poly _ -> "Ptyp_poly"
    | Ptyp_package _ -> "Ptyp_package"
    | Ptyp_open _ -> "Ptyp_open"
    | Ptyp_extension _ -> "Ptyp_extension"

  let tuple_pat typs =
    let vars = List.mapi (fun i _ -> ptuple_var i) typs in
    Ast_builder.ppat_tuple vars

  let param_index ~decl var_name =
    let rec find_index i p l =
      match l with
      | [] ->
          failwith
            (Printf.sprintf "Could not find var '%s in %s" var_name
               decl.ptype_name.txt)
      | hd :: _ when p hd -> i
      | _ :: tl -> find_index (i + 1) p tl
    in
    let is_var (typ, _) =
      match typ.ptyp_desc with
      | Ptyp_var name -> String.equal name var_name
      | _ -> false
    in
    find_index 0 is_var decl.ptype_params

  let copy_var ~decl var_name =
    let index = param_index ~decl var_name in
    let name = from_var_name index in
    Ast_builder.pexp_ident (loc (Lident name))

  let copy_t modname =
    Printf.sprintf "copy_%s" (String.uncapitalize_ascii modname)

  let copy_ident ~context ident =
    let mk_ident s = Ast_builder.pexp_ident (loc (Lident s)) in
    let loc = Location.none in
    match ident with
    | Lident "t" -> mk_ident (copy_t context.inside)
    | Ldot (Lident s, "t") -> mk_ident (copy_t s)
    | Lident
        ("string" | "bool" | "char" | "int" | "int32" | "int64" | "nativeint")
      ->
        [%expr fun x -> x]
    | Lident "list" -> [%expr List.map]
    | Lident "option" -> [%expr Option.map]
    | Ldot (_, s) | Lident s -> mk_ident (Printf.sprintf "copy_%s" s)
    | Lapply _ ->
        invalid_arg (Printf.sprintf "copy_ident: %s" (Longident.name ident))

  let rec copy_expr ~context ~decl ~var typ =
    match typ.ptyp_desc with
    | Ptyp_tuple typs ->
        let pat = tuple_pat typs in
        let vb = Ast_builder.value_binding ~pat ~expr:var in
        let expr =
          let elm_expr i typ =
            copy_expr ~context ~decl ~var:(etuple_var i) typ
          in
          Ast_builder.pexp_tuple (List.mapi elm_expr typs)
        in
        Ast_builder.pexp_let Nonrecursive [ vb ] expr
    | Ptyp_var _ ->
        let copy_fun = copy_fun ~context ~decl typ in
        Ast_builder.eapply copy_fun [ var ]
    | Ptyp_constr
        ( {
            txt =
              Lident
                ( "string" | "bool" | "char" | "int" | "int32" | "int64"
                | "nativeint" );
            _;
          },
          [] ) ->
        var
    | Ptyp_constr (_ident, _params) ->
        let fun_ = copy_fun ~context ~decl typ in
        Ast_builder.eapply fun_ [ var ]
    | _ ->
        failwith
          (Printf.sprintf "Don't know how to copy %s" (ptyp_desc_name typ))

  (* Produces an expression that can be applied to a variable of type [typ] to
     copy it, i.e. something in the form of [copy_x], [f0], [List.map copy_loc] (partial application) or
     [fun x -> (*code to copy x*)] *)
  and copy_fun ~context ~decl typ =
    match typ.ptyp_desc with
    | Ptyp_var name -> copy_var ~decl name
    | Ptyp_constr (ident, []) -> copy_ident ~context ident.txt
    | Ptyp_constr (ident, params) ->
        let param_funs = List.map (copy_fun ~context ~decl) params in
        let main_fun = copy_ident ~context ident.txt in
        Ast_builder.eapply main_fun param_funs
    | _ ->
        let arg = Ast_builder.pparam_val Nolabel None (mk_pvar "x") in
        let var = mk_evar "x" in
        let body = Pfunction_body (copy_expr ~context ~decl ~var typ) in
        Ast_builder.pexp_function [ arg ] None body

  let record_pattern ~mk_ident ~context labels =
    let mk_field { pld_name; _ } =
      let ident = mk_ident ~context pld_name.txt in
      let pattern = Ast_builder.ppat_var pld_name in
      (ident, pattern)
    in
    let fields = List.map mk_field labels in
    Ast_builder.ppat_record fields Closed

  let copy_record ?(qualified_fields = true) ~context ~decl labels =
    let mk_field { pld_name; pld_type; _ } =
      let ident =
        if qualified_fields then to_ident ~context pld_name.txt
        else loc (Lident pld_name.txt)
      in
      let var = mk_evar pld_name.txt in
      let expr = copy_expr ~context ~decl ~var pld_type in
      (ident, expr)
    in
    let fields = List.map mk_field labels in
    Ast_builder.pexp_record fields None

  let record_fun_expr ~context ~decl labels =
    let extra_args = extra_args decl in
    let main_arg =
      let pat = record_pattern ~mk_ident:from_ident ~context labels in
      Ast_builder.pparam_val Nolabel None pat
    in
    let body = Pfunction_body (copy_record ~context ~decl labels) in
    Ast_builder.pexp_function (extra_args @ [ main_arg ]) None body

  let ctor_pattern ~context { pcd_name; pcd_args; _ } =
    let ident = from_ident ~context pcd_name.txt in
    let arg_pattern =
      match pcd_args with
      | Pcstr_tuple [] -> None
      | Pcstr_tuple args -> Some (tuple_pat args)
      | Pcstr_record labels ->
          let mk_ident ~context:_ fname = loc (Lident fname) in
          Some (record_pattern ~mk_ident ~context labels)
    in
    Ast_builder.ppat_construct ident arg_pattern

  let copy_ctor ~context ~decl { pcd_name; pcd_args; _ } =
    let args =
      match pcd_args with
      | Pcstr_tuple [] -> None
      | Pcstr_tuple args ->
          let exprs =
            List.mapi
              (fun i typ ->
                let var = etuple_var i in
                copy_expr ~context ~decl ~var typ)
              args
          in
          Some (Ast_builder.pexp_tuple exprs)
      | Pcstr_record labels ->
          (* For inline records we can't use qualified names, this would
           cause compiler errors, hence [~qualified_fields:false]. *)
          Some (copy_record ~qualified_fields:false ~context ~decl labels)
    in
    let ident = to_ident ~context pcd_name.txt in
    Ast_builder.pexp_construct ident args

  let variant_fun_expr ~context ~decl ctors =
    let extra_args = extra_args decl in
    let body =
      let mk_ctor_case ctor =
        let lhs = ctor_pattern ~context ctor in
        let rhs = copy_ctor ~context ~decl ctor in
        Ast_builder.case ~guard:None ~lhs ~rhs
      in
      let cases = List.map mk_ctor_case ctors in
      Pfunction_cases (cases, Location.none, [])
    in
    Ast_builder.pexp_function extra_args None body

  (* generates [(fun x -> (*code to copy x*)] *)
  let alias_fun_expr ~context ~decl typ =
    let extra_args = extra_args decl in
    let main_arg = Ast_builder.pparam_val Nolabel None (mk_pvar "x") in
    let var = mk_evar "x" in
    let body = Pfunction_body (copy_expr ~context ~decl ~var typ) in
    Ast_builder.pexp_function (extra_args @ [ main_arg ]) None body

  let fun_expr ~context decl =
    match (decl.ptype_kind, decl.ptype_manifest) with
    | Ptype_variant ctors, _ -> variant_fun_expr ~context ~decl ctors
    | Ptype_record labels, _ -> record_fun_expr ~context ~decl labels
    | Ptype_abstract, Some typ -> alias_fun_expr ~context ~decl typ
    | _ -> assert false (* No open types or fully abstract types in the AST *)

  let from_ty_decl ~context decl =
    let fun_name = fun_name ~context decl.ptype_name.txt in
    let fun_name_pattern = Ast_builder.(ppat_var (Located.mk fun_name)) in
    let constraint_ =
      Pvc_constraint
        { typ = fun_type ~context decl; locally_abstract_univars = [] }
    in
    let fun_expr = fun_expr ~context decl in
    Ast_builder.Latest.value_binding ~pat:fun_name_pattern ~constraint_
      ~expr:fun_expr ()
end

let open_stdlib0 =
  let open Ast_builder in
  pstr_open
    (open_infos ~expr:(pmod_ident (ident_loc "Stdlib0")) ~override:Fresh)

let type_decls str =
  List.filter_map
    (fun stri ->
      match stri.pstr_desc with Pstr_type (_, tds) -> Some tds | _ -> None)
    str
  |> List.flatten

let default_copy_location =
  let loc = Location.none in
  [%stri let copy_location = fun x -> x]

let gen_copy ~from ~to_ ast =
  let modules =
    List.filter_map
      (fun stri ->
        match stri.pstr_desc with
        | Pstr_module
            {
              pmb_name = { txt = Some name; _ };
              pmb_expr = { pmod_desc = Pmod_structure str; _ };
              _;
            } ->
            Some (name, type_decls str)
        | _ -> None)
      ast
  in
  let vbs =
    List.map
      (fun (inside, tds) ->
        let context = { Copy.from; to_; inside } in
        List.map (Copy.from_ty_decl ~context) tds)
      modules
  in
  [
    open_stdlib0;
    default_copy_location;
    Ast_builder.pstr_value Recursive (List.flatten vbs);
  ]

let mod_name filename =
  let fn = Filename.basename filename in
  match String.split_on_char '.' fn with
  | name :: _ext :: _ -> String.capitalize_ascii name
  | _ -> invalid_arg ("mod_name: " ^ filename)

let parse_ast fn =
  let input_kind = Ast_io.Possibly_source (Impl, fn) in
  match Ppxlib__.Utils.Ast_io.read ~input_kind (File fn) with
  | Error _ ->
      Printf.eprintf "Could not parse %s" fn;
      exit 1
  | Ok { input_name = _; input_version = _; ast } -> (
      match ast with Impl str -> str | Intf _ -> assert false)

let print_copy ~from ~to_ =
  let ast = parse_ast from in
  let from = mod_name from in
  let to_ = mod_name to_ in
  let copy_mod = gen_copy ~from ~to_ ast in
  Format.printf "%a\n" Pprintast.structure copy_mod

let () =
  match Sys.argv with
  | [| _; left; right |] -> print_copy ~from:left ~to_:right
  | _ ->
      Printf.eprintf "Invalid usage, should be: %s ast_FROM.ml ast_TO.ml\n"
        Sys.argv.(0);
      exit 1
