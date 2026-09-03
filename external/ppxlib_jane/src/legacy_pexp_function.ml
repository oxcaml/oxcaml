open! Astlib
open! Ppxlib_ast.Asttypes
open! Ppxlib_ast.Parsetree

type legacy_pexp_fun = arg_label * expression option * pattern * expression
type legacy_pexp_function = case list
type legacy_pexp_newtype = string loc * expression

type t =
  | Legacy_pexp_fun of legacy_pexp_fun
  (** Match [Pexp_fun], as in the OCaml parsetree prior to 5.2. To construct, use
      [Ppxlib.Ast_builder.Default.pexp_fun]. *)
  | Legacy_pexp_function of legacy_pexp_function
  (** Match [Pexp_function], as in the OCaml parsetree prior to 5.2. To construct, use
      [Ppxlib.Ast_builder.Default.pexp_function]. *)
  | Legacy_pexp_newtype of legacy_pexp_newtype

let of_pexp_function
  ~(params : Shim.Pexp_function.function_param list)
  ~(constraint_ : Shim.Pexp_function.Function_constraint.t)
  ~(body : Shim.Pexp_function.function_body)
  =
  match params, body with
  | [], Pfunction_cases (cases, _, _) -> Legacy_pexp_function cases
  | { pparam_desc = first_param; pparam_loc } :: params, _ ->
    let body =
      match params, body with
      (* The remaining body is still a valid function: we either have more parameters in
         the list, or the body is function cases (which implies a parameter).
      *)
      | _ :: _, _ | [], Pfunction_cases _ ->
        let body_loc =
          match body with
          | Pfunction_body body -> body.pexp_loc
          | Pfunction_cases (_, body_loc, _) -> body_loc
        in
        let rest_loc : Location.t =
          { loc_start = pparam_loc.loc_end; loc_end = body_loc.loc_end; loc_ghost = true }
        in
        let rest_fun = Shim.Pexp_function.to_parsetree ~params ~constraint_ ~body in
        Ppxlib_ast.Ast_helper.Exp.mk rest_fun ~loc:rest_loc
      (* The remaining body is not a valid function, and we just return it directly. *)
      | [], Pfunction_body expr ->
        (match constraint_ with
         | c when Shim.Pexp_function.Function_constraint.is_none c -> expr
         | ({ ret_type_constraint; ret_mode_annotations; mode_annotations = _ } :
             Shim.Pexp_function.Function_constraint.t) ->
           let loc = { expr.pexp_loc with loc_ghost = true } in
           let pexp_desc =
             match ret_type_constraint with
             | Some (Pcoerce (ty1, ty2)) -> Pexp_coerce (expr, ty1, ty2)
             | Some (Pconstraint ty) ->
               Pexp_constraint (expr, Some ty, ret_mode_annotations)
               |> Shim.Expression_desc.to_parsetree ~loc
             | None ->
               Pexp_constraint (expr, None, ret_mode_annotations)
               |> Shim.Expression_desc.to_parsetree ~loc
           in
           Ppxlib_ast.Ast_helper.Exp.mk pexp_desc ~loc)
    in
    (match first_param with
     | Pparam_val (lbl, opt, pat) -> Legacy_pexp_fun (lbl, opt, pat, body)
     | Pparam_newtype (newtype, _jkind) -> Legacy_pexp_newtype (newtype, body))
  | _, Pfunction_body body ->
    Location.raise_errorf "empty function body" ~loc:body.pexp_loc
;;

let of_parsetree expression =
  Option.map
    (fun (params, constraint_, body) -> of_pexp_function ~params ~constraint_ ~body)
    (Shim.Pexp_function.of_parsetree
       expression
       (* This location is just filled in as the [Pfunction_cases] location, and
          [of_pexp_function] drops that location.
       *)
       ~loc:Location.none)
;;

let legacy_pexp_fun_of_parsetree expression =
  match of_parsetree expression with
  | Some (Legacy_pexp_fun x) -> Some x
  | _ -> None
;;

let legacy_pexp_function_of_parsetree expression =
  match of_parsetree expression with
  | Some (Legacy_pexp_function x) -> Some x
  | _ -> None
;;

let legacy_pexp_newtype_of_parsetree expression =
  match of_parsetree expression with
  | Some (Legacy_pexp_newtype x) -> Some x
  | _ -> None
;;
