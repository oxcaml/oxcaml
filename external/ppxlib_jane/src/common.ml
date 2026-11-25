open Ppxlib_ast.Asttypes
open Ppxlib_ast.Parsetree

let not_a_type_parameter ~loc =
  Astlib.Location.Error.make { loc; txt = "not a named type parameter" } ~sub:[]
;;

let get_type_param_name_and_jkind_res ty =
  let loc = ty.ptyp_loc in
  match Shim.Core_type_desc.of_parsetree ty.ptyp_desc with
  | Ptyp_var (name, jkind) -> Ok ({ loc; txt = name }, jkind)
  | _ -> Error (not_a_type_parameter ~loc)
;;

let get_type_param_name_and_jkind_of_core_type t =
  match get_type_param_name_and_jkind_res t with
  | Ok e -> e
  | Error err -> Ppxlib_ast.Location_error.raise err
;;

let get_type_param_name_and_jkind (t, _) = get_type_param_name_and_jkind_of_core_type t

let as_unlabeled_tuple components =
  if List.for_all (fun (label, _) -> Option.is_none label) components
  then Some (List.map snd components)
  else None
;;

let mangle_longident ~suffix : Astlib.Longident.t -> Astlib.Longident.t = function
  | Lident name -> Lident (name ^ suffix)
  | Ldot (path, name) -> Ldot (path, name ^ suffix)
  | Lapply _ as longident -> longident
;;

let localize_longident = mangle_longident ~suffix:"__local"
let stackify_longident = mangle_longident ~suffix:"__stack"

let mangle_include_sig incl ~f =
  { incl with
    pincl_mod =
      { incl.pincl_mod with
        pmty_desc =
          (match incl.pincl_mod.pmty_desc with
           | Pmty_ident { txt; loc } -> Pmty_ident { txt = f txt; loc }
           | Pmty_with (({ pmty_desc = Pmty_ident { txt; loc }; _ } as mty), cstrs) ->
             Pmty_with ({ mty with pmty_desc = Pmty_ident { txt = f txt; loc } }, cstrs)
           | _ -> failwith "expected [include S] or [include S with ...]")
      }
  }
;;

let localize_include_sig incl = mangle_include_sig incl ~f:localize_longident
let stackify_include_sig incl = mangle_include_sig incl ~f:stackify_longident

let append_arbitrary_suffix_to_include_signature incl ~suffix =
  mangle_include_sig incl ~f:(mangle_longident ~suffix)
;;

let type_declarations_contain_jkind_annotation td =
  let fold =
    object
      inherit [bool] Ast_traverse.fold
      method! jkind_annotation _ _ = true
    end
  in
  fold#list fold#type_declaration td false
;;

let disable_imprecise_kind_annotation_attribute ~loc : attribute =
  let payload_expression =
    let pexp_desc =
      Pexp_constant (Pconst_string ("-imprecise-kind-annotation", loc, None))
    in
    { pexp_desc; pexp_loc = loc; pexp_loc_stack = []; pexp_attributes = [] }
  in
  { attr_name = { txt = "warning"; loc }
  ; attr_payload =
      PStr [ { pstr_desc = Pstr_eval (payload_expression, []); pstr_loc = loc } ]
  ; attr_loc = loc
  }
;;

let suppress_imprecise_kind_annotation_warning_in_structure ~loc tds items =
  match items with
  | [] -> []
  | _ :: _ ->
    if type_declarations_contain_jkind_annotation tds
    then (
      let disable_warning =
        { pstr_desc = Pstr_attribute (disable_imprecise_kind_annotation_attribute ~loc)
        ; pstr_loc = loc
        }
      in
      let module_expr =
        { pmod_desc = Pmod_structure (disable_warning :: items)
        ; pmod_loc = loc
        ; pmod_attributes = []
        }
      in
      [ { pstr_desc =
            Pstr_include
              (Ast_builder.Default.include_infos ~loc ~kind:Structure module_expr)
        ; pstr_loc = loc
        }
      ])
    else items
;;

let suppress_imprecise_kind_annotation_warning_in_signature ~loc tds items =
  match items with
  | [] -> []
  | _ :: _ ->
    if type_declarations_contain_jkind_annotation tds
    then (
      let disable_warning =
        { psig_desc = Psig_attribute (disable_imprecise_kind_annotation_attribute ~loc)
        ; psig_loc = loc
        }
      in
      [ Ast_builder.Default.psig_include
          ~loc
          ~modalities:[]
          (Ast_builder.Default.include_infos
             ~loc
             ~kind:Structure
             (Ast_builder.Default.pmty_signature
                ~loc
                (Ast_builder.Default.signature ~loc (disable_warning :: items))))
      ])
    else items
;;
