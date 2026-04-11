open Import
open Ast_helper
open Printf
module Section_map = String.Map

let section_map_of_assoc items =
  List.fold_left
    ~f:(fun acc (name, v) ->
      match Section_map.find_opt name acc with
      | None -> Section_map.add name [ v ] acc
      | Some vs -> Section_map.add name (v :: vs) acc)
    ~init:Section_map.empty items

let doc_comment_from_attribue (attr : attribute) =
  match attr.attr_name.txt with
  | "ocaml.doc" -> (
      match attr.attr_payload with
      | PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _);
              _;
            };
          ] ->
          Some s
      | _ -> None)
  | _ -> None

let doc_comment ~node_name ~function_name attributes =
  let parsetree_comment =
    List.find_map ~f:doc_comment_from_attribue attributes
  in
  let pp_parsetree_comment ppf = function
    | None -> ()
    | Some pc -> Format.fprintf ppf "{b Example OCaml}\n\n%s" pc
  in
  Format.asprintf "[%s] constructs an {! Ast.%s}\n\n%a" function_name node_name
    pp_parsetree_comment parsetree_comment

let prefix_of_record lds =
  common_prefix (List.map lds ~f:(fun ld -> ld.pld_name.txt))

module Gen (Fixed_loc : sig
  val fixed_loc : bool
end) =
struct
  open Fixed_loc

  let core_type_of_return_type (typ : type_declaration) =
    let typ_name = typ.ptype_name.txt in
    let typ_name =
      match List.rev (String.split_on_char ~sep:'_' typ_name) with
      | "desc" :: _ ->
          String.sub ~pos:0 ~len:(String.length typ_name - 5) typ_name
      | _ -> typ_name
    in
    match typ.ptype_params with
    | [] -> M.ctyp "%s" typ_name
    | params ->
        let params =
          List.map params ~f:(fun (ctyp, _) -> Format.asprintf "%a" A.ctyp ctyp)
        in
        M.ctyp "(%s) %s" (String.concat ~sep:", " params) typ_name

  let gen_combinator_for_constructor
      ~wrapper:(wpath, wprefix, has_attrs, has_loc_stack) path ~prefix
      return_type cd =
    match cd.pcd_args with
    | Pcstr_record _ ->
        (* TODO. *)
        failwith "Pcstr_record not supported"
    | Pcstr_tuple cd_args ->
        let args = List.mapi cd_args ~f:(fun i _ -> sprintf "x%d" i) in
        let exp =
          Exp.construct
            (Loc.mk (fqn_longident path cd.pcd_name.txt))
            (match args with
            | [] -> None
            | [ x ] -> Some (evar x)
            | _ -> Some (Exp.tuple (List.map args ~f:evar)))
        in
        let body =
          let fields =
            [
              (Loc.mk (fqn_longident' wpath (wprefix ^ "loc")), evar "loc");
              (Loc.mk (fqn_longident' wpath (wprefix ^ "desc")), exp);
            ]
          in
          let fields =
            if has_attrs then
              ( Loc.mk (fqn_longident' wpath (wprefix ^ "attributes")),
                M.expr "[]" )
              :: fields
            else fields
          in
          let fields =
            if has_loc_stack then
              ( Loc.mk (fqn_longident' wpath (wprefix ^ "loc_stack")),
                M.expr "[]" )
              :: fields
            else fields
          in
          Exp.record fields None
        in
        let body =
          (* match args with
             | [] -> [%expr fun () -> [%e body]]
             | _ ->*)
          List.fold_right args ~init:body ~f:(fun arg acc ->
              M.expr "fun %a -> %a" A.patt (pvar arg) A.expr acc)
        in
        (* let body =
             if not has_attrs then
               body
             else
               [%expr fun ?(attrs=[]) -> [%e body]]
           in*)
        let body =
          if fixed_loc then body else M.expr "fun ~loc -> %a" A.expr body
        in
        let function_name = function_name_of_id ~prefix cd.pcd_name.txt in
        let pvar_function_name = pvar function_name in
        let str = M.stri "let %a = %a" A.patt pvar_function_name A.expr body in
        let return_type = core_type_of_return_type return_type in
        let typ =
          List.fold_right cd_args ~init:return_type ~f:(fun cty acc ->
              M.ctyp "%a -> %a" A.ctyp cty A.ctyp acc)
        in
        let typ =
          if fixed_loc then typ else M.ctyp "loc:Location.t -> %a" A.ctyp typ
        in
        let sign =
          M.sigi "val %a : %a (** %s *)" A.patt pvar_function_name A.ctyp typ
            (doc_comment ~function_name ~node_name:cd.pcd_name.txt
               cd.pcd_attributes)
        in
        (str, (Format.asprintf "%a" A.ctyp return_type, sign))

  let gen_combinator_for_record path ~prefix return_type lds =
    let fields =
      List.map lds ~f:(fun ld -> fqn_longident path ld.pld_name.txt)
    in
    let funcs =
      List.map lds ~f:(fun ld ->
          (ld.pld_type, map_keyword (without_prefix ~prefix ld.pld_name.txt)))
    in
    let body =
      Exp.record
        (List.map2 fields funcs ~f:(fun field (_, func) ->
             ( Loc.mk field,
               if func = "attributes" then M.expr "[]" else evar func )))
        None
    in
    let body =
      let l =
        List.filter funcs ~f:(fun (_, f) -> f <> "loc" && f <> "attributes")
      in
      match l with
      | [ (_, x) ] -> Exp.fun_ Nolabel None (pvar x) body
      | _ ->
          List.fold_right l ~init:body ~f:(fun (_, func) acc ->
              Exp.fun_ (Labelled func) None (pvar func) acc)
    in
    (* let body =
         if List.mem "attributes" ~set:funcs then
           [%expr fun ?(attrs=[]) -> [%e body]]
         else
           body
       in*)
    let has_loc_field =
      List.exists ~f:(function _, "loc" -> true | _ -> false) funcs
    in
    let body =
      if has_loc_field && not fixed_loc then M.expr "fun ~loc -> %a" A.expr body
      else body
    in
    let return_ctyp = core_type_of_return_type return_type in
    let typ =
      let l =
        List.filter funcs ~f:(fun (_, f) -> f <> "loc" && f <> "attributes")
      in
      match l with
      | [ (c, _) ] -> M.ctyp "%a -> %a" A.ctyp c A.ctyp return_ctyp
      | _ ->
          List.fold_right l ~init:return_ctyp ~f:(fun (typ, func) acc ->
              M.ctyp "%s:%a -> %a" func A.ctyp typ A.ctyp acc)
    in
    let typ =
      if has_loc_field && not fixed_loc then
        M.ctyp "loc:Location.t -> %a" A.ctyp typ
      else typ
    in
    let pvar_function_name = pvar (function_name_of_path path) in
    let str = M.stri "let %a = %a" A.patt pvar_function_name A.expr body in
    let sign =
      M.sigi "val %a : %a (** %s *)" A.patt pvar_function_name A.ctyp typ
        (doc_comment
           ~function_name:(function_name_of_path path)
           ~node_name:(Format.asprintf "%a" A.ctyp return_ctyp)
           return_type.ptype_attributes)
    in
    (str, (Format.asprintf "%a" A.ctyp return_ctyp, sign))

  let gen_td ?wrapper path td =
    if is_loc path then []
    else
      match td.ptype_kind with
      | Ptype_variant cds -> (
          match wrapper with
          | None -> []
          | Some wrapper ->
              let prefix =
                common_prefix (List.map cds ~f:(fun cd -> cd.pcd_name.txt))
              in
              List.map cds
                ~f:(gen_combinator_for_constructor ~wrapper path ~prefix td))
      | Ptype_record lds ->
          let prefix = prefix_of_record lds in
          [ gen_combinator_for_record path ~prefix td lds ]
      | Ptype_abstract | Ptype_open -> []
end

let filter_labels ~prefix lds =
  List.filter lds ~f:(fun ld ->
      match without_prefix ~prefix ld.pld_name.txt with
      | "loc" | "loc_stack" | "attributes" -> false
      | _ -> true)

let is_abstract td =
  match td.ptype_kind with Ptype_abstract -> true | _ -> false

let dump fn ~ext printer x =
  let oc = open_out (fn ^ ext) in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%a@." printer x;
  close_out oc

let floating_comment s =
  let doc =
    PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc = Pexp_constant (Pconst_string (s, loc, None));
                  pexp_loc = loc;
                  pexp_loc_stack = [];
                  pexp_attributes = [];
                },
                [] );
          pstr_loc = loc;
        };
      ]
  in
  Sig.attribute (Attr.mk { txt = "ocaml.text"; loc } doc)

let generate filename =
  (*  let fn = Misc.find_in_path_uncap !Config.load_path (unit ^ ".cmi") in*)
  let types = get_types ~filename in
  let types_with_wrapped =
    List.map types ~f:(fun (path, td) ->
        match td.ptype_kind with
        | Ptype_record lds -> (
            let prefix = prefix_of_record lds in
            let lds' = filter_labels ~prefix lds in
            match is_wrapper ~prefix lds' with
            | None -> (path, td, None)
            | Some p ->
                let has_attrs =
                  List.exists lds ~f:(fun ld ->
                      ld.pld_name.txt = prefix ^ "attributes")
                in
                let has_loc_stack =
                  List.exists lds ~f:(fun ld ->
                      ld.pld_name.txt = prefix ^ "loc_stack")
                in
                (path, td, Some (prefix, has_attrs, has_loc_stack, p.txt)))
        | _ -> (path, td, None))
  in
  let wrapped =
    List.filter_map types_with_wrapped ~f:(fun (_, _, x) ->
        match x with None -> None | Some (_, _, _, p) -> Some p)
  in
  let types =
    List.filter types_with_wrapped ~f:(fun (path, _, _) ->
        not (List.mem path ~set:wrapped))
    |> List.map ~f:(fun (path, td, wrapped) ->
        match wrapped with
        | None -> (path, td, None)
        | Some (prefix, has_attrs, has_loc_stack, p) ->
            ( path,
              td,
              Some (prefix, has_attrs, has_loc_stack, p, List.assoc p types) ))
  in
  (*  let all_types = List.map fst types in*)
  let types = List.sort types ~cmp:(fun (a, _, _) (b, _, _) -> compare a b) in
  let items fixed_loc =
    let module G = Gen (struct
      let fixed_loc = fixed_loc
    end) in
    List.map types ~f:(fun (path, td, wrapped) ->
        if is_abstract td then []
        else
          match wrapped with
          | None -> G.gen_td path td
          | Some (prefix, has_attrs, has_loc_stack, path', td') ->
              G.gen_td
                ~wrapper:(path, prefix, has_attrs, has_loc_stack)
                path' td')
    |> List.flatten
  in
  let mod_items b = items b |> List.map ~f:fst in
  let mod_sig_items b = items b |> List.map ~f:snd |> section_map_of_assoc in
  let mk_intf ~name located =
    let ident : label with_loc = { txt = name; loc } in
    let longident = { txt = Lident name; loc } in
    let documented_items =
      Section_map.fold
        (fun label items acc ->
          let label =
            match String.split_on_char ~sep:'_' label with
            | [] -> assert false
            | l :: rest ->
                let bs = Bytes.of_string l in
                Bytes.set bs 0 (Char.uppercase_ascii @@ Bytes.get bs 0);
                String.concat ~sep:" " (Bytes.to_string bs :: rest)
          in
          (floating_comment (Format.asprintf "{2 %s}" label) :: items) @ acc)
        (mod_sig_items located) []
    in
    let items =
      if located then M.sigi "val loc : Location.t" :: documented_items
      else documented_items
    in
    let intf = Str.modtype (Mtd.mk ~typ:(Mty.signature items) ident) in
    (longident, intf)
  in
  let intf_name, intf = mk_intf ~name:"Intf" false in
  let intf_located_name, intf_located = mk_intf ~name:"Intf_located" true in
  let st =
    [
      Str.open_ (Opn.mk (Mod.ident (Loc.lident "Import")));
      intf;
      intf_located;
      Str.module_
        (Mb.mk (Loc.mk (Some "M"))
           (Mod.constraint_
              (Mod.structure (mod_items false))
              (Mty.ident intf_name)));
      Str.module_
        (Mb.mk (Loc.mk (Some "Make"))
           (Mod.functor_
              (Named
                 ( Loc.mk (Some "Loc"),
                   Mty.signature
                     [ Sig.value (Val.mk (Loc.mk "loc") (M.ctyp "Location.t")) ]
                 ))
              (Mod.constraint_
                 (Mod.structure (M.stri "let loc = Loc.loc" :: mod_items true))
                 (Mty.ident intf_located_name))));
    ]
  in
  dump "ast_builder_generated" Pprintast.structure st ~ext:".ml"

let args = []
let usage = Printf.sprintf "%s [options] <.ml files>\n" Sys.argv.(0)

let () =
  let fns = ref [] in
  Arg.parse (Arg.align args) (fun fn -> fns := fn :: !fns) usage;
  try List.iter (List.rev !fns) ~f:generate
  with exn ->
    Astlib.Location.report_exception Format.err_formatter exn;
    exit 2
