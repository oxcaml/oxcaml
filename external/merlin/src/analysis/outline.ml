(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                                Thomas Refis  <refis.thomas(_)gmail.com>
                                Simon Castellan  <simon.castellan(_)iuwt.fr>

     Permission is hereby granted, free of charge, to any person obtaining a
     copy of this software and associated documentation files (the "Software"),
     to deal in the Software without restriction, including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
     sell copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in
     all copies or substantial portions of the Software.

     The Software is provided "as is", without warranty of any kind, express or
     implied, including but not limited to the warranties of merchantability,
     fitness for a particular purpose and noninfringement. In no event shall
     the authors or copyright holders be liable for any claim, damages or other
     liability, whether in an action of contract, tort or otherwise, arising
     from, out of or in connection with the software or the use or other dealings
     in the Software.

   )* }}} *)

open Std
open Option.Infix

(* Réglisse la police *)
open Typedtree

open Browse_raw
open Browse_tree

let id_of_patt = function
  | { pat_desc = Tpat_var { id; name; _ }; _ } -> Some (id, name.loc)
  | _ -> None

let mk ?(children = []) ~location ~selection ~deprecated outline_kind
    outline_type id =
  { Query_protocol.outline_kind;
    outline_type;
    location;
    selection;
    children;
    outline_name = Ident.name id;
    deprecated
  }

let get_class_field_desc_infos = function
  | Typedtree.Tcf_val (str_loc, _, _, _, _) -> Some (str_loc, `Value)
  | Typedtree.Tcf_method (str_loc, _, _) -> Some (str_loc, `Method)
  | _ -> None

let get_class_signature_field_desc_infos = function
  | Typedtree.Tctf_val (name, _, _, _) -> Some (name, `Value)
  | Typedtree.Tctf_method (name, _, _, _) -> Some (name, `Method)
  | _ -> None

let outline_type ~include_types ~env typ =
  match include_types with
  | true ->
    let ppf, to_string = Format.to_string () in
    Printtyp.wrap_printing_env env (fun () ->
        Type_utils.print_type_with_decl ~verbosity:(Mconfig.Verbosity.Lvl 0) env
          ppf typ);
    Some (to_string ())
  | false -> None

let rec summarize ~include_types node =
  let location = node.t_loc in
  match node.t_node with
  | Value_binding vb ->
    let children =
      List.concat_map
        (Lazy.force node.t_children)
        ~f:(get_val_elements ~include_types)
    in
    let deprecated = Type_utils.is_deprecated vb.vb_attributes in
    begin match id_of_patt vb.vb_pat with
    | None -> None
    | Some (ident, selection) ->
      let typ =
        outline_type ~include_types ~env:node.t_env vb.vb_pat.pat_type
      in
      Some (mk ~children ~location ~selection ~deprecated `Value typ ident)
    end
  | Value_description vd ->
    let deprecated = Type_utils.is_deprecated vd.val_attributes in
    let typ = outline_type ~include_types ~env:node.t_env vd.val_val.val_type in
    Some
      (mk ~location ~selection:vd.val_name.loc ~deprecated `Value typ vd.val_id)
  | Module_declaration md ->
    let children = get_mod_children ~include_types node in
    begin match (md.md_id, md.md_name) with
    | None, _ | _, { txt = None; _ } -> None
    | Some id, { loc = selection; _ } ->
      let deprecated = Type_utils.is_deprecated md.md_attributes in
      Some (mk ~children ~location ~selection ~deprecated `Module None id)
    end
  | Module_binding mb ->
    let children = get_mod_children ~include_types node in
    begin match (mb.mb_id, mb.mb_name) with
    | None, _ | _, { txt = None; _ } -> None
    | Some id, { loc = selection; _ } ->
      let deprecated = Type_utils.is_deprecated mb.mb_attributes in
      Some (mk ~children ~location ~selection ~deprecated `Module None id)
    end
  | Module_type_declaration mtd ->
    let children = get_mod_children ~include_types node in
    let deprecated = Type_utils.is_deprecated mtd.mtd_attributes in
    Some
      (mk ~deprecated ~children ~location ~selection:mtd.mtd_name.loc `Modtype
         None mtd.mtd_id)
  | Type_declaration td ->
    let children =
      List.concat_map (Lazy.force node.t_children) ~f:(fun child ->
          match child.t_node with
          | Type_kind _ ->
            List.map (Lazy.force child.t_children) ~f:(fun x ->
                match x.t_node with
                | Constructor_declaration c ->
                  let deprecated = Type_utils.is_deprecated c.cd_attributes in
                  mk `Constructor None c.cd_id ~deprecated ~location:c.cd_loc
                    ~selection:c.cd_name.loc
                | Label_declaration ld ->
                  let deprecated = Type_utils.is_deprecated ld.ld_attributes in
                  mk `Label None ld.ld_id ~deprecated ~location:ld.ld_loc
                    ~selection:ld.ld_name.loc
                | _ -> assert false (* ! *))
          | _ -> [])
    in
    let deprecated = Type_utils.is_deprecated td.typ_attributes in
    Some
      (mk ~children ~location ~selection:td.typ_name.loc ~deprecated `Type None
         td.typ_id)
  | Type_extension te ->
    let name = Path.name te.tyext_path in
    let children =
      List.filter_map (Lazy.force node.t_children) ~f:(fun x ->
          summarize ~include_types x >>| fun x ->
          { x with Query_protocol.outline_kind = `Constructor })
    in
    let deprecated = Type_utils.is_deprecated te.tyext_attributes in
    Some
      { Query_protocol.outline_name = name;
        outline_kind = `Type;
        outline_type = None;
        location;
        selection = te.tyext_txt.loc;
        children;
        deprecated
      }
  | Extension_constructor ec ->
    let deprecated = Type_utils.is_deprecated ec.ext_attributes in
    Some
      (mk ~location ~selection:ec.ext_name.loc `Exn None ec.ext_id ~deprecated)
  | Class_declaration cd ->
    let children =
      try get_class_expr_elements ~include_types cd.ci_expr with _ -> []
    in
    let deprecated = Type_utils.is_deprecated cd.ci_attributes in
    Some
      (mk ~children ~location ~selection:cd.ci_id_name.loc `Class None
         cd.ci_id_class_type ~deprecated)
  | Class_type_declaration ctd ->
    let children =
      List.concat_map
        (Lazy.force node.t_children)
        ~f:(get_class_elements ~include_types)
    in
    let deprecated = Type_utils.is_deprecated ctd.ci_attributes in
    Some
      (mk ~children ~location ~selection:ctd.ci_id_name.loc `ClassType None
         ctd.ci_id_class_type ~deprecated)
  | _ -> None

and get_val_elements ~include_types node =
  match node.t_node with
  | Expression _ ->
    List.concat_map
      (Lazy.force node.t_children)
      ~f:(get_val_elements ~include_types)
  | Class_expr _ | Class_structure _ -> get_class_elements ~include_types node
  | _ -> Option.to_list (summarize ~include_types node)

and get_class_elements ~include_types node =
  match node.t_node with
  | Class_type { cltyp_desc = Tcty_signature { csig_fields; _ }; _ } ->
    List.filter_map csig_fields ~f:(fun field ->
        match get_class_signature_field_desc_infos field.ctf_desc with
        | Some (name, outline_kind) ->
          let deprecated = Type_utils.is_deprecated field.ctf_attributes in
          Some
            { Query_protocol.outline_name = name;
              outline_kind;
              outline_type = None;
              location = field.ctf_loc;
              selection = field.ctf_loc;
              children = [];
              deprecated
            }
        | None -> None)
  | Class_expr class_expr -> get_class_expr_elements ~include_types class_expr
  | Class_field cf -> get_class_field_elements ~include_types cf
  | Class_field_kind (Tcfk_concrete (_, expr)) ->
    get_expr_elements ~include_types expr
  | Class_field_kind _ -> []
  | Class_structure class_structure ->
    get_class_structure_elements ~include_types class_structure
  | _ -> []

and get_class_expr_elements ~include_types class_expr =
  match class_expr.cl_desc with
  | Tcl_structure class_structure ->
    get_class_structure_elements ~include_types class_structure
  | Tcl_fun (_, _, _, class_expr, _)
  | Tcl_constraint (class_expr, _, _, _, _)
  | Tcl_open (_, class_expr) ->
    get_class_expr_elements ~include_types class_expr
  | Tcl_let (_, vbs, _, class_expr) ->
    let bindings =
      List.filter_map (List.rev vbs)
        ~f:(get_value_binding_element ~include_types)
    in
    bindings @ get_class_expr_elements ~include_types class_expr
  | Tcl_apply (class_expr, _) ->
    get_class_expr_elements ~include_types class_expr
  | Tcl_ident _ -> []

and get_class_structure_elements ~include_types class_structure =
  List.concat_map
    (List.rev class_structure.cstr_fields)
    ~f:(get_class_field_elements ~include_types)

and get_class_field_elements ~include_types cf =
  let children =
    match cf.cf_desc with
    | Tcf_val (_, _, _, Tcfk_concrete (_, expr), _) ->
      get_expr_elements ~include_types expr
    | _ -> []
  in
  match get_class_field_desc_infos cf.cf_desc with
  | Some (str_loc, outline_kind) ->
    let deprecated = Type_utils.is_deprecated cf.cf_attributes in
    [ { Query_protocol.outline_name = str_loc.Location.txt;
        outline_kind;
        outline_type = None;
        location = cf.cf_loc;
        selection = str_loc.loc;
        children;
        deprecated
      }
    ]
  | None -> []

and get_expr_elements ~include_types expr =
  match expr.exp_desc with
  | Texp_object (class_structure, _) ->
    get_class_structure_elements ~include_types class_structure
  | Texp_let (_, vbs, body) ->
    let bindings =
      List.filter_map (List.rev vbs)
        ~f:(get_value_binding_element ~include_types)
    in
    bindings @ get_expr_elements ~include_types body
  | Texp_letmutable (vb, body) ->
    Option.to_list (get_value_binding_element ~include_types vb)
    @ get_expr_elements ~include_types body
  | _ -> []

and get_value_binding_element ~include_types vb =
  match id_of_patt vb.vb_pat with
  | None -> None
  | Some (ident, selection) ->
    let children = get_expr_elements ~include_types vb.vb_expr in
    let deprecated = Type_utils.is_deprecated vb.vb_attributes in
    let typ =
      outline_type ~include_types ~env:vb.vb_expr.exp_env vb.vb_pat.pat_type
    in
    Some
      (mk ~children ~location:vb.vb_loc ~selection ~deprecated `Value typ ident)

and get_mod_children ~include_types node =
  List.concat_map
    (Lazy.force node.t_children)
    ~f:(remove_mod_indir ~include_types)

and remove_mod_indir ~include_types node =
  match node.t_node with
  | Module_expr _ | Module_type _ -> get_mod_children ~include_types node
  | _ -> remove_top_indir ~include_types node

and remove_top_indir ~include_types t =
  match t.t_node with
  | Structure _ | Signature _ -> get_mod_children ~include_types t
  | Signature_item _ | Structure_item _ ->
    List.concat_map (Lazy.force t.t_children) ~f:(fun child ->
        match child.t_node with
        | Include_declaration _ | Include_description _ ->
          get_mod_children ~include_types child
        | _ -> summarize ~include_types child |> Option.to_list)
  | _ -> []

let get ~include_types browses =
  List.concat @@ List.rev_map ~f:(remove_top_indir ~include_types) browses

let shape cursor nodes =
  let rec aux node =
    (* A node is selected if:
       - part of the module language
       - or under the cursor *)
    let selected =
      match node.t_node with
      | Module_expr _
      | Module_type_constraint _
      | Structure _
      | Structure_item _
      | Module_binding _
      | Module_type _
      | Signature _
      | Signature_item _
      | Module_declaration _
      | Module_type_declaration _
      | Module_binding_name _
      | Module_declaration_name _
      | Module_type_declaration_name _ -> not node.t_loc.Location.loc_ghost
      | _ ->
        Location_aux.compare_pos cursor node.t_loc = 0
        && Lexing.compare_pos node.t_loc.Location.loc_start cursor <> 0
        && Lexing.compare_pos node.t_loc.Location.loc_end cursor <> 0
    in
    if selected then
      [ { Query_protocol.shape_loc = node.t_loc;
          shape_sub = List.concat_map ~f:aux (Lazy.force node.t_children)
        }
      ]
    else []
  in
  List.concat_map ~f:aux nodes
