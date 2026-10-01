(******************************************************************************
 *                                  OxCaml                                    *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open Ast_helper

let mknoloc = Location.mknoloc

let unsupported what =
  Misc.fatal_errorf "Parsetree_of_outcometree: %s is not supported" what

let rec longident_of_out_ident : Outcometree.out_ident -> Longident.t =
  function
  | Oide_ident name -> Lident name.printed_name
  | Oide_dot (p, name) ->
      Ldot (mknoloc (longident_of_out_ident p), mknoloc name)
  | Oide_apply (p, q) ->
      Lapply
        (mknoloc (longident_of_out_ident p), mknoloc (longident_of_out_ident q))
  | Oide_hash p ->
      (* [t#] is the constructor ["t#"], see [Parser.unboxed_type]. *)
      begin match longident_of_out_ident p with
      | Lident name -> Lident (name ^ "#")
      | Ldot (p, name) -> Ldot (p, { name with txt = name.txt ^ "#" })
      | Lapply _ -> unsupported "Oide_hash of an application"
      end

let lid p = mknoloc (longident_of_out_ident p)

let modes = List.map (fun m -> mknoloc (Parsetree.Mode m))

let arg_label : Outcometree.arg_label -> Asttypes.arg_label = function
  | Nolabel -> Nolabel
  | Labelled l -> Labelled l
  | Optional l -> Optional l
  | Position _ -> unsupported "Position"

let modalities = List.map (fun m -> mknoloc (Parsetree.Modality m))

let rec jkind_annotation (jkind : Outcometree.out_jkind)
    : Parsetree.jkind_annotation =
  let mk pjka_desc : Parsetree.jkind_annotation =
    { pjka_desc; pjka_loc = Location.none }
  in
  match jkind with
  | Ojkind_const const -> mk (jkind_const const)
  | Ojkind_product jkinds -> mk (Pjk_product (List.map jkind_annotation jkinds))
  | Ojkind_var _ -> unsupported "Ojkind_var"
  | Ojkind_addressable _ -> unsupported "Ojkind_addressable"

and jkind_const (const : Outcometree.out_jkind_const)
    : Parsetree.jkind_annotation_desc =
  let mk pjka_desc : Parsetree.jkind_annotation =
    { pjka_desc; pjka_loc = Location.none }
  in
  match const with
  | Ojkind_const_default -> Pjk_default
  | Ojkind_const_abbreviation (name, []) ->
      Pjk_abbreviation (mknoloc (Longident.Lident name))
  | Ojkind_const_abbreviation (name, axes) ->
      Pjk_operator
        (mk (Pjk_abbreviation (mknoloc (Longident.Lident name))),
         List.map mknoloc axes)
  | Ojkind_const_mod (Some base, modes') ->
      Pjk_mod (mk (jkind_const base), modes modes')
  | Ojkind_const_mod (None, _) -> unsupported "Ojkind_const_mod without base"
  | Ojkind_const_with (base, ty, modalities') ->
      Pjk_with (mk (jkind_const base), core_type ty, modalities modalities')
  | Ojkind_const_product consts ->
      Pjk_product (List.map (fun const -> mk (jkind_const const)) consts)
  | Ojkind_const_kind_of _ -> unsupported "Ojkind_const_kind_of"

and closed_flag closed : Asttypes.closed_flag = if closed then Closed else Open

and core_type (ty : Outcometree.out_type) : Parsetree.core_type =
  let labelled (label, ty) = (label, core_type ty) in
  match ty with
  | Otyp_var (_, name) -> Typ.var name None
  | Otyp_constr (p, args) -> Typ.constr (lid p) (List.map core_type args)
  | Otyp_arrow (label, arg_modes, arg, ret) ->
      let ret_modes, ret =
        match ret with
        | Otyp_ret ((Orm_any ret_modes | Orm_parens ret_modes), ret) ->
            ret_modes, ret
        | Otyp_ret (Orm_no_parens, ret) -> [], ret
        | ret -> [], ret
      in
      Typ.arrow (arg_label label) (core_type arg) (core_type ret)
        (modes arg_modes) (modes ret_modes)
  | Otyp_tuple tys -> Typ.tuple (List.map labelled tys)
  | Otyp_unboxed_tuple tys -> Typ.unboxed_tuple (List.map labelled tys)
  | Otyp_alias { non_gen = _; aliased; alias } ->
      Typ.alias (core_type aliased) (Some (mknoloc alias)) None
  | Otyp_poly (vars, ty) ->
      Typ.poly
        (List.map
           (fun (v, jkind) -> (mknoloc v, Option.map jkind_annotation jkind))
           vars)
        (core_type ty)
  | Otyp_variant (Ovar_fields fields, closed, tags) ->
      Typ.variant
        (List.map
           (fun (tag, empty, tys) ->
              Rf.tag (mknoloc tag) empty (List.map core_type tys))
           fields)
        (closed_flag closed) tags
  | Otyp_variant (Ovar_typ ty, closed, tags) ->
      Typ.variant [Rf.inherit_ (core_type ty)] (closed_flag closed) tags
  | Otyp_object { fields; open_row } ->
      Typ.object_
        (List.map (fun (name, ty) -> Of.tag (mknoloc name) (core_type ty))
           fields)
        (closed_flag (not open_row))
  | Otyp_class (p, args) -> Typ.class_ (lid p) (List.map core_type args)
  | Otyp_module { opack_path; opack_cstrs } ->
      Typ.package
        { ppt_path = lid opack_path;
          ppt_cstrs =
            List.map
              (fun (name, ty) ->
                 (mknoloc (Longident.Lident name), core_type ty))
              opack_cstrs;
          ppt_loc = Location.none;
          ppt_attrs = [] }
  | Otyp_jkind_annot (Otyp_var (_, name), jkind) ->
      Typ.var name (Some (jkind_annotation jkind))
  | Otyp_jkind_annot (ty, jkind) ->
      Typ.alias (core_type ty) None (Some (jkind_annotation jkind))
  | Otyp_abstract -> unsupported "Otyp_abstract"
  | Otyp_open -> unsupported "Otyp_open"
  | Otyp_manifest _ -> unsupported "Otyp_manifest"
  | Otyp_record _ -> unsupported "Otyp_record"
  | Otyp_record_unboxed_product _ -> unsupported "Otyp_record_unboxed_product"
  | Otyp_stuff _ -> unsupported "Otyp_stuff"
  | Otyp_sum _ -> unsupported "Otyp_sum"
  | Otyp_quote _ -> unsupported "Otyp_quote"
  | Otyp_splice _ -> unsupported "Otyp_splice"
  | Otyp_repr _ -> unsupported "Otyp_repr"
  | Otyp_newlayout _ -> unsupported "Otyp_newlayout"
  | Otyp_attribute _ -> unsupported "Otyp_attribute"
  | Otyp_mod _ -> unsupported "Otyp_mod"
  | Otyp_of_kind _ -> unsupported "Otyp_of_kind"
  | Otyp_ret _ -> unsupported "Otyp_ret"
