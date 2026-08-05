(******************************************************************************
 *                                  OxCaml                                    *
 *                       Jakub Bachurski, Jane Street                         *
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

open Types
open Typedtree

(* wildcard annotations *)
let newvar () = Ctype.newvar (Jkind.Builtin.any ~why:Dummy_jkind)

let newcorevar ctyp_env ctyp_loc =
  { ctyp_desc = Ttyp_var (None, None);
    ctyp_type = newvar ();
    ctyp_env;
    ctyp_loc;
    ctyp_attributes = []
  }

(* Approximate the [core_type] for type annotation from a given [type_expr].
   Used for annotating the results of type inspections in quotes. *)
let approximate_type_annotation env loc typ =
  let unwrap_univar ty =
    match get_desc ty with
    | Tunivar { name = Some name; jkind } -> Some (name, jkind.annotation)
    | Tunivar { name = None; jkind = _ } -> None
    | _ ->
      Misc.fatal_errorf
        "Type_inspection [at %a]: a named universal type variable was expected \
         to appear within this type"
        Location.print_loc_in_lowercase loc
  in
  let aliasable ty =
    match get_desc ty with Tvar _ | Tunivar _ -> false | _ -> true
  in
  let rec go aliased ty =
    (* CR metaprogramming jbachurski: Once jkind annotations are supported
       in quotes, we should use [any] wildcards:
       (Jkind.Builtin.any ~why:Wildcard).annotation *)
    let ctyp_desc =
      if aliasable ty && List.memq ty aliased
      then Ttyp_var (None, None)
      else
        let go = go (ty :: aliased) in
        match get_desc ty with
        | Tvar { name = _; jkind } | Tof_kind jkind ->
          Ttyp_var (None, jkind.annotation)
        | Tunivar _ ->
          let name, jkind_annotation = unwrap_univar ty |> Option.get in
          Ttyp_var (Some name, jkind_annotation)
        | Tarrow ((arg_label, _, _), ty, ty', _) ->
          Ttyp_arrow
            ( arg_label,
              go ty,
              Typemode.transl_alloc_mode [],
              go ty',
              Typemode.transl_alloc_mode [] )
        | Tpoly (ty, tyl) -> (
          let cty = go ty in
          match List.filter_map unwrap_univar tyl with
          | [] -> cty.ctyp_desc
          | _ :: _ as ctyl -> Ttyp_poly (ctyl, go ty))
        | Trepr _ ->
          Misc.fatal_errorf "Type_inspection [at %a]: unexpected Trepr"
            Location.print_loc_in_lowercase loc
        | Ttuple tyl -> Ttyp_tuple (List.map (fun (l, ty') -> l, go ty') tyl)
        | Tunboxed_tuple tyl ->
          Ttyp_unboxed_tuple (List.map (fun (l, ty') -> l, go ty') tyl)
        | Tconstr (p, tyl, _) ->
          Ttyp_constr
            (p, mkloc (Untypeast.lident_of_path p) loc, List.map go tyl)
        | Tmod _ ->
          Misc.fatal_errorf "Type_inspection [at %a]: unexpected Tmod"
            Location.print_loc_in_lowercase loc
        | Tobject (fields, _) ->
          let Out_type.{ fields; open_row } =
            Out_type.tree_of_typobject_repr fields
          in
          let fields =
            List.map
              (fun (label, ty') ->
                { of_desc = OTtag (mkloc label loc, go ty');
                  of_loc = loc;
                  of_attributes = []
                })
              fields
          in
          Ttyp_object (fields, if open_row then Open else Closed)
        | Tvariant row ->
          let Out_type.
                { fields; name = _; closed; present = _; all_present = _; tags }
              =
            Out_type.tree_of_typvariant_repr row
          in
          let fields =
            List.map
              (fun (l, p, tyl) ->
                { rf_desc = Ttag (mkloc l loc, p, List.map go tyl);
                  rf_loc = loc;
                  rf_attributes = []
                })
              fields
          in
          Ttyp_variant (fields, (if closed then Closed else Open), tags)
        | Tquote ty -> Ttyp_quote (go ty)
        | Tbox ty ->
          let lident = Untypeast.lident_of_path Predef.path_box in
          Ttyp_constr (Predef.path_box, mkloc lident loc, [go ty])
        | Tsplice _ ->
          (* This is a metaprogramming-specific fatal error until we have
             a nice error message for open type quotes (ticket 6357). *)
          Misc.fatal_errorf
            "Type_inspection [at %a]: Splices cannot appear in elaborated type \
             annotations."
            Location.print_loc_in_lowercase loc
        | Tquote_eval _ ->
          let lident = Untypeast.lident_of_path Predef.path_eval in
          Ttyp_constr
            (Predef.path_eval, mkloc lident loc, [go (Btype.new_quote_ty ty)])
        | Tpackage { pack_path; pack_cstrs } ->
          Ttyp_package
            { tpt_path = pack_path;
              tpt_cstrs =
                List.map
                  (fun (parts, ty) ->
                    mkloc (Longident.unflatten parts |> Option.get) loc, go ty)
                  pack_cstrs;
              tpt_type = Mty_ident pack_path;
              tpt_txt = mkloc (Untypeast.lident_of_path pack_path) loc
            }
        | Tlink _ | Tsubst _ | Tfield _ | Tnil ->
          Misc.fatal_errorf
            "Type_inspection [at %a]: unexpected type expression"
            Location.print_loc_in_lowercase loc
    in
    { ctyp_desc;
      ctyp_type = ty;
      ctyp_env = env;
      ctyp_loc = loc;
      ctyp_attributes = []
    }
  in
  go [] typ

let annotation_of_type_inspection (type a) env loc :
    a type_inspection -> core_type option =
 fun type_inspection ->
  let core_type ctyp_desc =
    { ctyp_desc;
      ctyp_type = newvar ();
      ctyp_env = env;
      ctyp_loc = loc;
      ctyp_attributes = []
    }
  in
  match type_inspection with
  | Label_disambiguation ambiguity ->
    begin match ambiguity with
    | Unambiguous -> None
    | Ambiguous { path; arity } ->
      approximate_type_annotation env loc
        (Btype.newgenty
           (Tconstr (path, List.init arity (fun _ -> newvar ()), ref Mnil)))
      |> Option.some
    end
  | Polymorphic_parameter (Param ty) ->
    approximate_type_annotation env loc ty |> Option.some
  | Polymorphic_parameter (Method (met, ty)) ->
    let met_cty = approximate_type_annotation env loc ty in
    let met_field =
      { of_desc = OTtag (met, met_cty); of_loc = loc; of_attributes = [] }
    in
    core_type (Ttyp_object ([met_field], Open)) |> Option.some
  | Polymorphic_parameter (Arrow params) ->
    List.fold_right
      (fun (arg_lbl, sch) spine ->
        core_type
          (Ttyp_arrow
             ( arg_lbl,
               (match sch with
               | Some sch -> approximate_type_annotation env loc sch
               | None -> newcorevar env loc),
               Typemode.transl_alloc_mode [],
               spine,
               Typemode.transl_alloc_mode [] )))
      params (newcorevar env loc)
    |> Option.some
  | Module_pack pty -> approximate_type_annotation env loc pty |> Option.some

let elaborate_type_inspections (mapper : Tast_mapper.mapper) :
    Tast_mapper.mapper =
  let expr sub (e : expression) =
    let exp_extra =
      List.filter_map
        (function
          | Texp_inspected_type type_inspection, loc, attributes ->
            annotation_of_type_inspection e.exp_env loc type_inspection
            |> Option.map (fun cty -> Texp_constraint cty, loc, attributes)
          | extra -> Some extra)
        e.exp_extra
    in
    mapper.expr sub { e with exp_extra }
  in
  let pat (type k) sub (p : k general_pattern) =
    let pat_extra =
      List.filter_map
        (function
          | Tpat_inspected_type type_inspection, loc, attributes ->
            annotation_of_type_inspection p.pat_env loc type_inspection
            |> Option.map (fun cty ->
                ( Tpat_constraint (Some cty, Typemode.transl_alloc_mode []),
                  loc,
                  attributes ))
          | extra -> Some extra)
        p.pat_extra
    in
    mapper.pat sub { p with pat_extra }
  in
  { mapper with expr; pat }
