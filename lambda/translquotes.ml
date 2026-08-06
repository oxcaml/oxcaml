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

type namespace =
  | Value
  | Type
  | Label
  | Constructor

type occurrence =
  | Bound
  | Free

let map_til_unquote (mapper : Tast_mapper.mapper) =
  let expr sub (e : Typedtree.expression) =
    match e.exp_desc with Texp_unquote _ -> e | _ -> mapper.expr sub e
  in
  { mapper with expr }

let location_like Location.{ txt = _; loc } txt = Location.{ txt; loc }

let constructor_path (cd : Data_types.constructor_description) : Path.t =
  match cd.cstr_tag with
  | Extension path -> path
  | Ordinary _ | Null -> (
    match Data_types.cstr_res_type_path cd with
    | Pident _ -> Pident (Ident.create_predef cd.cstr_name)
    | Pdot (path, _) -> Pdot (path, cd.cstr_name)
    | _ -> Misc.fatal_errorf "Translquotes: unexpected constructor type path")

let label_path (ld : _ Data_types.gen_label_description) : Path.t =
  match Data_types.gen_lbl_res_type_path ld with
  | Pident _ -> Pident (Ident.create_predef ld.lbl_name)
  | Pdot (path, _) -> Pdot (path, ld.lbl_name)
  | _ -> Misc.fatal_errorf "Translquotes: unexpected constructor type path"

let rec map_ident f : Path.t -> Path.t = function
  | Pident id -> Pident (f id)
  | Pdot (p, s) -> Pdot (map_ident f p, s)
  | Papply (p1, p2) -> Papply (map_ident f p1, map_ident f p2)
  | Pextra_ty (p, extra_ty) -> Pextra_ty (map_ident f p, extra_ty)

let unwrap_lident : Longident.t -> string = function
  | Lident x -> x
  | Ldot _ | Lapply _ -> Misc.fatal_errorf "Translquotes: expected Lident"

let stamped_suffix = "/TranslquotesStamped"

module Stamped_ident : sig
  type t

  val of_ident : Ident.t -> t

  val to_ident : t -> Ident.t

  val to_base : t -> string
end = struct
  type t =
    { ident : Ident.t;
      base : string
    }

  let of_ident ident =
    { ident =
        Ident.unique_toplevel_name ident ^ stamped_suffix
        |> Ident.create_persistent;
      base = Ident.name ident
    }

  let to_ident x = x.ident

  let to_base x = x.base
end

let elaborate_identifiers env (mapper : Tast_mapper.mapper) =
  let bound = Hashtbl.create 0 in
  let free = Hashtbl.create 0 in
  let register_identifier (space : namespace) (occ : occurrence) (path : Path.t)
      =
    let path' =
      map_ident
        (fun ident ->
          if not (Env.path_is_persistent_in_quotations env (Pident ident))
          then (
            let ident' = Stamped_ident.of_ident ident in
            Hashtbl.add free (Ident.name (Stamped_ident.to_ident ident')) ident';
            Stamped_ident.to_ident ident')
          else ident)
        path
    in
    begin match Env.path_is_persistent_in_quotations env path, space, occ with
    | true, _, Free ->
      (* CR translquotes: there might be a more precise compilation unit than [Path.head] *)
      Env.add_required_global_for_quote (Pident (Path.head path)) env;
      path
    | true, _, Bound ->
      Misc.fatal_errorf "binding of global persistent identifier"
    | false, _, _ ->
      (match occ with
      | Free ->
        (* already added as free *)
        ()
      | Bound ->
        begin match path with
        | Pident ident ->
          let ident' = Stamped_ident.of_ident ident in
          if Hashtbl.mem bound ident'
          then Misc.fatal_errorf "Translquotes: duplicated binding";
          Hashtbl.add bound ident' ();
          Hashtbl.add free (Ident.name (Stamped_ident.to_ident ident')) ident'
        | _ -> Misc.fatal_errorf "Translquotes: binding of non-Pident path"
        end);
      path'
    end
  in
  let register_loop_var id pat =
    Parsetree.
      { pat with
        ppat_desc =
          Ppat_var
            (Location.mkloc
               (register_identifier Value Bound (Pident id)
               |> Untypeast.lident_of_path |> unwrap_lident)
               pat.ppat_loc)
      }
  in
  let comprehension ({ comp_body; comp_clauses } : Typedtree.comprehension) :
      Typedtree.comprehension =
    let open Typedtree in
    { comp_body;
      comp_clauses =
        List.map
          (function
            | Texp_comp_for iterators ->
              Texp_comp_for
                (List.map
                   (function
                     | { comp_cb_iterator =
                           Texp_comp_range
                             { ident;
                               ident_debug_uid;
                               pattern;
                               start;
                               stop;
                               direction
                             };
                         comp_cb_attributes
                       } ->
                       { comp_cb_iterator =
                           Texp_comp_range
                             { ident;
                               ident_debug_uid;
                               pattern = register_loop_var ident pattern;
                               start;
                               stop;
                               direction
                             };
                         comp_cb_attributes
                       }
                     | { comp_cb_iterator = Texp_comp_in { pattern; sequence };
                         comp_cb_attributes
                       } ->
                       { comp_cb_iterator = Texp_comp_in { pattern; sequence };
                         comp_cb_attributes
                       })
                   iterators)
            | Texp_comp_when expr -> Texp_comp_when expr)
          comp_clauses
    }
  in
  let expr sub (e : Typedtree.expression) =
    let exp_desc =
      let open Typedtree in
      match e.exp_desc with
      | Texp_ident { path; lid; desc; kind; unique_use; staticity; mode } ->
        Texp_ident
          { path;
            lid =
              register_identifier Value Free path
              |> Untypeast.lident_of_path |> location_like lid;
            desc;
            kind;
            unique_use;
            staticity;
            mode
          }
      | Texp_apply_layout (exp, args) -> Texp_apply_layout (exp, args)
      | Texp_constant c -> Texp_constant c
      | Texp_let (rec_flag, list, exp) -> Texp_let (rec_flag, list, exp)
      | Texp_letmutable (vb, exp) -> Texp_letmutable (vb, exp)
      | Texp_function
          { params; body; alloc_mode; ret_mode; ret_sort; yielding; zero_alloc }
        ->
        Texp_function
          { params =
              List.map
                (fun { fp_arg_label;
                       fp_param;
                       fp_param_debug_uid;
                       fp_partial;
                       fp_kind;
                       fp_sort;
                       fp_mode;
                       fp_curry;
                       fp_newtypes;
                       fp_loc
                     } ->
                  { fp_arg_label;
                    fp_param;
                    fp_param_debug_uid;
                    fp_partial;
                    fp_kind;
                    fp_sort;
                    fp_mode;
                    fp_curry;
                    fp_newtypes =
                      List.map
                        (fun (id, name, jkind, uid) ->
                          ( id,
                            register_identifier Type Bound (Pident id)
                            |> Untypeast.lident_of_path |> unwrap_lident
                            |> location_like name,
                            jkind,
                            uid ))
                        fp_newtypes;
                    fp_loc
                  })
                params;
            body;
            alloc_mode;
            ret_mode;
            ret_sort;
            yielding;
            zero_alloc
          }
      | Texp_apply (exp, list, pos, am, ym, za) ->
        Texp_apply (exp, list, pos, am, ym, za)
      | Texp_match (exp, sort, cases, eff_cases, p) ->
        Texp_match (exp, sort, cases, eff_cases, p)
      | Texp_try (exp, exn_cases, eff_cases) ->
        Texp_try (exp, exn_cases, eff_cases)
      | Texp_unboxed_unit -> Texp_unboxed_unit
      | Texp_unboxed_bool b -> Texp_unboxed_bool b
      | Texp_tuple (list, am) -> Texp_tuple (list, am)
      | Texp_unboxed_tuple list -> Texp_unboxed_tuple list
      | Texp_construct (lid, cd, rep, args, am) ->
        Texp_construct
          ( constructor_path cd
            |> register_identifier Constructor Free
            |> Untypeast.lident_of_path |> location_like lid,
            cd,
            rep,
            args,
            am )
      | Texp_variant (l, expo) -> Texp_variant (l, expo)
      | Texp_record { fields; representation; extended_expression; alloc_mode }
        ->
        Texp_record
          { fields =
              Array.map
                (fun (ld, sort, def) ->
                  ( ld,
                    sort,
                    match def with
                    | Kept (a, b, c) -> Kept (a, b, c)
                    | Overridden (lid, u) ->
                      Overridden
                        ( label_path ld
                          |> register_identifier Label Free
                          |> Untypeast.lident_of_path |> location_like lid,
                          u ) ))
                fields;
            representation;
            extended_expression;
            alloc_mode
          }
      | Texp_record_unboxed_product
          { fields; representation; extended_expression } ->
        Texp_record_unboxed_product
          { fields =
              Array.map
                (fun (ld, sort, def) ->
                  ( ld,
                    sort,
                    match def with
                    | Kept (a, b, c) -> Kept (a, b, c)
                    | Overridden (lid, u) ->
                      Overridden
                        ( label_path ld
                          |> register_identifier Label Free
                          |> Untypeast.lident_of_path |> location_like lid,
                          u ) ))
                fields;
            representation;
            extended_expression
          }
      | Texp_field
          { record;
            record_sort;
            record_repres;
            lid;
            label;
            boxing;
            unique_barrier
          } ->
        Texp_field
          { record;
            lid =
              label_path label
              |> register_identifier Label Free
              |> Untypeast.lident_of_path |> location_like lid;
            record_sort;
            record_repres;
            label;
            boxing;
            unique_barrier
          }
      | Texp_unboxed_field
          { record;
            record_sort;
            record_sorts;
            record_repres;
            lid;
            label;
            unique_use
          } ->
        Texp_unboxed_field
          { record;
            record_sort;
            record_sorts;
            record_repres;
            lid =
              label_path label
              |> register_identifier Label Free
              |> Untypeast.lident_of_path |> location_like lid;
            label;
            unique_use
          }
      | Texp_setfield
          { record; record_repres; record_sorts; modality; lid; label; newval }
        ->
        Texp_setfield
          { record;
            record_repres;
            record_sorts;
            modality;
            lid =
              label_path label
              |> register_identifier Label Free
              |> Untypeast.lident_of_path |> location_like lid;
            label;
            newval
          }
      | Texp_atomic_loc
          { record; record_sort; record_repres; lid; label; alloc_mode } ->
        Texp_atomic_loc
          { record;
            record_sort;
            record_repres;
            lid =
              label_path label
              |> register_identifier Constructor Free
              |> Untypeast.lident_of_path |> location_like lid;
            label;
            alloc_mode
          }
      | Texp_array (amut, sort, list, alloc_mode) ->
        Texp_array (amut, sort, list, alloc_mode)
      | Texp_idx (ba, uas) ->
        (* CR translquotes: elaborate [ba] and [uas] *)
        Texp_idx (ba, uas)
      | Texp_list_comprehension comp ->
        Texp_list_comprehension (comprehension comp)
      | Texp_array_comprehension (amut, sort, comp) ->
        Texp_array_comprehension (amut, sort, comprehension comp)
      | Texp_ifthenelse (exp1, exp2, expo) -> Texp_ifthenelse (exp1, exp2, expo)
      | Texp_sequence (exp1, jkind, exp2) -> Texp_sequence (exp1, jkind, exp2)
      | Texp_while wh -> Texp_while wh
      | Texp_for
          { for_id;
            for_debug_uid;
            for_pat;
            for_from;
            for_to;
            for_dir;
            for_body;
            for_body_sort
          } ->
        Texp_for
          { for_id;
            for_debug_uid;
            for_pat = register_loop_var for_id for_pat;
            for_from;
            for_to;
            for_dir;
            for_body;
            for_body_sort
          }
      | Texp_send (exp, meth, ap) -> Texp_send (exp, meth, ap)
      | Texp_new (path, lid, cd, apos) ->
        Texp_new
          ( path,
            register_identifier Value Free path
            |> Untypeast.lident_of_path |> location_like lid,
            cd,
            apos )
      | Texp_instvar (path1, path2, id) ->
        Texp_instvar
          ( path1,
            path2,
            register_identifier Value Free path2
            |> Untypeast.lident_of_path |> unwrap_lident |> location_like id )
      | Texp_mutvar id ->
        Texp_mutvar
          (register_identifier Value Free (Pident id.txt)
          |> Untypeast.lident_of_path |> unwrap_lident |> Ident.create_local
          |> location_like id)
      | Texp_setinstvar (path1, path2, id, exp) ->
        Texp_setinstvar
          ( path1,
            path2,
            register_identifier Value Free path2
            |> Untypeast.lident_of_path |> unwrap_lident |> location_like id,
            exp )
      | Texp_setmutvar (id, sort, exp) ->
        Texp_setmutvar
          ( register_identifier Value Free (Pident id.txt)
            |> Untypeast.lident_of_path |> unwrap_lident |> Ident.create_local
            |> location_like id,
            sort,
            exp )
      | Texp_override (path, list) ->
        Texp_override
          (path, List.map (fun (id, name, exp) -> id, name, exp) list)
      | Texp_letmodule (id, s, pres, mexpr, exp) ->
        Texp_letmodule
          ( id,
            Option.map
              (fun id ->
                register_identifier Value Bound (Pident id)
                |> Untypeast.lident_of_path |> unwrap_lident)
              id
            |> location_like s,
            pres,
            mexpr,
            exp )
      | Texp_letexception
          ( { ext_id; ext_name; ext_type; ext_kind; ext_loc; ext_attributes },
            exp ) ->
        Texp_letexception
          ( { ext_id;
              ext_name =
                register_identifier Constructor Bound (Pident ext_id)
                |> Untypeast.lident_of_path |> unwrap_lident
                |> location_like ext_name;
              ext_type;
              ext_kind;
              ext_loc;
              ext_attributes
            },
            exp )
      | Texp_assert (exp, loc) -> Texp_assert (exp, loc)
      | Texp_lazy exp -> Texp_lazy exp
      | Texp_object (cl, sl) -> Texp_object (cl, sl)
      | Texp_pack mexpr -> Texp_pack mexpr
      | Texp_letop
          { let_;
            ands;
            param;
            param_debug_uid;
            param_sort;
            body;
            body_sort;
            partial
          } ->
        Texp_letop
          { let_;
            ands;
            param;
            param_debug_uid;
            param_sort;
            body;
            body_sort;
            partial
          }
      | Texp_unreachable -> Texp_unreachable
      | Texp_extension_constructor (lid, path) ->
        Texp_extension_constructor
          ( register_identifier Constructor Bound path
            |> Untypeast.lident_of_path |> location_like lid,
            path )
      | Texp_open (od, e) -> Texp_open (od, e)
      | Texp_probe { name; handler; enabled_at_init } ->
        Texp_probe { name; handler; enabled_at_init }
      | Texp_probe_is_enabled _ as e -> e
      | Texp_exclave exp -> Texp_exclave exp
      | Texp_src_pos -> Texp_src_pos
      | Texp_overwrite (exp1, exp2) -> Texp_overwrite (exp1, exp2)
      | Texp_hole use -> Texp_hole use
      | Texp_quote exp -> Texp_quote exp
      | Texp_splice exp -> Texp_splice exp
      | Texp_unquote exp -> Texp_unquote exp
    in
    let exp_extra =
      List.map
        (fun (extra, loc, attrs) ->
          let extra =
            let open Typedtree in
            match extra with
            | Texp_newtype (id, name, jkind, uid) ->
              Texp_newtype
                ( id,
                  register_identifier Type Bound (Pident id)
                  |> Untypeast.lident_of_path |> unwrap_lident
                  |> location_like name,
                  jkind,
                  uid )
            | extra -> extra
          in
          extra, loc, attrs)
        e.exp_extra
    in
    mapper.expr sub { e with exp_desc; exp_extra }
  in
  let typ sub (t : Typedtree.core_type) =
    let ctyp_desc =
      let open Typedtree in
      match t.ctyp_desc with
      | Ttyp_var (x, jkind) -> Ttyp_var (x, jkind)
      | Ttyp_arrow (label, ct1, ma1, ct2, ma2) ->
        Ttyp_arrow (label, ct1, ma1, ct2, ma2)
      | Ttyp_tuple list ->
        Ttyp_tuple (List.map (fun (label, t) -> label, t) list)
      | Ttyp_unboxed_tuple list ->
        Ttyp_unboxed_tuple (List.map (fun (label, t) -> label, t) list)
      | Ttyp_constr (path, lid, list) ->
        Ttyp_constr
          ( path,
            register_identifier Type Free path
            |> Untypeast.lident_of_path |> location_like lid,
            list )
      | Ttyp_object (list, closed) -> Ttyp_object (list, closed)
      | Ttyp_class (path, lid, list) ->
        Ttyp_class
          ( path,
            register_identifier Type Free path
            |> Untypeast.lident_of_path |> location_like lid,
            list )
      | Ttyp_alias (ct, s, jkind) -> Ttyp_alias (ct, s, jkind)
      | Ttyp_variant (list, closed, labels) ->
        Ttyp_variant (list, closed, labels)
      | Ttyp_poly (vars, ct) -> Ttyp_poly (vars, ct)
      | Ttyp_package { tpt_path; tpt_cstrs; tpt_type; tpt_txt } ->
        Ttyp_package
          { tpt_path;
            tpt_cstrs;
            tpt_type;
            tpt_txt =
              register_identifier Type Free tpt_path
              |> Untypeast.lident_of_path |> location_like tpt_txt
          }
      | Ttyp_open (path, lid, t) ->
        Ttyp_open
          ( path,
            register_identifier Value Free path
            |> Untypeast.lident_of_path |> location_like lid,
            t )
      | Ttyp_repr (vars, ct) -> Ttyp_repr (vars, ct)
      | Ttyp_newlayout (vars, ct) -> Ttyp_newlayout (vars, ct)
      | Ttyp_of_kind jkind -> Ttyp_of_kind jkind
      | Ttyp_quote t -> Ttyp_quote t
      | Ttyp_splice t -> Ttyp_splice t
      | Ttyp_call_pos -> Ttyp_call_pos
    in
    mapper.typ sub { t with ctyp_desc }
  in
  let pat sub (p : _ Typedtree.general_pattern) =
    let go : type k. k Typedtree.pattern_desc -> k Typedtree.pattern_desc =
      let open Typedtree in
      function
      | Tpat_any -> Tpat_any
      | Tpat_var { id; name; uid; sort; mode } ->
        Tpat_var
          { id;
            name =
              register_identifier Value Bound (Pident id)
              |> Untypeast.lident_of_path |> unwrap_lident |> location_like name;
            uid;
            sort;
            mode
          }
      | Tpat_constant c -> Tpat_constant c
      | Tpat_unboxed_unit -> Tpat_unboxed_unit
      | Tpat_unboxed_bool b -> Tpat_unboxed_bool b
      | Tpat_tuple l -> Tpat_tuple l
      | Tpat_unboxed_tuple l -> Tpat_unboxed_tuple l
      | Tpat_construct (lid, cd, rep, l, vto) ->
        Tpat_construct
          ( constructor_path cd
            |> register_identifier Constructor Free
            |> Untypeast.lident_of_path |> location_like lid,
            cd,
            rep,
            l,
            vto )
      | Tpat_variant (l, po, rd) -> Tpat_variant (l, po, rd)
      | Tpat_record (fields, sorts, rep, closed) ->
        Tpat_record
          ( List.map
              (fun (lid, ld, p) ->
                ( label_path ld
                  |> register_identifier Label Free
                  |> Untypeast.lident_of_path |> location_like lid,
                  ld,
                  p ))
              fields,
            sorts,
            rep,
            closed )
      | Tpat_record_unboxed_product (fields, sorts, rep, closed) ->
        Tpat_record_unboxed_product
          ( List.map
              (fun (lid, ld, p) ->
                ( label_path ld
                  |> register_identifier Label Free
                  |> Untypeast.lident_of_path |> location_like lid,
                  ld,
                  p ))
              fields,
            sorts,
            rep,
            closed )
      | Tpat_array (am, arg_sort, l) -> Tpat_array (am, arg_sort, l)
      | Tpat_alias { pattern; id; name; uid; sort; mode; type_expr } ->
        Tpat_alias
          { pattern;
            id;
            name =
              register_identifier Value Bound (Pident id)
              |> Untypeast.lident_of_path |> unwrap_lident |> location_like name;
            uid;
            sort;
            mode;
            type_expr
          }
      | Tpat_fun_layout { id; name; uid; sort; mode; lpoly; env_alloc_mode } ->
        Tpat_fun_layout
          { id;
            name =
              register_identifier Value Bound (Pident id)
              |> Untypeast.lident_of_path |> unwrap_lident |> location_like name;
            uid;
            sort;
            mode;
            lpoly;
            env_alloc_mode
          }
      | Tpat_lazy p -> Tpat_lazy p
      | Tpat_value p -> Tpat_value p
      | Tpat_exception p -> Tpat_exception p
      | Tpat_or (p1, p2, rd) -> Tpat_or (p1, p2, rd)
    in
    let pat_desc = go p.pat_desc in
    let pat_extra =
      List.map
        (fun (extra, loc, attrs) ->
          let extra =
            let open Typedtree in
            match extra with
            | Tpat_open (path, lid, env) ->
              Tpat_open
                ( path,
                  register_identifier Value Free path
                  |> Untypeast.lident_of_path |> location_like lid,
                  env )
            | extra -> extra
          in
          extra, loc, attrs)
        p.pat_extra
    in
    mapper.pat sub { p with pat_desc; pat_extra }
  in
  (* CR translquotes: handle [module_expr], [module_type], subsuming [Ttyp_package] *)
  { mapper with expr; pat; typ }, bound, free

let untype_til_unquote (mapper : Untypeast.mapper) =
  let unquotes = ref [] in
  let expr sub
      ({ exp_desc;
         exp_extra;
         exp_loc;
         exp_attributes;
         exp_type = _;
         exp_env = _
       } as e :
        Typedtree.expression) =
    match exp_desc with
    | Texp_unquote e ->
      let pexpr : Parsetree.expression =
        { pexp_desc = Pexp_unreachable;
          pexp_loc = exp_loc;
          pexp_loc_stack = [];
          pexp_attributes = mapper.attributes sub exp_attributes
        }
      in
      unquotes := (pexpr, e) :: !unquotes;
      List.fold_right (Untypeast.exp_extra sub) exp_extra pexpr
    | _ -> mapper.expr sub e
  in
  let typ sub (t : Typedtree.core_type) =
    match t.ctyp_desc with
    | Ttyp_splice _ ->
      mapper.typ sub { t with ctyp_desc = Ttyp_var (None, None) }
    | _ -> mapper.typ sub t
  in
  { mapper with expr; typ }, unquotes

module LambdaHelpers = struct
  open Lambda

  let int x = Lconst (Const_base (Const_int x))

  let string ~loc x = Lconst (Const_base (Const_string (x, loc, None)))

  let block ~loc tag fields =
    match fields with
    | [] -> int tag
    | _ ->
      Lprim (Pmakeblock (tag, Immutable, All_value, alloc_heap), fields, loc)

  let apply ~loc f args =
    Lapply
      { ap_func = f;
        ap_args = args;
        ap_probe = None;
        ap_yielding = Unyielding;
        ap_loc = loc;
        ap_result_layout =
          Pvalue { raw_kind = Pgenval; nullable = Non_nullable };
        ap_region_close = Rc_normal;
        ap_mode = not_alloc_stack;
        ap_tailcall = Default_tailcall;
        ap_inlined = Default_inlined;
        ap_specialised = Default_specialise
      }

  let prim_fresh_oo_id =
    Pccall
      (simple_prim_on_values ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

  let stdlib_value name =
    lazy
      (let env = Lazy.force Env.initial in
       let lid =
         Longident.Ldot
           (Location.mknoloc (Longident.Lident "Stdlib"), Location.mknoloc name)
       in
       match Env.find_value_by_name_lazy lid env with
       | path, _ -> transl_value_path Loc_unknown env path
       | exception Not_found ->
         Misc.fatal_errorf "Translquotes: Stdlib.%s is unavailable" name)

  let string_of_int_fn = stdlib_value "string_of_int"

  let string_concat_fn = stdlib_value "^"

  let bind id def body =
    Llet (Strict, layout_any_value, id, debug_uid_none, def, body)
end

module LambdaGensym = struct
  open Lambda
  open LambdaHelpers
  open Debuginfo.Scoped_location

  let get ~loc base =
    let fresh = Lprim (prim_fresh_oo_id, [lambda_unit], loc) in
    let fresh_str = apply ~loc (Lazy.force string_of_int_fn) [fresh] in
    apply ~loc
      (Lazy.force string_concat_fn)
      [string ~loc:(to_location loc) (base ^ "|Translquotes|"); fresh_str]

  let with_bind ~loc id base body = bind id (get ~loc base) body
end

let rec lambda_of_obj ~scopes ~loc ~substitute_strings ~substitute_blocks
    (obj : Obj.t) : Lambda.lambda =
  (* The location information in the generated [Lambda] is imprecise *)
  let open LambdaHelpers in
  match Obj.tag obj with
  | tag when tag = Obj.int_tag -> int (Obj.obj obj)
  | tag when tag = Obj.string_tag ->
    let str : string = Obj.obj obj in
    begin match Hashtbl.find_opt substitute_strings str with
    | Some ident -> Lvar (Stamped_ident.to_ident ident)
    | None ->
      if String.ends_with ~suffix:stamped_suffix str
      then Misc.fatal_errorf "Translquotes: free name without stamp %s" str
      else string ~loc (Obj.obj obj)
    end
  | tag when tag < Obj.no_scan_tag ->
    begin match List.assq_opt (Obj.obj obj) substitute_blocks with
    | Some lambda -> lambda
    | None ->
      block
        ~loc:(Debuginfo.Scoped_location.of_location ~scopes loc)
        (Obj.tag obj)
        (List.init (Obj.size obj) (fun i ->
             Obj.field obj i
             |> lambda_of_obj ~scopes ~loc ~substitute_strings
                  ~substitute_blocks))
    end
  | _ -> Misc.fatal_error "Translquotes: unexpected tag in serialized object"

let transl_quote ~(scopes : Debuginfo.Scoped_location.scopes)
    ~(loc : Location.t) ~(transl : Typedtree.expression -> Lambda.lambda)
    (expr : Typedtree.expression) : Lambda.lambda =
  let mapper = Tast_mapper.default in
  let mapper, bound, free = elaborate_identifiers expr.exp_env mapper in
  let mapper = Type_inspection.elaborate_type_inspections mapper in
  let mapper = map_til_unquote mapper in
  let expr = mapper.expr mapper expr in
  let mapper, unquotes = untype_til_unquote Untypeast.default_mapper in
  let expr = Untypeast.untype_expression ~mapper expr in
  let bind_strings =
    Hashtbl.fold
      (fun id () acc ->
        LambdaGensym.with_bind
          ~loc:(Debuginfo.Scoped_location.of_location ~scopes loc)
          (Stamped_ident.to_ident id)
          (Stamped_ident.to_base id) acc)
      bound
  in
  lambda_of_obj ~scopes ~loc ~substitute_strings:free
    ~substitute_blocks:
      (List.map (fun (key, unquoted) -> key, transl unquoted) !unquotes)
    (Obj.repr expr)
  |> bind_strings
