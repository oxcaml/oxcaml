(******************************************************************************
 *                                  OxCaml                                    *
 *                      Andrej Ivaskovic, Jane Street                         *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

(* A quotation [<[ e ]>] evaluates, at run time, to a value of type
   [Parsetree.expression] representing [e]. We build that value in stages, each
   a simple traversal:

   1. two pre-passes over the [Typedtree]: [elaborate_inspections] turns the
      typer's type inspections into ordinary type constraints, and
      [register_required_globals] records the modules [e] refers to so they are
      bundled for evaluation;
   2. [Untypeast] converts [e] to a [Parsetree.expression]. It is the plain
      default traversal, save for [Untypeast.Unquote]: each unquote [$e'] is
      replaced by a placeholder while [transl e'] yields the lambda computing
      the inserted fragment;
   3. [hygiene] freshens every bound variable over the resulting Parsetree, so
      repeated splicing stays hygienic;
   4. a generic serializer emits lambda rebuilding the [Parsetree.expression] at
      run time, substituting the unquote fragments and the freshened variables.

   The serializer never mentions the [Parsetree] constructors: it reads the
   block tags directly from the compile-time value, so it stays in sync with
   [Parsetree] automatically. This relies on the run-time [Parsetree] (the copy
   linked into the standard library) having the same layout as the compiler's,
   which holds because both come from the same source. *)

open Lambda
open Debuginfo.Scoped_location

(* --- Small lambda builders --------------------------------------------- *)

let value_layout = Pvalue { raw_kind = Pgenval; nullable = Non_nullable }

let int n = Lconst (Const_base (Const_int n))

let string ~loc s =
  Lconst (Const_base (Const_string (s, to_location loc, None)))

(* An immutable, all-value heap block, i.e. the run-time representation of a
   record / tuple / non-constant constructor with [size] value fields. *)
let block ~loc tag fields =
  match fields with
  | [] -> int tag (* a constant constructor is an immediate *)
  | _ -> Lprim (Pmakeblock (tag, Immutable, All_value, alloc_heap), fields, loc)

let bind id def body = Llet (Strict, value_layout, id, debug_uid_none, def, body)

let apply ~loc f args =
  Lapply
    { ap_func = f;
      ap_args = args;
      ap_probe = None;
      ap_yielding = Unyielding;
      ap_loc = loc;
      ap_result_layout = value_layout;
      ap_region_close = Rc_normal;
      ap_mode = alloc_heap;
      ap_tailcall = Default_tailcall;
      ap_inlined = Default_inlined;
      ap_specialised = Default_specialise
    }

(* --- Fresh run-time names ---------------------------------------------- *)

(* [caml_fresh_oo_id ()] returns a fresh [int] on each call (the primitive the
   object system uses for object ids); we reuse it as a run-time gensym. *)
let prim_fresh_oo_id =
  Pccall (simple_prim_on_values ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

(* Resolve an ordinary [Stdlib.<name>] value to the lambda that denotes it. *)
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

(* [gensym ~loc base] builds [base ^ "\\" ^ string_of_int (caml_fresh_oo_id ())],
   produced anew every time the quote runs. A backslash cannot occur bare in
   OCaml source (only inside string/char literals), so these stamped names are
   unambiguous and unconfusable with any operator;
   [Pprintast.normalize_quote] turns them back into readable identifiers
   ([base], [base__1], ...) when printing. *)
let gensym ~loc base =
  let fresh = Lprim (prim_fresh_oo_id, [lambda_unit], loc) in
  let fresh_str = apply ~loc (Lazy.force string_of_int_fn) [fresh] in
  apply ~loc (Lazy.force string_concat_fn) [string ~loc (base ^ "\\"); fresh_str]

(* Drop a trailing ["__" ^ digits] so a source name like [x__1] is stamped under
   base [x] and renumbered when printed, matching the usual hygiene
   convention. *)
let strip_stamp s =
  let is_digit c = c >= '0' && c <= '9' in
  let n = String.length s in
  let i = ref n in
  while !i > 0 && is_digit s.[!i - 1] do
    decr i
  done;
  if !i < n && !i >= 2 && s.[!i - 1] = '_' && s.[!i - 2] = '_'
  then String.sub s 0 (!i - 2)
  else s

(* --- Generic serializer ------------------------------------------------ *)

(* Physical-identity association lists, small (one entry per quote-bound
   variable / unquote), scanned with [==]. *)
let assq_phys x l =
  List.find_map (fun (k, v) -> if k == x then Some v else None) l

(* Emit lambda rebuilding the pure data value [v] at run time. [placeholders]
   maps an unquote placeholder (a whole [Parsetree.expression]) to the lambda
   computing its fragment; [markers] maps a quote-bound variable's marker string
   to the lambda variable holding its freshened name. *)
let rec lambda_of_value ~loc ~placeholders ~markers (v : Obj.t) : lambda =
  match assq_phys v placeholders with
  | Some frag -> frag
  | None -> (
    match assq_phys v markers with
    | Some var -> Lvar var
    | None ->
      if Obj.is_int v
      then int (Obj.obj v : int)
      else
        let tag = Obj.tag v in
        if tag = Obj.string_tag
        then string ~loc (Obj.obj v : string)
        else if tag < Obj.no_scan_tag
        then
          block ~loc tag
            (List.init (Obj.size v) (fun i ->
                 lambda_of_value ~loc ~placeholders ~markers (Obj.field v i)))
        else
          (* Parsetree is pure boxed data (no closures, floats or custom
             blocks), so any other tag indicates a bug. *)
          Misc.fatal_errorf "Translquotes: cannot serialize value with tag %d"
            tag)

(* --- Path helpers ------------------------------------------------------- *)

(* The module a path lives in, if any: [M.N.x] has prefix [M.N], a local
   identifier has none. *)
let rec module_prefix = function
  | Path.Pdot (p, _) -> Some p
  | Path.Pextra_ty (p, _) -> module_prefix p
  | Path.Pident _ | Path.Papply _ -> None

(* --- Type annotations for type inspections ----------------------------- *)

(* The typer inserts [type_inspection]s where it had to look at a type to
   resolve inference (label disambiguation, a polymorphic parameter, a package
   type). A quote elaborates them back to explicit type constraints so the
   re-parsed/evaluated code type-checks the same way; otherwise, for example, a
   polymorphic record field or a first-class module's signature could not be
   re-inferred. [type_for_annotation] reconstructs a syntactic [core_type]
   approximating a [type_expr]; it (and [assert_no_jkinds]) are recovered from
   the previous implementation. *)
module Type_annotation = struct
  open Asttypes
  open Typedtree
  open Types

  let mkloc = Location.mkloc

  let assert_no_jkinds jkind =
    Option.iter
      (fun ({ pjka_loc; pjka_desc } : Parsetree.jkind_annotation) ->
        (* Naively check if the jkind annotation is trivial *)
        match pjka_desc with
        | Pjk_abbreviation { loc = _; txt = Longident.Lident "value" } -> ()
        | _ ->
          Misc.fatal_errorf
            "Translquotes [at %a]: no support for jkind annotations in this \
             position."
            Location.print_loc pjka_loc)
      jkind

  (* Approximate the [core_type] for type annotation from a given [type_expr].
     Used for annotating the results of type inspections in quotes. *)
  let type_for_annotation ~env ~loc typ =
    let unwrap_univar ty =
      match get_desc ty with
      | Tunivar { name = Some name; jkind } ->
        assert_no_jkinds jkind.annotation;
        Some (name, jkind.annotation)
      | Tunivar { name = None; jkind = _ } -> None
      | _ ->
        Misc.fatal_errorf
          "Translquotes [at %a]:@ A named universal type variable@ was \
           expected to appear@ within this type"
          Location.print_loc_in_lowercase loc
    in
    let aliasable ty =
      match get_desc ty with Tvar _ | Tunivar _ -> false | _ -> true
    in
    let rec go aliased ty =
      let ctyp_desc =
        if aliasable ty && List.memq ty aliased
        then Ttyp_var (None, None)
        else
          let go = go (ty :: aliased) in
          match get_desc ty with
          | Tvar { name = _; jkind } | Tof_kind jkind ->
            assert_no_jkinds jkind.annotation;
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
            Misc.fatal_errorf "Translquotes [at %a]: no support for Trepr"
              Location.print_loc_in_lowercase loc
          | Ttuple tyl -> Ttyp_tuple (List.map (fun (l, ty') -> l, go ty') tyl)
          | Tunboxed_tuple tyl ->
            Ttyp_unboxed_tuple (List.map (fun (l, ty') -> l, go ty') tyl)
          | Tconstr (p, tyl, _) ->
            Ttyp_constr
              (p, mkloc (Untypeast.lident_of_path p) loc, List.map go tyl)
          | Tmod _ -> Misc.fatal_errorf "Translquotes: unexpected Tmod"
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
                  { fields;
                    name = _;
                    closed;
                    present = _;
                    all_present = _;
                    tags
                  } =
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
            Misc.fatal_errorf
              "Translquotes [at %a]:@ Splices cannot appear in type \
               annotations inserted in quotations@ for higher-rank or package \
               types."
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
              "Translquotes [at %a]:@ Unexpected type expression@ in a quoted \
               higher-rank function type"
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

  (* Elaborate a type inspection into the [core_type] constraint that recovers
     the inspected type. [None] leaves the term unconstrained. *)
  let of_inspection : type k.
      env:Env.t -> loc:Location.t -> k type_inspection -> core_type option =
   fun ~env ~loc insp ->
    let newvar () = Ctype.newvar (Jkind.Builtin.any ~why:Dummy_jkind) in
    let mk ctyp_desc =
      { ctyp_desc;
        ctyp_type = newvar ();
        ctyp_env = env;
        ctyp_loc = loc;
        ctyp_attributes = []
      }
    in
    let wildcard () = mk (Ttyp_var (None, None)) in
    match insp with
    | Label_disambiguation Unambiguous -> None
    | Label_disambiguation (Ambiguous { path; arity }) ->
      let args = List.init arity (fun _ -> wildcard ()) in
      Some
        (mk
           (Ttyp_constr (path, mkloc (Untypeast.lident_of_path path) loc, args)))
    | Polymorphic_parameter (Param ty) ->
      Some (type_for_annotation ~env ~loc ty)
    | Polymorphic_parameter (Method (met, ty)) ->
      let met_cty = type_for_annotation ~env ~loc ty in
      let met_field =
        { of_desc = OTtag (met, met_cty); of_loc = loc; of_attributes = [] }
      in
      Some (mk (Ttyp_object ([met_field], Open)))
    | Polymorphic_parameter (Arrow params) ->
      Some
        (List.fold_right
           (fun (arg_lbl, sch) spine ->
             let dom =
               match sch with
               | Some sch -> type_for_annotation ~env ~loc sch
               | None -> wildcard ()
             in
             mk
               (Ttyp_arrow
                  ( arg_lbl,
                    dom,
                    Typemode.transl_alloc_mode [],
                    spine,
                    Typemode.transl_alloc_mode [] )))
           params (wildcard ()))
    | Module_pack pty -> Some (type_for_annotation ~env ~loc pty)
end

(* Replace every type inspection with the type constraint it elaborates to,
   before untyping (Untypeast itself just drops inspections). Runs over the
   whole Typedtree with [Tast_mapper]; a splice-bearing type is dropped. *)
let elaborate_inspections (exp : Typedtree.expression) : Typedtree.expression =
  let open Typedtree in
  let modes = Typemode.transl_alloc_mode [] in
  let elaborate = Type_annotation.of_inspection in
  let mapper =
    { Tast_mapper.default with
      expr =
        (fun sub e ->
          let exp_extra =
            List.filter_map
              (fun (x, loc, attrs) ->
                match x with
                | Texp_inspected_type insp ->
                  Option.map
                    (fun cty -> Texp_constraint cty, loc, attrs)
                    (elaborate ~env:e.exp_env ~loc:e.exp_loc insp)
                | _ -> Some (x, loc, attrs))
              e.exp_extra
          in
          Tast_mapper.default.expr sub { e with exp_extra });
      pat =
        (fun (type k) sub (p : k general_pattern) ->
          let pat_extra =
            List.filter_map
              (fun (x, loc, attrs) ->
                match x with
                | Tpat_inspected_type insp ->
                  Option.map
                    (fun cty -> Tpat_constraint (Some cty, modes), loc, attrs)
                    (elaborate ~env:p.pat_env ~loc:p.pat_loc insp)
                | _ -> Some (x, loc, attrs))
              p.pat_extra
          in
          Tast_mapper.default.pat sub { p with pat_extra })
    }
  in
  mapper.expr mapper exp

(* Register the module that each identifier the quote refers to lives in, so
   its [.cmi]/[.cmx] are bundled and available when the quote is evaluated at
   run time. A read-only pass over the Typedtree (where paths are resolved). *)
let register_required_globals (exp : Typedtree.expression) =
  let open Typedtree in
  let require env path =
    match module_prefix path with
    | Some p -> Env.add_required_global_for_quote p env
    | None -> ()
  in
  let iterator =
    { Tast_iterator.default_iterator with
      expr =
        (fun sub e ->
          (match e.exp_desc with
          | Texp_ident { path; _ } -> require e.exp_env path
          | Texp_construct (_, cstr, _, _, _) ->
            require e.exp_env (Data_types.cstr_res_type_path cstr)
          | Texp_field { label; _ } | Texp_setfield { label; _ } ->
            require e.exp_env (Data_types.lbl_res_type_path label)
          | Texp_record { fields; _ } ->
            Array.iter
              (fun (label, _, _) ->
                require e.exp_env (Data_types.lbl_res_type_path label))
              fields
          | _ -> ());
          Tast_iterator.default_iterator.expr sub e);
      typ =
        (fun sub t ->
          (match t.ctyp_desc with
          | Ttyp_constr (path, _, _) -> require t.ctyp_env path
          | Ttyp_package pack -> require t.ctyp_env pack.tpt_path
          | _ -> ());
          Tast_iterator.default_iterator.typ sub t);
      pat =
        (fun (type k) sub (p : k general_pattern) ->
          (match p.pat_desc with
          | Tpat_construct (_, cstr, _, _, _) ->
            require p.pat_env (Data_types.cstr_res_type_path cstr)
          | Tpat_record (fields, _, _, _) ->
            List.iter
              (fun (_, label, _) ->
                require p.pat_env (Data_types.lbl_res_type_path label))
              fields
          | _ -> ());
          Tast_iterator.default_iterator.pat sub p)
    }
  in
  iterator.expr iterator exp

(* Resolve the identifiers whose meaning depends on the enclosing scope so that
   the quote is self-contained: record labels and constructors are qualified
   with the module of their type, and type constructors and package types get
   their fully resolved (prettified) path. A pass over the Typedtree, where the
   resolved paths are available. Value identifiers are left as source names:
   local ones are freshened by [hygiene], free ones resolve the same way at the
   splice site. *)
let resolve_paths (exp : Typedtree.expression) : Typedtree.expression =
  let open Typedtree in
  let prettify env lid =
    Out_type.rewrite_double_underscore_longidents env lid
  in
  let last (lid : Longident.t) =
    match lid with
    | Longident.Lident s -> s
    | Longident.Ldot (_, s) -> s.txt
    | Longident.Lapply _ -> assert false
  in
  (* Qualify a label/constructor with the module of the type [tp] it belongs
     to; a local type (no module prefix) leaves it as is. *)
  let qualify env tp (lid : Longident.t Location.loc) =
    match module_prefix tp with
    | None -> lid
    | Some mp ->
      let m = prettify env (Untypeast.lident_of_path mp) in
      { lid with
        txt =
          Longident.Ldot (Location.mknoloc m, Location.mknoloc (last lid.txt))
      }
  in
  let resolve env path (lid : Longident.t Location.loc) =
    { lid with txt = prettify env (Untypeast.lident_of_path path) }
  in
  let mapper =
    { Tast_mapper.default with
      expr =
        (fun sub e ->
          let exp_desc =
            match e.exp_desc with
            | Texp_construct (lid, cstr, a, b, c) ->
              let lid =
                qualify e.exp_env (Data_types.cstr_res_type_path cstr) lid
              in
              Texp_construct (lid, cstr, a, b, c)
            | Texp_field r ->
              let lid =
                qualify e.exp_env (Data_types.lbl_res_type_path r.label) r.lid
              in
              Texp_field { r with lid }
            | Texp_setfield r ->
              let lid =
                qualify e.exp_env (Data_types.lbl_res_type_path r.label) r.lid
              in
              Texp_setfield { r with lid }
            | Texp_record r ->
              let fields =
                Array.map
                  (fun (label, sort, def) ->
                    match def with
                    | Overridden (lid, ex) ->
                      let lid =
                        qualify e.exp_env
                          (Data_types.lbl_res_type_path label)
                          lid
                      in
                      label, sort, Overridden (lid, ex)
                    | Kept _ -> label, sort, def)
                  r.fields
              in
              Texp_record { r with fields }
            | d -> d
          in
          Tast_mapper.default.expr sub { e with exp_desc });
      typ =
        (fun sub t ->
          let ctyp_desc =
            match t.ctyp_desc with
            | Ttyp_constr (path, lid, args) ->
              Ttyp_constr (path, resolve t.ctyp_env path lid, args)
            | Ttyp_package pack ->
              Ttyp_package
                { pack with
                  tpt_txt = resolve t.ctyp_env pack.tpt_path pack.tpt_txt
                }
            | d -> d
          in
          Tast_mapper.default.typ sub { t with ctyp_desc });
      pat =
        (fun (type k) sub (p : k general_pattern) : k general_pattern ->
          let p : k general_pattern =
            match p.pat_desc with
            | Tpat_construct (lid, cstr, a, b, c) ->
              let lid =
                qualify p.pat_env (Data_types.cstr_res_type_path cstr) lid
              in
              { p with pat_desc = Tpat_construct (lid, cstr, a, b, c) }
            | Tpat_record (fields, a, b, c) ->
              let fields =
                List.map
                  (fun (lid, label, pp) ->
                    let lid =
                      qualify p.pat_env (Data_types.lbl_res_type_path label) lid
                    in
                    lid, label, pp)
                  fields
              in
              { p with pat_desc = Tpat_record (fields, a, b, c) }
            | _ -> p
          in
          Tast_mapper.default.pat sub p)
    }
  in
  mapper.expr mapper exp

(* --- Hygiene (post-untyping) ------------------------------------------- *)

open Parsetree
open Location

(* A lexical scope, one map [source name -> marker] per namespace. A [marker]
   is a physically-unique string carrying the source name; the serializer later
   replaces it with the run-time gensym held by its variable. *)
type scope =
  { values : (string * string) list;
    types : (string * string) list
  }

let empty_scope = { values = []; types = [] }

(* One namespace of a [scope], as a first-class accessor pair, so that [intro]
   and [use] below can be written once and applied to either. *)
type namespace =
  { get : scope -> (string * string) list;
    set : scope -> (string * string) list -> scope
  }

let values =
  { get = (fun sc -> sc.values); set = (fun sc v -> { sc with values = v }) }

let types =
  { get = (fun sc -> sc.types); set = (fun sc v -> { sc with types = v }) }

let map_txt f (x : 'a Location.loc) = { x with txt = f x.txt }

(* Freshen every variable bound inside the quote so repeated splicing stays
   hygienic. Returns the rewritten expression, the [markers] the serializer
   substitutes (a marker string -> its run-time variable) and the [prelude]
   bindings (a variable and the base name to [gensym] for it). *)
let hygiene (root : expression) =
  let markers = ref [] in
  let prelude = ref [] in
  let fresh base =
    (* A genuinely fresh copy: [String.sub s 0 (length s)] returns [s] itself,
       and punned records share one string between a field label and its binder,
       so a non-unique marker would tag the (never-freshened) label too. *)
    let marker = Bytes.to_string (Bytes.of_string base) in
    let var = Ident.create_local base in
    markers := (Obj.repr marker, var) :: !markers;
    prelude := (var, base) :: !prelude;
    marker
  in
  (* Bind [name] in namespace [ns] to a fresh marker, returning both. *)
  let intro ns sc name =
    let marker = fresh name in
    marker, ns.set sc ((name, marker) :: ns.get sc)
  in
  (* Resolve a use of [name]; a name not bound in the quote is left alone. *)
  let use ns sc name =
    Option.value (List.assoc_opt name (ns.get sc)) ~default:name
  in
  (* Rewrite a used value/type longident: a bare [Lident] that is in scope
     becomes its marker; everything else (qualified or free) is left alone. *)
  let value_lid sc lid =
    match lid with
    | Longident.Lident s -> Longident.Lident (use values sc s)
    | _ -> lid
  in
  let rec expr sc e =
    match e.pexp_desc with
    | Pexp_hole -> e (* preserve unquote placeholders (matched by identity) *)
    | _ -> { e with pexp_desc = edesc sc e.pexp_desc }
  and edesc sc d =
    match d with
    | Pexp_ident lid -> Pexp_ident (map_txt (value_lid sc) lid)
    | Pexp_constant _ | Pexp_unboxed_unit | Pexp_unboxed_bool _ | Pexp_new _
    | Pexp_extension _ | Pexp_unreachable | Pexp_hole | Pexp_pack _
    | Pexp_object _ | Pexp_idx _ ->
      d
    | Pexp_let (m, r, vbs, body) ->
      (* For [let rec], the bindings see their own binders. *)
      let sc_binders, vbs = List.fold_left_map value_binding sc vbs in
      let sc_rhs =
        match r with Recursive -> sc_binders | Nonrecursive -> sc
      in
      let vbs = List.map (value_binding_rhs sc_rhs) vbs in
      Pexp_let (m, r, vbs, expr sc_binders body)
    | Pexp_function (ps, c, b) ->
      let sc, ps = List.fold_left_map param sc ps in
      Pexp_function (ps, fun_constraint sc c, fun_body sc b)
    | Pexp_apply (f, args) ->
      Pexp_apply (expr sc f, List.map (fun (l, a) -> l, expr sc a) args)
    | Pexp_match (e, cs) -> Pexp_match (expr sc e, List.map (case sc) cs)
    | Pexp_try (e, cs) -> Pexp_try (expr sc e, List.map (case sc) cs)
    | Pexp_tuple l -> Pexp_tuple (List.map (fun (s, e) -> s, expr sc e) l)
    | Pexp_unboxed_tuple l ->
      Pexp_unboxed_tuple (List.map (fun (s, e) -> s, expr sc e) l)
    | Pexp_construct (lid, eo) -> Pexp_construct (lid, Option.map (expr sc) eo)
    | Pexp_variant (l, eo) -> Pexp_variant (l, Option.map (expr sc) eo)
    | Pexp_record (fs, eo) ->
      Pexp_record
        (List.map (fun (lid, e) -> lid, expr sc e) fs, Option.map (expr sc) eo)
    | Pexp_record_unboxed_product (fs, eo) ->
      Pexp_record_unboxed_product
        (List.map (fun (lid, e) -> lid, expr sc e) fs, Option.map (expr sc) eo)
    | Pexp_field (e, lid) -> Pexp_field (expr sc e, lid)
    | Pexp_unboxed_field (e, lid) -> Pexp_unboxed_field (expr sc e, lid)
    | Pexp_setfield (e1, lid, e2) -> Pexp_setfield (expr sc e1, lid, expr sc e2)
    | Pexp_array (m, l) -> Pexp_array (m, List.map (expr sc) l)
    | Pexp_ifthenelse (c, t, eo) ->
      Pexp_ifthenelse (expr sc c, expr sc t, Option.map (expr sc) eo)
    | Pexp_sequence (e1, e2) -> Pexp_sequence (expr sc e1, expr sc e2)
    | Pexp_while (e1, e2) -> Pexp_while (expr sc e1, expr sc e2)
    | Pexp_for (p, e1, e2, dir, e3) ->
      let e1 = expr sc e1 and e2 = expr sc e2 in
      let sc, p = pat sc p in
      Pexp_for (p, e1, e2, dir, expr sc e3)
    | Pexp_constraint (e, cto, m) ->
      Pexp_constraint (expr sc e, Option.map (typ sc) cto, m)
    | Pexp_coerce (e, cto, ct) ->
      Pexp_coerce (expr sc e, Option.map (typ sc) cto, typ sc ct)
    | Pexp_send (e, l) -> Pexp_send (expr sc e, l)
    | Pexp_setvar (l, e) -> Pexp_setvar (l, expr sc e)
    | Pexp_override l -> Pexp_override (List.map (fun (n, e) -> n, expr sc e) l)
    | Pexp_letmodule (n, me, e) ->
      (* Module binders are left verbatim: the module language is out of scope
         for hygiene, and freshening only the binder (without rewriting every
         [M.x] use across the value/type/module namespaces) would be
         inconsistent. *)
      Pexp_letmodule (n, me, expr sc e)
    | Pexp_letexception (ec, e) -> Pexp_letexception (ec, expr sc e)
    | Pexp_assert e -> Pexp_assert (expr sc e)
    | Pexp_lazy e -> Pexp_lazy (expr sc e)
    | Pexp_poly (e, cto) -> Pexp_poly (expr sc e, Option.map (typ sc) cto)
    | Pexp_newtype (n, j, e) -> Pexp_newtype (n, j, expr sc e)
    | Pexp_open (od, e) -> Pexp_open (od, expr sc e)
    | Pexp_letop lop ->
      let sc, let_ = binding_op sc lop.let_ in
      let sc, ands = List.fold_left_map binding_op sc lop.ands in
      Pexp_letop { let_; ands; body = expr sc lop.body }
    | Pexp_stack e -> Pexp_stack (expr sc e)
    | Pexp_overwrite (e1, e2) -> Pexp_overwrite (expr sc e1, expr sc e2)
    | Pexp_quote e -> Pexp_quote (expr sc e)
    | Pexp_splice e -> Pexp_splice (expr sc e)
    | Pexp_borrow e -> Pexp_borrow (expr sc e)
    | Pexp_comprehension c -> Pexp_comprehension (comprehension sc c)
  and param sc p =
    match p.pparam_desc with
    | Pparam_val (l, eo, p') ->
      let eo = Option.map (expr sc) eo in
      let sc, p' = pat sc p' in
      sc, { p with pparam_desc = Pparam_val (l, eo, p') }
    | Pparam_newtype _ -> sc, p
  and fun_body sc = function
    | Pfunction_body e -> Pfunction_body (expr sc e)
    | Pfunction_cases (cs, loc, attrs) ->
      Pfunction_cases (List.map (case sc) cs, loc, attrs)
  and fun_constraint sc c =
    { c with
      ret_type_constraint =
        Option.map (type_constraint sc) c.ret_type_constraint
    }
  and type_constraint sc = function
    | Pconstraint ct -> Pconstraint (typ sc ct)
    | Pcoerce (cto, ct) -> Pcoerce (Option.map (typ sc) cto, typ sc ct)
  (* Return the binding with its pattern freshened (introducing binders into the
     accumulated scope); its RHS is processed separately, once the recursion
     flavour is known. *)
  and value_binding sc vb =
    let sc, pvb_pat = pat sc vb.pvb_pat in
    let pvb_constraint = Option.map (value_constraint sc) vb.pvb_constraint in
    sc, { vb with pvb_pat; pvb_constraint }
  and value_binding_rhs sc vb = { vb with pvb_expr = expr sc vb.pvb_expr }
  and value_constraint sc = function
    | Pvc_constraint { locally_abstract_univars; typ = t } ->
      Pvc_constraint { locally_abstract_univars; typ = typ sc t }
    | Pvc_coercion { ground; coercion } ->
      Pvc_coercion
        { ground = Option.map (typ sc) ground; coercion = typ sc coercion }
  and binding_op sc bop =
    let e = expr sc bop.pbop_exp in
    let sc, pbop_pat = pat sc bop.pbop_pat in
    sc, { bop with pbop_pat; pbop_exp = e }
  and case sc c =
    let sc, pc_lhs = pat sc c.pc_lhs in
    { pc_lhs;
      pc_guard = Option.map (expr sc) c.pc_guard;
      pc_rhs = expr sc c.pc_rhs
    }
  (* Patterns introduce binders; return the extended scope. *)
  and pat sc p =
    let sc, ppat_desc = pdesc sc p.ppat_desc in
    sc, { p with ppat_desc }
  and pdesc sc d =
    match d with
    | Ppat_any | Ppat_constant _ | Ppat_interval _ | Ppat_unboxed_unit
    | Ppat_unboxed_bool _ | Ppat_type _ | Ppat_extension _ ->
      sc, d
    | Ppat_var s ->
      let m, sc = intro values sc s.txt in
      sc, Ppat_var { s with txt = m }
    | Ppat_alias (p, s) ->
      let sc, p = pat sc p in
      let m, sc = intro values sc s.txt in
      sc, Ppat_alias (p, { s with txt = m })
    | Ppat_unpack s ->
      (* See [Pexp_letmodule]: module binders are not freshened. *)
      sc, Ppat_unpack s
    | Ppat_tuple (l, c) ->
      let sc, l =
        List.fold_left_map
          (fun sc (s, p) ->
            let sc, p = pat sc p in
            sc, (s, p))
          sc l
      in
      sc, Ppat_tuple (l, c)
    | Ppat_unboxed_tuple (l, c) ->
      let sc, l =
        List.fold_left_map
          (fun sc (s, p) ->
            let sc, p = pat sc p in
            sc, (s, p))
          sc l
      in
      sc, Ppat_unboxed_tuple (l, c)
    | Ppat_construct (lid, arg) ->
      let sc, arg =
        match arg with
        | None -> sc, None
        | Some (vs, p) ->
          let sc, p = pat sc p in
          sc, Some (vs, p)
      in
      sc, Ppat_construct (lid, arg)
    | Ppat_variant (l, po) ->
      let sc, po =
        match po with
        | None -> sc, None
        | Some p ->
          let sc, p = pat sc p in
          sc, Some p
      in
      sc, Ppat_variant (l, po)
    | Ppat_record (fs, c) ->
      let sc, fs =
        List.fold_left_map
          (fun sc (lid, p) ->
            let sc, p = pat sc p in
            sc, (lid, p))
          sc fs
      in
      sc, Ppat_record (fs, c)
    | Ppat_record_unboxed_product (fs, c) ->
      let sc, fs =
        List.fold_left_map
          (fun sc (lid, p) ->
            let sc, p = pat sc p in
            sc, (lid, p))
          sc fs
      in
      sc, Ppat_record_unboxed_product (fs, c)
    | Ppat_array (m, l) ->
      let sc, l = List.fold_left_map pat sc l in
      sc, Ppat_array (m, l)
    | Ppat_or (p1, p2) ->
      let sc, p1 = pat sc p1 in
      let sc, p2 = pat sc p2 in
      sc, Ppat_or (p1, p2)
    | Ppat_constraint (p, cto, m) ->
      let sc, p = pat sc p in
      sc, Ppat_constraint (p, Option.map (typ sc) cto, m)
    | Ppat_lazy p ->
      let sc, p = pat sc p in
      sc, Ppat_lazy p
    | Ppat_exception p ->
      let sc, p = pat sc p in
      sc, Ppat_exception p
    | Ppat_effect (p1, p2) ->
      let sc, p1 = pat sc p1 in
      let sc, p2 = pat sc p2 in
      sc, Ppat_effect (p1, p2)
    | Ppat_open (lid, p) ->
      let sc, p = pat sc p in
      sc, Ppat_open (lid, p)
  and typ sc t = { t with ptyp_desc = tdesc sc t.ptyp_desc }
  and tdesc sc d =
    match d with
    | Ptyp_any _ | Ptyp_of_kind _ | Ptyp_extension _ -> d
    | Ptyp_var (s, j) -> Ptyp_var (use types sc s, j)
    | Ptyp_package pt ->
      (* The module path is not freshened (module binders never are), but the
         [with type t = ...] constraints can mention a freshened variable. *)
      Ptyp_package
        { pt with
          ppt_cstrs = List.map (fun (l, t) -> l, typ sc t) pt.ppt_cstrs
        }
    | Ptyp_constr (lid, args) -> Ptyp_constr (lid, List.map (typ sc) args)
    | Ptyp_class (lid, args) -> Ptyp_class (lid, List.map (typ sc) args)
    | Ptyp_repr (l, t) -> Ptyp_repr (l, typ sc t)
    | Ptyp_newlayout (l, t) -> Ptyp_newlayout (l, typ sc t)
    | Ptyp_arrow (l, t1, t2, m1, m2) ->
      Ptyp_arrow (l, typ sc t1, typ sc t2, m1, m2)
    | Ptyp_tuple l -> Ptyp_tuple (List.map (fun (s, t) -> s, typ sc t) l)
    | Ptyp_unboxed_tuple l ->
      Ptyp_unboxed_tuple (List.map (fun (s, t) -> s, typ sc t) l)
    | Ptyp_object (fs, c) -> Ptyp_object (List.map (object_field sc) fs, c)
    | Ptyp_alias (t, s, j) -> Ptyp_alias (typ sc t, s, j)
    | Ptyp_variant (rs, c, l) -> Ptyp_variant (List.map (row_field sc) rs, c, l)
    | Ptyp_poly (vars, body) ->
      (* The bound type variables are freshened; their uses in [body] resolve
           to the same markers. *)
      let sc, vars =
        List.fold_left_map
          (fun sc (v, j) ->
            let m, sc = intro types sc v.txt in
            sc, ({ v with txt = m }, j))
          sc vars
      in
      Ptyp_poly (vars, typ sc body)
    | Ptyp_open (lid, t) -> Ptyp_open (lid, typ sc t)
    | Ptyp_quote t -> Ptyp_quote (typ sc t)
    | Ptyp_splice t -> Ptyp_splice (typ sc t)
  and object_field sc f =
    match f.pof_desc with
    | Otag (l, t) -> { f with pof_desc = Otag (l, typ sc t) }
    | Oinherit t -> { f with pof_desc = Oinherit (typ sc t) }
  and row_field sc f =
    match f.prf_desc with
    | Rtag (l, b, ts) -> { f with prf_desc = Rtag (l, b, List.map (typ sc) ts) }
    | Rinherit t -> { f with prf_desc = Rinherit (typ sc t) }
  and comprehension sc = function
    | Pcomp_list_comprehension c -> Pcomp_list_comprehension (comp sc c)
    | Pcomp_array_comprehension (m, c) ->
      Pcomp_array_comprehension (m, comp sc c)
  and comp sc c =
    let sc, clauses = List.fold_left_map comp_clause sc c.pcomp_clauses in
    { pcomp_clauses = clauses; pcomp_body = expr sc c.pcomp_body }
  and comp_clause sc = function
    | Pcomp_for bs ->
      let sc, bs = List.fold_left_map comp_binding sc bs in
      sc, Pcomp_for bs
    | Pcomp_when e -> sc, Pcomp_when (expr sc e)
  and comp_binding sc b =
    let it = comp_iterator sc b.pcomp_cb_iterator in
    let sc, p = pat sc b.pcomp_cb_pattern in
    sc, { b with pcomp_cb_iterator = it; pcomp_cb_pattern = p }
  and comp_iterator sc = function
    | Pcomp_range { start; stop; direction } ->
      Pcomp_range { start = expr sc start; stop = expr sc stop; direction }
    | Pcomp_in e -> Pcomp_in (expr sc e)
  in
  let root = expr empty_scope root in
  root, !markers, !prelude

(* --- Entry point ------------------------------------------------------- *)

let transl_quote ~scopes ~loc ~transl exp =
  let loc = of_location ~scopes loc in
  (* Unquotes [$e] become placeholders while untyping; the serializer replaces
     each with the lambda that computes the inserted fragment. *)
  let placeholders = ref [] in
  let unquote e =
    (* Local allocations are not expected to escape from this expression; if
       they did the [ret_mode] of the enclosing function would need updating. *)
    let frag = Lregion (transl e, layout_any_value) in
    let placeholder : Parsetree.expression =
      { pexp_desc = Pexp_hole;
        pexp_loc = Location.none;
        pexp_loc_stack = [];
        pexp_attributes = []
      }
    in
    placeholders := (Obj.repr placeholder, frag) :: !placeholders;
    placeholder
  in
  (* Typedtree pre-passes: turn type inspections into constraints, and register
     the modules the quote refers to so eval can find them. *)
  let exp = elaborate_inspections exp in
  register_required_globals exp;
  let exp = resolve_paths exp in
  (* Untype with the default mapper; only [Unquote] is handled specially, and
     locations are dropped to keep the built value compact. *)
  let mapper =
    { Untypeast.default_mapper with location = (fun _ _ -> Location.none) }
  in
  let parsetree : Parsetree.expression =
    Effect.Deep.try_with
      (fun () -> mapper.expr mapper exp)
      ()
      { effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Untypeast.Unquote e ->
              Some
                (fun (k : (b, Parsetree.expression) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (unquote e))
            | _ -> None)
      }
  in
  (* Freshen bound variables over the resulting Parsetree (hygiene). *)
  let parsetree, markers, prelude = hygiene parsetree in
  let body =
    lambda_of_value ~loc ~placeholders:!placeholders ~markers
      (Obj.repr parsetree)
  in
  (* Bind each freshened variable to a fresh run-time gensym around the quote,
     so every construction of the quote uses distinct names. *)
  List.fold_left
    (fun acc (var, base) -> bind var (gensym ~loc (strip_stamp base)) acc)
    body prelude
