(* Erasure of the erasable OxCaml syntax extensions, as a typed copy of OxCaml's
   parsetree into OCaml 5.4's ([Ast_504], a verbatim copy of OCaml 5.4.1's
   parsetree.mli). Written in the style of ppxlib's astlib migrations: one
   function per type, no catch-all patterns, so that any change to parsetree.mli
   shows up as a non-exhaustive match here.

   The erasure rules follow those of [ocamlformat --erase-jane-syntax] (Jane
   Street's ocamlformat fork): mode, modality and kind annotations are dropped
   (kinds [immediate] and [immediate64] become attributes), [stack_], [exclave_]
   and friends are removed, unboxed types and literals become their boxed
   counterparts, [global_] becomes [@globalized] and [[%call_pos]] becomes an
   optional [Lexing.position]. Constructs that the upstream compiler cannot
   express with the same meaning are rejected with a located error.

   [Location.t], ['a Location.loc] and [Longident.t] have the same definition in
   both compilers and are shared (see longident_504.ml), so they need no
   copying. *)

module From = Parsetree
module To = Ast_504.Parsetree
module Fa = Asttypes
module Ta = Ast_504.Asttypes

let not_erasable ~loc what =
  Location.raise_errorf ~loc
    "%s is not erasable: this file must also compile with the upstream OCaml \
     compiler (see tools/oxpp)"
    what

let is_attr name (attr : From.attribute) = String.equal attr.attr_name.txt name

let is_extension name ((ext_name, _) : From.extension) =
  String.equal ext_name.txt name

let has_modality name (modalities : From.modalities) =
  List.exists
    (fun { Location.txt = From.Modality modality; _ } ->
      String.equal modality name)
    modalities

let attribute_named name : To.attribute =
  { To.attr_name = Location.mknoloc name;
    attr_payload = To.PStr [];
    attr_loc = Location.none
  }

let globalized_attr = attribute_named "globalized"

(* [Stdlib.Lexing.position] and [Stdlib.Lexing.dummy_pos] at [loc]. *)
let lexing_lid ~loc name =
  let ldot path s =
    Longident.Ldot (Location.mkloc path loc, Location.mkloc s loc)
  in
  Location.mkloc (ldot (ldot (Longident.Lident "Stdlib") "Lexing") name) loc

let lexing_position_type ~loc : To.core_type =
  { To.ptyp_desc = To.Ptyp_constr (lexing_lid ~loc "position", []);
    ptyp_loc = loc;
    ptyp_loc_stack = [];
    ptyp_attributes = []
  }

let dummy_pos ~loc : To.expression =
  { To.pexp_desc = To.Pexp_ident (lexing_lid ~loc "dummy_pos");
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = []
  }

let unit_lid ~loc = Location.mkloc (Longident.Lident "()") loc

let bool_lid ~loc b = Location.mkloc (Longident.Lident (string_of_bool b)) loc

(* [float#] is the boxed [float] once erased. *)
let strip_hash s =
  if String.ends_with ~suffix:"#" s
  then Some (String.sub s 0 (String.length s - 1))
  else None

let strip_hash_lid ({ Location.txt; loc } as lid) =
  match txt with
  | Longident.Lident s -> (
    match strip_hash s with
    | Some s -> Location.mkloc (Longident.Lident s) loc
    | None -> lid)
  | Longident.Ldot (path, ({ Location.txt = s; _ } as last)) -> (
    match strip_hash s with
    | Some s ->
      Location.mkloc (Longident.Ldot (path, { last with txt = s })) loc
    | None -> lid)
  | Longident.Lapply _ -> lid

let copy_rec_flag : Fa.rec_flag -> Ta.rec_flag = function
  | Fa.Nonrecursive -> Ta.Nonrecursive
  | Fa.Recursive -> Ta.Recursive

let copy_direction_flag : Fa.direction_flag -> Ta.direction_flag = function
  | Fa.Upto -> Ta.Upto
  | Fa.Downto -> Ta.Downto

let copy_private_flag : Fa.private_flag -> Ta.private_flag = function
  | Fa.Private -> Ta.Private
  | Fa.Public -> Ta.Public

let copy_mutable_flag : Fa.mutable_flag -> Ta.mutable_flag = function
  | Fa.Immutable -> Ta.Immutable
  | Fa.Mutable -> Ta.Mutable

let copy_virtual_flag : Fa.virtual_flag -> Ta.virtual_flag = function
  | Fa.Virtual -> Ta.Virtual
  | Fa.Concrete -> Ta.Concrete

let copy_override_flag : Fa.override_flag -> Ta.override_flag = function
  | Fa.Override -> Ta.Override
  | Fa.Fresh -> Ta.Fresh

let copy_closed_flag : Fa.closed_flag -> Ta.closed_flag = function
  | Fa.Closed -> Ta.Closed
  | Fa.Open -> Ta.Open

let copy_arg_label : Fa.arg_label -> Ta.arg_label = function
  | Fa.Nolabel -> Ta.Nolabel
  | Fa.Labelled s -> Ta.Labelled s
  | Fa.Optional s -> Ta.Optional s

let copy_variance : Fa.variance -> Ta.variance = function
  | Fa.Covariant -> Ta.Covariant
  | Fa.Contravariant -> Ta.Contravariant
  | Fa.NoVariance -> Ta.NoVariance
  | Fa.Bivariant -> Ta.Bivariant

let copy_injectivity : Fa.injectivity -> Ta.injectivity = function
  | Fa.Injective -> Ta.Injective
  | Fa.NoInjectivity -> Ta.NoInjectivity

(* Type variables bound with an optional kind, [(type (a : k))], GADT
   existentials, ['a : k. ...]: the kind is erased. *)
let copy_kinded_var ((var : string Location.loc), _kind) = var

let copy_constant_desc : From.constant_desc -> To.constant_desc = function
  | From.Pconst_integer (s, suffix) -> To.Pconst_integer (s, suffix)
  | From.Pconst_unboxed_integer (s, suffix) -> To.Pconst_integer (s, Some suffix)
  | From.Pconst_char c | From.Pconst_untagged_char c -> To.Pconst_char c
  | From.Pconst_string (s, loc, delim) -> To.Pconst_string (s, loc, delim)
  | From.Pconst_float (s, suffix) | From.Pconst_unboxed_float (s, suffix) ->
    To.Pconst_float (s, suffix)

let copy_constant { From.pconst_desc; pconst_loc } : To.constant =
  { To.pconst_desc = copy_constant_desc pconst_desc; pconst_loc }

let rec copy_attribute { From.attr_name; attr_payload; attr_loc } : To.attribute
    =
  { To.attr_name; attr_payload = copy_payload attr_payload; attr_loc }

(* [[@extension.curry]] marks a parenthesised arrow codomain; it is dropped. *)
and copy_attributes attrs =
  List.filter_map
    (fun attr ->
      if is_attr "extension.curry" attr
      then None
      else Some (copy_attribute attr))
    attrs

and copy_extension ((name, payload) : From.extension) : To.extension =
  name, copy_payload payload

and copy_payload : From.payload -> To.payload = function
  | From.PStr str -> To.PStr (copy_structure str)
  | From.PSig sg -> To.PSig (copy_signature sg)
  | From.PTyp typ -> To.PTyp (copy_core_type typ)
  | From.PPat (pat, guard) ->
    To.PPat (copy_pattern pat, Option.map copy_expression guard)

(* Core language *)

and copy_core_type
    { From.ptyp_desc; ptyp_loc = loc; ptyp_loc_stack; ptyp_attributes } :
    To.core_type =
  let mk ptyp_desc =
    { To.ptyp_desc;
      ptyp_loc = loc;
      ptyp_loc_stack;
      ptyp_attributes = copy_attributes ptyp_attributes
    }
  in
  (* An erased wrapper: keep its attributes on the inner type. *)
  let unwrap (inner : From.core_type) =
    copy_core_type
      { inner with ptyp_attributes = ptyp_attributes @ inner.ptyp_attributes }
  in
  match ptyp_desc with
  | From.Ptyp_any _ -> mk To.Ptyp_any
  | From.Ptyp_var (name, _) -> mk (To.Ptyp_var name)
  | From.Ptyp_arrow
      ( Fa.Labelled label,
        { From.ptyp_desc = From.Ptyp_extension ext; _ },
        ret,
        _,
        _ )
    when is_extension "call_pos" ext ->
    mk
      (To.Ptyp_arrow
         ( Ta.Optional label,
           lexing_position_type ~loc:(fst ext).loc,
           copy_core_type ret ))
  | From.Ptyp_arrow (label, t1, t2, _, _) ->
    mk
      (To.Ptyp_arrow (copy_arg_label label, copy_core_type t1, copy_core_type t2))
  | From.Ptyp_tuple ts | From.Ptyp_unboxed_tuple ts ->
    mk (To.Ptyp_tuple (List.map copy_labeled_type ts))
  | From.Ptyp_constr (lid, args) ->
    mk (To.Ptyp_constr (strip_hash_lid lid, List.map copy_core_type args))
  | From.Ptyp_object (fields, closed) ->
    mk
      (To.Ptyp_object
         (List.map copy_object_field fields, copy_closed_flag closed))
  | From.Ptyp_class (lid, args) ->
    mk (To.Ptyp_class (lid, List.map copy_core_type args))
  | From.Ptyp_alias (inner, Some name, _) ->
    mk (To.Ptyp_alias (copy_core_type inner, name))
  | From.Ptyp_alias (inner, None, _) -> unwrap inner
  | From.Ptyp_variant (fields, closed, labels) ->
    mk
      (To.Ptyp_variant
         (List.map copy_row_field fields, copy_closed_flag closed, labels))
  | From.Ptyp_poly (vars, inner) ->
    mk (To.Ptyp_poly (List.map copy_kinded_var vars, copy_core_type inner))
  | From.Ptyp_newlayout (_, inner) -> unwrap inner
  | From.Ptyp_package pkg -> mk (To.Ptyp_package (copy_package_type pkg))
  | From.Ptyp_open (lid, inner) -> mk (To.Ptyp_open (lid, copy_core_type inner))
  | From.Ptyp_quote _ | From.Ptyp_splice _ ->
    not_erasable ~loc "runtime metaprogramming"
  | From.Ptyp_of_kind _ -> not_erasable ~loc "(type : k)"
  | From.Ptyp_repr _ -> not_erasable ~loc "a representation type"
  | From.Ptyp_extension ext -> mk (To.Ptyp_extension (copy_extension ext))

and copy_labeled_type (label, typ) = label, copy_core_type typ

and copy_param (typ, (variance, injectivity)) =
  copy_core_type typ, (copy_variance variance, copy_injectivity injectivity)

and copy_package_type { From.ppt_path; ppt_cstrs; ppt_loc; ppt_attrs } :
    To.package_type =
  { To.ppt_path;
    ppt_cstrs = List.map (fun (lid, typ) -> lid, copy_core_type typ) ppt_cstrs;
    ppt_loc;
    ppt_attrs = copy_attributes ppt_attrs
  }

and copy_row_field { From.prf_desc; prf_loc; prf_attributes } : To.row_field =
  { To.prf_desc = copy_row_field_desc prf_desc;
    prf_loc;
    prf_attributes = copy_attributes prf_attributes
  }

and copy_row_field_desc : From.row_field_desc -> To.row_field_desc = function
  | From.Rtag (label, constant, types) ->
    To.Rtag (label, constant, List.map copy_core_type types)
  | From.Rinherit typ -> To.Rinherit (copy_core_type typ)

and copy_object_field { From.pof_desc; pof_loc; pof_attributes } :
    To.object_field =
  { To.pof_desc = copy_object_field_desc pof_desc;
    pof_loc;
    pof_attributes = copy_attributes pof_attributes
  }

and copy_object_field_desc : From.object_field_desc -> To.object_field_desc =
  function
  | From.Otag (label, typ) -> To.Otag (label, copy_core_type typ)
  | From.Oinherit typ -> To.Oinherit (copy_core_type typ)

(* Patterns *)

and copy_pattern
    { From.ppat_desc; ppat_loc = loc; ppat_loc_stack; ppat_attributes } :
    To.pattern =
  let mk ppat_desc =
    { To.ppat_desc;
      ppat_loc = loc;
      ppat_loc_stack;
      ppat_attributes = copy_attributes ppat_attributes
    }
  in
  let unwrap (inner : From.pattern) =
    copy_pattern
      { inner with ppat_attributes = ppat_attributes @ inner.ppat_attributes }
  in
  match ppat_desc with
  | From.Ppat_any -> mk To.Ppat_any
  | From.Ppat_var name -> mk (To.Ppat_var name)
  | From.Ppat_alias (pat, name) -> mk (To.Ppat_alias (copy_pattern pat, name))
  | From.Ppat_constant c -> mk (To.Ppat_constant (copy_constant c))
  | From.Ppat_interval (c1, c2) ->
    mk (To.Ppat_interval (copy_constant c1, copy_constant c2))
  | From.Ppat_unboxed_unit -> mk (To.Ppat_construct (unit_lid ~loc, None))
  | From.Ppat_unboxed_bool b -> mk (To.Ppat_construct (bool_lid ~loc b, None))
  | From.Ppat_tuple (pats, closed) | From.Ppat_unboxed_tuple (pats, closed) ->
    mk
      (To.Ppat_tuple
         (List.map copy_labeled_pattern pats, copy_closed_flag closed))
  | From.Ppat_construct (lid, None) -> mk (To.Ppat_construct (lid, None))
  | From.Ppat_construct (lid, Some (vars, pat)) ->
    mk
      (To.Ppat_construct
         (lid, Some (List.map copy_kinded_var vars, copy_pattern pat)))
  | From.Ppat_variant (label, pat) ->
    mk (To.Ppat_variant (label, Option.map copy_pattern pat))
  | From.Ppat_record (fields, closed)
  | From.Ppat_record_unboxed_product (fields, closed) ->
    mk
      (To.Ppat_record
         (List.map copy_field_pattern fields, copy_closed_flag closed))
  | From.Ppat_array (Fa.Mutable, pats) ->
    mk (To.Ppat_array (List.map copy_pattern pats))
  | From.Ppat_array (Fa.Immutable, _) ->
    not_erasable ~loc "an immutable array pattern"
  | From.Ppat_or (p1, p2) -> mk (To.Ppat_or (copy_pattern p1, copy_pattern p2))
  | From.Ppat_constraint (pat, Some typ, _) ->
    mk (To.Ppat_constraint (copy_pattern pat, copy_core_type typ))
  | From.Ppat_constraint (pat, None, _) -> unwrap pat
  | From.Ppat_type lid -> mk (To.Ppat_type lid)
  | From.Ppat_lazy pat -> mk (To.Ppat_lazy (copy_pattern pat))
  | From.Ppat_unpack name -> mk (To.Ppat_unpack name)
  | From.Ppat_exception pat -> mk (To.Ppat_exception (copy_pattern pat))
  | From.Ppat_effect (p1, p2) ->
    mk (To.Ppat_effect (copy_pattern p1, copy_pattern p2))
  | From.Ppat_extension ext -> mk (To.Ppat_extension (copy_extension ext))
  | From.Ppat_open (lid, pat) -> mk (To.Ppat_open (lid, copy_pattern pat))

and copy_labeled_pattern (label, pat) = label, copy_pattern pat

and copy_field_pattern (lid, pat) = lid, copy_pattern pat

(* Expressions *)

and copy_expression
    { From.pexp_desc; pexp_loc = loc; pexp_loc_stack; pexp_attributes } :
    To.expression =
  let mk pexp_desc =
    { To.pexp_desc;
      pexp_loc = loc;
      pexp_loc_stack;
      pexp_attributes = copy_attributes pexp_attributes
    }
  in
  let unwrap (inner : From.expression) =
    copy_expression
      { inner with pexp_attributes = pexp_attributes @ inner.pexp_attributes }
  in
  match pexp_desc with
  | From.Pexp_ident lid -> mk (To.Pexp_ident lid)
  | From.Pexp_constant c -> mk (To.Pexp_constant (copy_constant c))
  | From.Pexp_let (Fa.Immutable, rec_flag, bindings, body) ->
    mk
      (To.Pexp_let
         ( copy_rec_flag rec_flag,
           List.map copy_value_binding bindings,
           copy_expression body ))
  | From.Pexp_let (Fa.Mutable, _, _, _) -> not_erasable ~loc "let mutable"
  | From.Pexp_function (params, constraint_, body) ->
    mk
      (To.Pexp_function
         ( List.map copy_function_param params,
           Option.map copy_type_constraint constraint_.ret_type_constraint,
           copy_function_body body ))
  | From.Pexp_apply
      ({ From.pexp_desc = From.Pexp_extension ext; _ }, [(Fa.Nolabel, inner)])
    when is_extension "extension.exclave" ext ->
    unwrap inner
  | From.Pexp_apply (f, args) ->
    mk (To.Pexp_apply (copy_expression f, List.map copy_argument args))
  | From.Pexp_match (e, cases) ->
    mk (To.Pexp_match (copy_expression e, List.map copy_case cases))
  | From.Pexp_try (e, cases) ->
    mk (To.Pexp_try (copy_expression e, List.map copy_case cases))
  | From.Pexp_unboxed_unit -> mk (To.Pexp_construct (unit_lid ~loc, None))
  | From.Pexp_unboxed_bool b -> mk (To.Pexp_construct (bool_lid ~loc b, None))
  | From.Pexp_tuple es | From.Pexp_unboxed_tuple es ->
    mk (To.Pexp_tuple (List.map copy_labeled_expression es))
  | From.Pexp_construct (lid, arg) ->
    mk (To.Pexp_construct (lid, Option.map copy_expression arg))
  | From.Pexp_variant (label, arg) ->
    mk (To.Pexp_variant (label, Option.map copy_expression arg))
  | From.Pexp_record (fields, base)
  | From.Pexp_record_unboxed_product (fields, base) ->
    mk
      (To.Pexp_record
         (List.map copy_field_expression fields, Option.map copy_expression base))
  | From.Pexp_field (e, lid) | From.Pexp_unboxed_field (e, lid) ->
    mk (To.Pexp_field (copy_expression e, lid))
  | From.Pexp_setfield (e1, lid, e2) ->
    mk (To.Pexp_setfield (copy_expression e1, lid, copy_expression e2))
  | From.Pexp_array (Fa.Mutable, es) ->
    mk (To.Pexp_array (List.map copy_expression es))
  | From.Pexp_array (Fa.Immutable, _) -> not_erasable ~loc "an immutable array"
  | From.Pexp_idx _ -> not_erasable ~loc "a block index"
  | From.Pexp_ifthenelse (cond, e1, e2) ->
    mk
      (To.Pexp_ifthenelse
         ( copy_expression cond,
           copy_expression e1,
           Option.map copy_expression e2 ))
  | From.Pexp_sequence (e1, e2) ->
    mk (To.Pexp_sequence (copy_expression e1, copy_expression e2))
  | From.Pexp_while (cond, body) ->
    mk (To.Pexp_while (copy_expression cond, copy_expression body))
  | From.Pexp_for (pat, start, stop, direction, body) ->
    mk
      (To.Pexp_for
         ( copy_pattern pat,
           copy_expression start,
           copy_expression stop,
           copy_direction_flag direction,
           copy_expression body ))
  | From.Pexp_constraint (e, Some typ, _) ->
    mk (To.Pexp_constraint (copy_expression e, copy_core_type typ))
  | From.Pexp_constraint (e, None, _) -> unwrap e
  | From.Pexp_coerce (e, from, typ) ->
    mk
      (To.Pexp_coerce
         (copy_expression e, Option.map copy_core_type from, copy_core_type typ))
  | From.Pexp_send (e, meth) -> mk (To.Pexp_send (copy_expression e, meth))
  | From.Pexp_new lid -> mk (To.Pexp_new lid)
  | From.Pexp_setvar (var, e) ->
    mk (To.Pexp_setinstvar (var, copy_expression e))
  | From.Pexp_override fields ->
    mk
      (To.Pexp_override
         (List.map (fun (var, e) -> var, copy_expression e) fields))
  | From.Pexp_letmodule (name, me, body) ->
    mk (To.Pexp_letmodule (name, copy_module_expr me, copy_expression body))
  | From.Pexp_letexception (ext, body) ->
    mk
      (To.Pexp_letexception
         (copy_extension_constructor ext, copy_expression body))
  | From.Pexp_assert e -> mk (To.Pexp_assert (copy_expression e))
  | From.Pexp_lazy e -> mk (To.Pexp_lazy (copy_expression e))
  | From.Pexp_poly (e, typ) ->
    mk (To.Pexp_poly (copy_expression e, Option.map copy_core_type typ))
  | From.Pexp_object cs -> mk (To.Pexp_object (copy_class_structure cs))
  | From.Pexp_newtype (var, _, body) ->
    mk (To.Pexp_newtype (var, copy_expression body))
  | From.Pexp_pack (me, pkg) ->
    mk (To.Pexp_pack (copy_module_expr me, Option.map copy_package_type pkg))
  | From.Pexp_open (od, body) ->
    mk (To.Pexp_open (copy_open_declaration od, copy_expression body))
  | From.Pexp_letop letop -> mk (To.Pexp_letop (copy_letop letop))
  | From.Pexp_extension ext when is_extension "src_pos" ext ->
    mk (To.Pexp_ident (lexing_lid ~loc:(fst ext).loc "dummy_pos"))
  | From.Pexp_extension ext -> mk (To.Pexp_extension (copy_extension ext))
  | From.Pexp_unreachable -> mk To.Pexp_unreachable
  | From.Pexp_stack inner | From.Pexp_borrow inner -> unwrap inner
  | From.Pexp_comprehension _ -> not_erasable ~loc "a comprehension"
  | From.Pexp_overwrite _ -> not_erasable ~loc "overwrite_"
  | From.Pexp_quote _ | From.Pexp_splice _ ->
    not_erasable ~loc "runtime metaprogramming"
  | From.Pexp_hole -> not_erasable ~loc "a typed hole"

and copy_labeled_expression (label, e) = label, copy_expression e

and copy_argument (label, e) = copy_arg_label label, copy_expression e

and copy_field_expression (lid, e) = lid, copy_expression e

and copy_case { From.pc_lhs; pc_guard; pc_rhs } : To.case =
  { To.pc_lhs = copy_pattern pc_lhs;
    pc_guard = Option.map copy_expression pc_guard;
    pc_rhs = copy_expression pc_rhs
  }

and copy_letop { From.let_; ands; body } : To.letop =
  { To.let_ = copy_binding_op let_;
    ands = List.map copy_binding_op ands;
    body = copy_expression body
  }

and copy_binding_op { From.pbop_op; pbop_pat; pbop_exp; pbop_loc } :
    To.binding_op =
  { To.pbop_op;
    pbop_pat = copy_pattern pbop_pat;
    pbop_exp = copy_expression pbop_exp;
    pbop_loc
  }

and copy_function_param { From.pparam_loc; pparam_desc } : To.function_param =
  let pparam_desc =
    match pparam_desc with
    (* [~(p : [%call_pos])] becomes [?(p = Lexing.dummy_pos)]. *)
    | From.Pparam_val
        ( Fa.Labelled label,
          None,
          { From.ppat_desc =
              From.Ppat_constraint
                (pat, Some { From.ptyp_desc = From.Ptyp_extension ext; _ }, _);
            _
          } )
      when is_extension "call_pos" ext ->
      To.Pparam_val
        ( Ta.Optional label,
          Some (dummy_pos ~loc:(fst ext).loc),
          copy_pattern pat )
    | From.Pparam_val (label, default, pat) ->
      To.Pparam_val
        ( copy_arg_label label,
          Option.map copy_expression default,
          copy_pattern pat )
    | From.Pparam_newtype (var, _) -> To.Pparam_newtype var
  in
  { To.pparam_loc; pparam_desc }

and copy_function_body : From.function_body -> To.function_body = function
  | From.Pfunction_body e -> To.Pfunction_body (copy_expression e)
  | From.Pfunction_cases (cases, loc, attrs) ->
    To.Pfunction_cases (List.map copy_case cases, loc, copy_attributes attrs)

and copy_type_constraint : From.type_constraint -> To.type_constraint = function
  | From.Pconstraint typ -> To.Pconstraint (copy_core_type typ)
  | From.Pcoerce (from, typ) ->
    To.Pcoerce (Option.map copy_core_type from, copy_core_type typ)

(* Value descriptions and type declarations *)

and copy_value_description
    { From.pval_poly = _;
      pval_name;
      pval_type;
      pval_modalities = _;
      pval_prim;
      pval_attributes;
      pval_loc
    } : To.value_description =
  { To.pval_name;
    pval_type = copy_core_type pval_type;
    pval_prim;
    pval_attributes = copy_attributes pval_attributes;
    pval_loc
  }

and copy_type_declaration
    { From.ptype_name;
      ptype_params;
      ptype_cstrs;
      ptype_kind;
      ptype_private;
      ptype_manifest;
      ptype_attributes;
      ptype_jkind_annotation;
      ptype_loc
    } : To.type_declaration =
  (* [type t : immediate] keeps its meaning as [type t [@@immediate]]; other
     kind annotations are dropped. *)
  let kind_attributes =
    match ptype_jkind_annotation with
    | Some
        { From.pjka_desc =
            From.Pjk_abbreviation
              { txt = Longident.Lident (("immediate" | "immediate64") as kind);
                _
              };
          _
        } ->
      [attribute_named kind]
    | Some _ | None -> []
  in
  { To.ptype_name;
    ptype_params = List.map copy_param ptype_params;
    ptype_cstrs =
      List.map
        (fun (t1, t2, loc) -> copy_core_type t1, copy_core_type t2, loc)
        ptype_cstrs;
    ptype_kind = copy_type_kind ptype_kind;
    ptype_private = copy_private_flag ptype_private;
    ptype_manifest = Option.map copy_core_type ptype_manifest;
    ptype_attributes = copy_attributes ptype_attributes @ kind_attributes;
    ptype_loc
  }

and copy_type_kind : From.type_kind -> To.type_kind = function
  | From.Ptype_abstract -> To.Ptype_abstract
  | From.Ptype_variant constructors ->
    To.Ptype_variant (List.map copy_constructor_declaration constructors)
  | From.Ptype_record labels | From.Ptype_record_unboxed_product labels ->
    To.Ptype_record (List.map copy_label_declaration labels)
  | From.Ptype_open -> To.Ptype_open

and copy_label_declaration
    { From.pld_name;
      pld_mutable;
      pld_modalities;
      pld_type;
      pld_loc;
      pld_attributes
    } : To.label_declaration =
  let pld_attributes = copy_attributes pld_attributes in
  { To.pld_name;
    pld_mutable = copy_mutable_flag pld_mutable;
    pld_type = copy_core_type pld_type;
    pld_loc;
    pld_attributes =
      (if has_modality "global" pld_modalities
       then globalized_attr :: pld_attributes
       else pld_attributes)
  }

and copy_constructor_declaration
    { From.pcd_name; pcd_vars; pcd_args; pcd_res; pcd_loc; pcd_attributes } :
    To.constructor_declaration =
  { To.pcd_name;
    pcd_vars = List.map copy_kinded_var pcd_vars;
    pcd_args = copy_constructor_arguments pcd_args;
    pcd_res = Option.map copy_core_type pcd_res;
    pcd_loc;
    pcd_attributes = copy_attributes pcd_attributes
  }

and copy_constructor_argument { From.pca_modalities; pca_type; pca_loc = _ } =
  let typ = copy_core_type pca_type in
  if has_modality "global" pca_modalities
  then { typ with To.ptyp_attributes = globalized_attr :: typ.ptyp_attributes }
  else typ

and copy_constructor_arguments :
    From.constructor_arguments -> To.constructor_arguments = function
  | From.Pcstr_tuple args ->
    To.Pcstr_tuple (List.map copy_constructor_argument args)
  | From.Pcstr_record labels ->
    To.Pcstr_record (List.map copy_label_declaration labels)

and copy_type_extension
    { From.ptyext_path;
      ptyext_params;
      ptyext_constructors;
      ptyext_private;
      ptyext_loc;
      ptyext_attributes
    } : To.type_extension =
  { To.ptyext_path;
    ptyext_params = List.map copy_param ptyext_params;
    ptyext_constructors =
      List.map copy_extension_constructor ptyext_constructors;
    ptyext_private = copy_private_flag ptyext_private;
    ptyext_loc;
    ptyext_attributes = copy_attributes ptyext_attributes
  }

and copy_extension_constructor
    { From.pext_name; pext_kind; pext_loc; pext_attributes } :
    To.extension_constructor =
  { To.pext_name;
    pext_kind = copy_extension_constructor_kind pext_kind;
    pext_loc;
    pext_attributes = copy_attributes pext_attributes
  }

and copy_type_exception
    { From.ptyexn_constructor; ptyexn_loc; ptyexn_attributes } :
    To.type_exception =
  { To.ptyexn_constructor = copy_extension_constructor ptyexn_constructor;
    ptyexn_loc;
    ptyexn_attributes = copy_attributes ptyexn_attributes
  }

and copy_extension_constructor_kind :
    From.extension_constructor_kind -> To.extension_constructor_kind = function
  | From.Pext_decl (vars, args, res) ->
    To.Pext_decl
      ( List.map copy_kinded_var vars,
        copy_constructor_arguments args,
        Option.map copy_core_type res )
  | From.Pext_rebind lid -> To.Pext_rebind lid

(* Class language *)

and copy_class_type { From.pcty_desc; pcty_loc; pcty_attributes } :
    To.class_type =
  { To.pcty_desc = copy_class_type_desc pcty_desc;
    pcty_loc;
    pcty_attributes = copy_attributes pcty_attributes
  }

and copy_class_type_desc : From.class_type_desc -> To.class_type_desc = function
  | From.Pcty_constr (lid, args) ->
    To.Pcty_constr (lid, List.map copy_core_type args)
  | From.Pcty_signature cs -> To.Pcty_signature (copy_class_signature cs)
  | From.Pcty_arrow (label, typ, ct) ->
    To.Pcty_arrow (copy_arg_label label, copy_core_type typ, copy_class_type ct)
  | From.Pcty_extension ext -> To.Pcty_extension (copy_extension ext)
  | From.Pcty_open (od, ct) ->
    To.Pcty_open (copy_open_description od, copy_class_type ct)

and copy_class_signature { From.pcsig_self; pcsig_fields } : To.class_signature
    =
  { To.pcsig_self = copy_core_type pcsig_self;
    pcsig_fields = List.map copy_class_type_field pcsig_fields
  }

and copy_class_type_field { From.pctf_desc; pctf_loc; pctf_attributes } :
    To.class_type_field =
  { To.pctf_desc = copy_class_type_field_desc pctf_desc;
    pctf_loc;
    pctf_attributes = copy_attributes pctf_attributes
  }

and copy_class_type_field_desc :
    From.class_type_field_desc -> To.class_type_field_desc = function
  | From.Pctf_inherit ct -> To.Pctf_inherit (copy_class_type ct)
  | From.Pctf_val (name, mut, virt, typ) ->
    To.Pctf_val
      (name, copy_mutable_flag mut, copy_virtual_flag virt, copy_core_type typ)
  | From.Pctf_method (name, priv, virt, typ) ->
    To.Pctf_method
      (name, copy_private_flag priv, copy_virtual_flag virt, copy_core_type typ)
  | From.Pctf_constraint (t1, t2) ->
    To.Pctf_constraint (copy_core_type t1, copy_core_type t2)
  | From.Pctf_attribute attr -> To.Pctf_attribute (copy_attribute attr)
  | From.Pctf_extension ext -> To.Pctf_extension (copy_extension ext)

and copy_class_infos :
    'a 'b. ('a -> 'b) -> 'a From.class_infos -> 'b To.class_infos =
 fun copy_expr
     { From.pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes } ->
  { To.pci_virt = copy_virtual_flag pci_virt;
    pci_params = List.map copy_param pci_params;
    pci_name;
    pci_expr = copy_expr pci_expr;
    pci_loc;
    pci_attributes = copy_attributes pci_attributes
  }

and copy_class_expr { From.pcl_desc; pcl_loc; pcl_attributes } : To.class_expr =
  { To.pcl_desc = copy_class_expr_desc pcl_desc;
    pcl_loc;
    pcl_attributes = copy_attributes pcl_attributes
  }

and copy_class_expr_desc : From.class_expr_desc -> To.class_expr_desc = function
  | From.Pcl_constr (lid, args) ->
    To.Pcl_constr (lid, List.map copy_core_type args)
  | From.Pcl_structure cs -> To.Pcl_structure (copy_class_structure cs)
  | From.Pcl_fun (label, default, pat, ce) ->
    To.Pcl_fun
      ( copy_arg_label label,
        Option.map copy_expression default,
        copy_pattern pat,
        copy_class_expr ce )
  | From.Pcl_apply (ce, args) ->
    To.Pcl_apply (copy_class_expr ce, List.map copy_argument args)
  | From.Pcl_let (rec_flag, bindings, ce) ->
    To.Pcl_let
      ( copy_rec_flag rec_flag,
        List.map copy_value_binding bindings,
        copy_class_expr ce )
  | From.Pcl_constraint (ce, ct) ->
    To.Pcl_constraint (copy_class_expr ce, copy_class_type ct)
  | From.Pcl_extension ext -> To.Pcl_extension (copy_extension ext)
  | From.Pcl_open (od, ce) ->
    To.Pcl_open (copy_open_description od, copy_class_expr ce)

and copy_class_structure { From.pcstr_self; pcstr_fields } : To.class_structure
    =
  { To.pcstr_self = copy_pattern pcstr_self;
    pcstr_fields = List.map copy_class_field pcstr_fields
  }

and copy_class_field { From.pcf_desc; pcf_loc; pcf_attributes } : To.class_field
    =
  { To.pcf_desc = copy_class_field_desc pcf_desc;
    pcf_loc;
    pcf_attributes = copy_attributes pcf_attributes
  }

and copy_class_field_desc : From.class_field_desc -> To.class_field_desc =
  function
  | From.Pcf_inherit (override, ce, alias) ->
    To.Pcf_inherit (copy_override_flag override, copy_class_expr ce, alias)
  | From.Pcf_val (name, mut, kind) ->
    To.Pcf_val (name, copy_mutable_flag mut, copy_class_field_kind kind)
  | From.Pcf_method (name, priv, kind) ->
    To.Pcf_method (name, copy_private_flag priv, copy_class_field_kind kind)
  | From.Pcf_constraint (t1, t2) ->
    To.Pcf_constraint (copy_core_type t1, copy_core_type t2)
  | From.Pcf_initializer e -> To.Pcf_initializer (copy_expression e)
  | From.Pcf_attribute attr -> To.Pcf_attribute (copy_attribute attr)
  | From.Pcf_extension ext -> To.Pcf_extension (copy_extension ext)

and copy_class_field_kind : From.class_field_kind -> To.class_field_kind =
  function
  | From.Cfk_virtual typ -> To.Cfk_virtual (copy_core_type typ)
  | From.Cfk_concrete (override, e) ->
    To.Cfk_concrete (copy_override_flag override, copy_expression e)

(* Module language *)

and copy_module_type { From.pmty_desc; pmty_loc = loc; pmty_attributes } :
    To.module_type =
  let mk pmty_desc =
    { To.pmty_desc;
      pmty_loc = loc;
      pmty_attributes = copy_attributes pmty_attributes
    }
  in
  match pmty_desc with
  | From.Pmty_ident lid -> mk (To.Pmty_ident lid)
  | From.Pmty_signature sg -> mk (To.Pmty_signature (copy_signature sg))
  | From.Pmty_functor (param, res, _) ->
    mk (To.Pmty_functor (copy_functor_parameter param, copy_module_type res))
  | From.Pmty_with (mty, constraints) -> (
    (* Kind constraints are erased; [S with kind_ k = ...] alone is [S]. *)
    match List.filter_map copy_with_constraint constraints with
    | [] ->
      copy_module_type
        { mty with pmty_attributes = pmty_attributes @ mty.pmty_attributes }
    | constraints -> mk (To.Pmty_with (copy_module_type mty, constraints)))
  | From.Pmty_typeof me -> mk (To.Pmty_typeof (copy_module_expr me))
  | From.Pmty_extension ext -> mk (To.Pmty_extension (copy_extension ext))
  | From.Pmty_alias lid -> mk (To.Pmty_alias lid)
  | From.Pmty_strengthen _ -> not_erasable ~loc "module strengthening"

and copy_functor_parameter : From.functor_parameter -> To.functor_parameter =
  function
  | From.Unit -> To.Unit
  | From.Named (name, mty, _) -> To.Named (name, copy_module_type mty)

and copy_signature { From.psg_modalities = _; psg_items; psg_loc = _ } :
    To.signature =
  List.concat_map copy_signature_item psg_items

(* Kind declarations have no counterpart and are dropped, hence the list. *)
and copy_signature_item { From.psig_desc; psig_loc = loc } :
    To.signature_item list =
  let mk psig_desc = [{ To.psig_desc; psig_loc = loc }] in
  match psig_desc with
  | From.Psig_value vd -> mk (To.Psig_value (copy_value_description vd))
  | From.Psig_type (rec_flag, decls) ->
    mk
      (To.Psig_type
         (copy_rec_flag rec_flag, List.map copy_type_declaration decls))
  | From.Psig_typesubst decls ->
    mk (To.Psig_typesubst (List.map copy_type_declaration decls))
  | From.Psig_typext ext -> mk (To.Psig_typext (copy_type_extension ext))
  | From.Psig_exception exn -> mk (To.Psig_exception (copy_type_exception exn))
  | From.Psig_module md -> mk (To.Psig_module (copy_module_declaration md))
  | From.Psig_modsubst ms -> mk (To.Psig_modsubst (copy_module_substitution ms))
  | From.Psig_recmodule mds ->
    mk (To.Psig_recmodule (List.map copy_module_declaration mds))
  | From.Psig_modtype mtd ->
    mk (To.Psig_modtype (copy_module_type_declaration mtd))
  | From.Psig_modtypesubst mtd ->
    mk (To.Psig_modtypesubst (copy_module_type_declaration mtd))
  | From.Psig_open od -> mk (To.Psig_open (copy_open_description od))
  | From.Psig_include (incl, _) ->
    mk (To.Psig_include (copy_include_infos copy_module_type incl))
  | From.Psig_class cds ->
    mk (To.Psig_class (List.map (copy_class_infos copy_class_type) cds))
  | From.Psig_class_type ctds ->
    mk (To.Psig_class_type (List.map (copy_class_infos copy_class_type) ctds))
  | From.Psig_attribute attr -> mk (To.Psig_attribute (copy_attribute attr))
  | From.Psig_extension (ext, attrs) ->
    mk (To.Psig_extension (copy_extension ext, copy_attributes attrs))
  | From.Psig_jkind _ -> []

and copy_module_declaration
    { From.pmd_name; pmd_type; pmd_modalities = _; pmd_attributes; pmd_loc } :
    To.module_declaration =
  { To.pmd_name;
    pmd_type = copy_module_type pmd_type;
    pmd_attributes = copy_attributes pmd_attributes;
    pmd_loc
  }

and copy_module_substitution
    { From.pms_name; pms_manifest; pms_attributes; pms_loc } :
    To.module_substitution =
  { To.pms_name;
    pms_manifest;
    pms_attributes = copy_attributes pms_attributes;
    pms_loc
  }

and copy_module_type_declaration
    { From.pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } :
    To.module_type_declaration =
  { To.pmtd_name;
    pmtd_type = Option.map copy_module_type pmtd_type;
    pmtd_attributes = copy_attributes pmtd_attributes;
    pmtd_loc
  }

and copy_open_infos :
    'a 'b. ('a -> 'b) -> 'a From.open_infos -> 'b To.open_infos =
 fun copy_expr
     { From.popen_expr; popen_override; popen_loc; popen_attributes } ->
  { To.popen_expr = copy_expr popen_expr;
    popen_override = copy_override_flag popen_override;
    popen_loc;
    popen_attributes = copy_attributes popen_attributes
  }

and copy_open_description od = copy_open_infos (fun lid -> lid) od

and copy_open_declaration od = copy_open_infos copy_module_expr od

and copy_include_infos :
    'a 'b. ('a -> 'b) -> 'a From.include_infos -> 'b To.include_infos =
 fun copy_mod { From.pincl_kind; pincl_mod; pincl_loc; pincl_attributes } ->
  (match pincl_kind with
  | From.Structure -> ()
  | From.Functor -> not_erasable ~loc:pincl_loc "include functor");
  { To.pincl_mod = copy_mod pincl_mod;
    pincl_loc;
    pincl_attributes = copy_attributes pincl_attributes
  }

and copy_with_constraint : From.with_constraint -> To.with_constraint option =
  function
  | From.Pwith_type (lid, decl) ->
    Some (To.Pwith_type (lid, copy_type_declaration decl))
  | From.Pwith_module (lid1, lid2) -> Some (To.Pwith_module (lid1, lid2))
  | From.Pwith_modtype (lid, mty) ->
    Some (To.Pwith_modtype (lid, copy_module_type mty))
  | From.Pwith_jkind _ | From.Pwith_jkindsubst _ -> None
  | From.Pwith_modtypesubst (lid, mty) ->
    Some (To.Pwith_modtypesubst (lid, copy_module_type mty))
  | From.Pwith_typesubst (lid, decl) ->
    Some (To.Pwith_typesubst (lid, copy_type_declaration decl))
  | From.Pwith_modsubst (lid1, lid2) -> Some (To.Pwith_modsubst (lid1, lid2))

and copy_module_expr { From.pmod_desc; pmod_loc = loc; pmod_attributes } :
    To.module_expr =
  let mk pmod_desc =
    { To.pmod_desc;
      pmod_loc = loc;
      pmod_attributes = copy_attributes pmod_attributes
    }
  in
  match pmod_desc with
  | From.Pmod_ident lid -> mk (To.Pmod_ident lid)
  | From.Pmod_structure str -> mk (To.Pmod_structure (copy_structure str))
  | From.Pmod_functor (param, body) ->
    mk (To.Pmod_functor (copy_functor_parameter param, copy_module_expr body))
  | From.Pmod_apply (f, arg) ->
    mk (To.Pmod_apply (copy_module_expr f, copy_module_expr arg))
  | From.Pmod_apply_unit f -> mk (To.Pmod_apply_unit (copy_module_expr f))
  | From.Pmod_constraint (me, Some mty, _) ->
    mk (To.Pmod_constraint (copy_module_expr me, copy_module_type mty))
  | From.Pmod_constraint (me, None, _) ->
    copy_module_expr
      { me with pmod_attributes = pmod_attributes @ me.pmod_attributes }
  | From.Pmod_unpack e -> mk (To.Pmod_unpack (copy_expression e))
  | From.Pmod_extension ext -> mk (To.Pmod_extension (copy_extension ext))
  | From.Pmod_hole -> not_erasable ~loc "a module hole"
  | From.Pmod_instance _ -> not_erasable ~loc "an instance module name"

and copy_structure str = List.concat_map copy_structure_item str

and copy_structure_item { From.pstr_desc; pstr_loc = loc } :
    To.structure_item list =
  let mk pstr_desc = [{ To.pstr_desc; pstr_loc = loc }] in
  match pstr_desc with
  | From.Pstr_eval (e, attrs) ->
    mk (To.Pstr_eval (copy_expression e, copy_attributes attrs))
  | From.Pstr_value (rec_flag, bindings) ->
    mk
      (To.Pstr_value
         (copy_rec_flag rec_flag, List.map copy_value_binding bindings))
  | From.Pstr_primitive vd -> mk (To.Pstr_primitive (copy_value_description vd))
  | From.Pstr_type (rec_flag, decls) ->
    mk
      (To.Pstr_type
         (copy_rec_flag rec_flag, List.map copy_type_declaration decls))
  | From.Pstr_typext ext -> mk (To.Pstr_typext (copy_type_extension ext))
  | From.Pstr_exception exn -> mk (To.Pstr_exception (copy_type_exception exn))
  | From.Pstr_module mb -> mk (To.Pstr_module (copy_module_binding mb))
  | From.Pstr_recmodule mbs ->
    mk (To.Pstr_recmodule (List.map copy_module_binding mbs))
  | From.Pstr_modtype mtd ->
    mk (To.Pstr_modtype (copy_module_type_declaration mtd))
  | From.Pstr_open od -> mk (To.Pstr_open (copy_open_declaration od))
  | From.Pstr_class cds ->
    mk (To.Pstr_class (List.map (copy_class_infos copy_class_expr) cds))
  | From.Pstr_class_type ctds ->
    mk (To.Pstr_class_type (List.map (copy_class_infos copy_class_type) ctds))
  | From.Pstr_include incl ->
    mk (To.Pstr_include (copy_include_infos copy_module_expr incl))
  | From.Pstr_attribute attr -> mk (To.Pstr_attribute (copy_attribute attr))
  | From.Pstr_extension (ext, attrs) ->
    mk (To.Pstr_extension (copy_extension ext, copy_attributes attrs))
  | From.Pstr_jkind _ -> []

and copy_value_constraint : From.value_constraint -> To.value_constraint =
  function
  | From.Pvc_constraint { locally_abstract_univars; typ } ->
    To.Pvc_constraint { locally_abstract_univars; typ = copy_core_type typ }
  | From.Pvc_coercion { ground; coercion } ->
    To.Pvc_coercion
      { ground = Option.map copy_core_type ground;
        coercion = copy_core_type coercion
      }

and copy_value_binding
    { From.pvb_is_poly = _;
      pvb_pat;
      pvb_expr;
      pvb_constraint;
      pvb_modes = _;
      pvb_attributes;
      pvb_loc
    } : To.value_binding =
  { To.pvb_pat = copy_pattern pvb_pat;
    pvb_expr = copy_expression pvb_expr;
    pvb_constraint = Option.map copy_value_constraint pvb_constraint;
    pvb_attributes = copy_attributes pvb_attributes;
    pvb_loc
  }

and copy_module_binding { From.pmb_name; pmb_expr; pmb_attributes; pmb_loc } :
    To.module_binding =
  { To.pmb_name;
    pmb_expr = copy_module_expr pmb_expr;
    pmb_attributes = copy_attributes pmb_attributes;
    pmb_loc
  }

let structure = copy_structure

let signature = copy_signature
