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

open Typedtree
open Spec

type primitive_kind = Location

type unsupported =
  | Arrays
  | Atomic_fields
  | Attributes
  | Binding_operators
  | Block_indices
  | Borrow
  | Coercions
  | Comprehensions
  | Effect_handlers
  | Exception_handlers
  | Exclave
  | Extension_constructors
  | First_class_modules
  | Functions
  | Layout_polymorphism
  | Local_exceptions
  | Local_modules
  | Local_opens
  | Locally_abstract_types
  | Type_patterns
  | Loops
  | Mode_annotations
  | Mutable_variables
  | Mutation
  | Objects
  | Omitted_arguments
  | Overwrite
  | Polymorphic_annotations
  | Position_arguments
  | Primitive of primitive_kind
  | Probes
  | Quotations
  | Refutation_cases
  | Source_positions
  | Stack_allocation
  | Values_through_applications

type error = Unsupported of unsupported

exception Error of Location.t * error

let unsupported loc what = raise (Error (loc, Unsupported what))

(* Labels *)

let arg_label loc (lbl : Types.arg_label) : Spec.arg_label =
  match lbl with
  | Nolabel -> Nolabel
  | Labelled s -> Labelled s
  | Optional s -> Optional s
  | Position _ -> unsupported loc Position_arguments

(* Globals *)

let global loc (path : Path.t) =
  if Path.contains_apply path then unsupported loc Values_through_applications;
  path

let unsupported_primitive (prim : Primitive.description) =
  if String.starts_with ~prefix:"%loc_" prim.prim_name
  then Some Location
  else None

let check_primitive loc prim =
  match unsupported_primitive prim with
  | None -> ()
  | Some kind -> unsupported loc (Primitive kind)

let check_constructor loc (cstr : Data_types.constructor_description) =
  match cstr.cstr_tag with
  | Extension _ -> unsupported loc Extension_constructors
  | Ordinary _ | Null -> ()

let value_description (desc : Types.value_description) : spec_value_description
    =
  { sval_type = desc.val_type;
    sval_modalities = desc.val_modalities;
    sval_lpoly = desc.val_lpoly;
    sval_loc = desc.val_loc;
    sval_zero_alloc = desc.val_zero_alloc;
    sval_attributes = desc.val_attributes;
    sval_uid = desc.val_uid;
    sval_alias = desc.val_alias
  }

(* Expressions. [bound] contains the local variables in scope. *)

let check_attributes loc (attrs : Parsetree.attributes) =
  match attrs with [] -> () | _ :: _ -> unsupported loc Attributes

(* Patterns *)

let add_bound bound ids =
  List.fold_left (fun s id -> Ident.Set.add id s) bound ids

let pat_extra (extra, loc, attrs) : spec_pat_extra * Location.t =
  check_attributes loc attrs;
  match (extra : pat_extra) with
  | Tpat_constraint (Some { ctyp_desc = Ttyp_poly _; _ }, _) ->
    unsupported loc Polymorphic_annotations
  | Tpat_constraint (Some cty, { mode_desc = []; _ }) ->
    Stpat_constraint cty, loc
  | Tpat_constraint _ -> unsupported loc Mode_annotations
  | Tpat_inspected_type inspection -> Stpat_inspected_type inspection, loc
  | Tpat_type _ -> unsupported loc Type_patterns
  | Tpat_open _ -> unsupported loc Local_opens
  | Tpat_unpack -> unsupported loc First_class_modules

let rec spectree_pattern : type k. k general_pattern -> k spec_general_pattern =
 fun p ->
  let loc = p.pat_loc in
  check_attributes loc p.pat_attributes;
  let stpat_extra = List.map pat_extra p.pat_extra in
  let labelled (lbl, p, sort) = lbl, spectree_pattern p, sort in
  let field (lid, lbl, p) = lid, lbl, spectree_pattern p in
  let stpat_desc : k spec_pattern_desc =
    match p.pat_desc with
    | Tpat_any -> Stpat_any
    | Tpat_var { id; name; uid; sort; mode } ->
      Stpat_var { id; name; uid; sort; mode }
    | Tpat_alias { pattern; id; name; uid; sort; mode; type_expr } ->
      let pattern = spectree_pattern pattern in
      Stpat_alias { pattern; id; name; uid; sort; mode; type_expr }
    | Tpat_constant c -> Stpat_constant c
    | Tpat_unboxed_unit -> Stpat_unboxed_unit
    | Tpat_unboxed_bool b -> Stpat_unboxed_bool b
    | Tpat_tuple ps -> Stpat_tuple (List.map labelled ps)
    | Tpat_unboxed_tuple ps -> Stpat_unboxed_tuple (List.map labelled ps)
    | Tpat_construct (lid, cstr, repr, args, None) ->
      check_constructor loc cstr;
      let args = List.map (fun (sort, p) -> sort, spectree_pattern p) args in
      Stpat_construct (lid, cstr, repr, args)
    | Tpat_construct (_, _, _, _, Some _) ->
      unsupported loc Locally_abstract_types
    | Tpat_variant (lbl, arg, row) ->
      Stpat_variant (lbl, Option.map spectree_pattern arg, row)
    | Tpat_record (fields, repr, closed) ->
      Stpat_record (List.map field fields, repr, closed)
    | Tpat_record_unboxed_product (fields, repr, closed) ->
      Stpat_record_unboxed_product (List.map field fields, repr, closed)
    | Tpat_array _ -> unsupported loc Arrays
    | Tpat_fun_layout _ -> unsupported loc Layout_polymorphism
    | Tpat_lazy p -> Stpat_lazy (spectree_pattern p)
    | Tpat_value p -> Stpat_value (spectree_pattern (p :> Typedtree.pattern))
    | Tpat_exception _ -> unsupported loc Exception_handlers
    | Tpat_or (p1, p2, None) ->
      Stpat_or (spectree_pattern p1, spectree_pattern p2)
    | Tpat_or (_, _, Some _) -> unsupported loc Type_patterns
  in
  { stpat_desc;
    stpat_loc = loc;
    stpat_extra;
    stpat_type = p.pat_type;
    stpat_env = p.pat_env;
    stpat_unique_barrier = p.pat_unique_barrier
  }

let extra (extra, loc, attrs) : spec_extra * Location.t =
  check_attributes loc attrs;
  match extra with
  | Texp_constraint cty -> Stexp_constraint cty, loc
  | Texp_inspected_type inspection -> Stexp_inspected_type inspection, loc
  | Texp_mode _ -> unsupported loc Mode_annotations
  | Texp_poly _ -> unsupported loc Polymorphic_annotations
  | Texp_coerce _ -> unsupported loc Coercions
  | Texp_newtype _ -> unsupported loc Locally_abstract_types
  | Texp_stack -> unsupported loc Stack_allocation
  | Texp_borrowed | Texp_ghost_region -> unsupported loc Borrow

let check_position loc (position : apply_position) =
  match position with
  | Default -> ()
  | Tail | Nontail -> unsupported loc Attributes

let check_zero_alloc loc (zero_alloc : Zero_alloc.assume option) =
  match zero_alloc with None -> () | Some _ -> unsupported loc Attributes

let rec spectree bound (e : Typedtree.expression) : spec_expression =
  check_attributes e.exp_loc e.exp_attributes;
  let stexp_extra = List.map extra e.exp_extra in
  let stexp_desc = spectree_desc bound e.exp_loc e.exp_desc in
  { stexp_desc;
    stexp_loc = e.exp_loc;
    stexp_extra;
    stexp_type = e.exp_type;
    stexp_env = e.exp_env
  }

and spectree_desc bound loc desc : spec_expression_desc =
  match desc with
  | Texp_ident { path; lid; desc; kind; unique_use; staticity; mode } ->
    let ident : spec_ident =
      { sid_unique_use = unique_use;
        sid_staticity = staticity;
        sid_mode = mode;
        sid_desc = value_description desc
      }
    in
    ident_desc bound loc path lid desc.val_kind kind ident
  | Texp_constant c -> Stexp_constant c
  | Texp_apply (_, [], _, _, _, _) ->
    Misc.fatal_error "Translspec.spectree: application without arguments"
  | Texp_apply
      (funct, (_ :: _ as args), position, ret_mode, yielding, zero_alloc) ->
    check_position loc position;
    check_zero_alloc loc zero_alloc;
    let funct = spectree bound funct in
    let args = List.map (argument bound loc) args in
    Stexp_apply { funct; args; ret_mode; yielding }
  | Texp_apply_layout _ -> unsupported loc Layout_polymorphism
  | Texp_let (rec_flag, vbs, body) ->
    let inner = add_bound bound (let_bound_idents vbs) in
    let in_exprs =
      match rec_flag with Recursive -> inner | Nonrecursive -> bound
    in
    let vbs = List.map (value_binding in_exprs) vbs in
    Stexp_let (rec_flag, vbs, spectree inner body)
  | Texp_letmutable _ -> unsupported loc Mutable_variables
  | Texp_function _ -> unsupported loc Functions
  | Texp_match (e, sort, cases, [], partial) ->
    let e = spectree bound e in
    Stexp_match (e, sort, List.map (case bound) cases, partial)
  | Texp_match (_, _, _, _ :: _, _) -> unsupported loc Effect_handlers
  | Texp_try _ -> unsupported loc Exception_handlers
  | Texp_tuple (es, mode) -> Stexp_tuple (List.map (labelled bound) es, mode)
  | Texp_unboxed_tuple es -> Stexp_unboxed_tuple (List.map (labelled bound) es)
  | Texp_unboxed_unit -> Stexp_unboxed_unit
  | Texp_unboxed_bool b -> Stexp_unboxed_bool b
  | Texp_construct (lid, constructor, repr, args, mode) ->
    check_constructor loc constructor;
    let args = List.map (fun (sort, e) -> sort, spectree bound e) args in
    Stexp_construct { lid; constructor; repr; args; mode }
  | Texp_variant (lbl, arg) ->
    Stexp_variant (lbl, Option.map (fun (e, mode) -> spectree bound e, mode) arg)
  | Texp_record { fields; representation; extended_expression; locality_mode }
    ->
    let fields =
      Array.map (fun (lbl, sort, def) -> lbl, sort, definition bound def) fields
    in
    let extended_expression =
      Option.map
        (fun (e, sort, repr, barrier) -> spectree bound e, sort, repr, barrier)
        extended_expression
    in
    Stexp_record { fields; representation; extended_expression; locality_mode }
  | Texp_record_unboxed_product { fields; representation; extended_expression }
    ->
    let fields =
      Array.map (fun (lbl, sort, def) -> lbl, sort, definition bound def) fields
    in
    let extended_expression =
      Option.map (fun (e, sort) -> spectree bound e, sort) extended_expression
    in
    Stexp_record_unboxed_product { fields; representation; extended_expression }
  | Texp_atomic_loc _ -> unsupported loc Atomic_fields
  | Texp_field
      { record; record_sort; record_repres; lid; label; boxing; unique_barrier }
    ->
    let record = spectree bound record in
    Stexp_field
      { record; record_sort; record_repres; lid; label; boxing; unique_barrier }
  | Texp_unboxed_field
      { record; record_sort; record_repres; lid; label; unique_use } ->
    let record = spectree bound record in
    Stexp_unboxed_field
      { record; record_sort; record_repres; lid; label; unique_use }
  | Texp_setfield _ -> unsupported loc Mutation
  | Texp_array _ -> unsupported loc Arrays
  | Texp_idx _ -> unsupported loc Block_indices
  | Texp_list_comprehension _ | Texp_array_comprehension _ ->
    unsupported loc Comprehensions
  | Texp_ifthenelse (c, e1, e2) ->
    Stexp_ifthenelse
      (spectree bound c, spectree bound e1, Option.map (spectree bound) e2)
  | Texp_sequence (e1, sort, e2) ->
    Stexp_sequence (spectree bound e1, sort, spectree bound e2)
  | Texp_while _ | Texp_for _ -> unsupported loc Loops
  | Texp_send _ | Texp_new _ | Texp_instvar _ | Texp_setinstvar _
  | Texp_override _ | Texp_object _ ->
    unsupported loc Objects
  | Texp_mutvar _ | Texp_setmutvar _ -> unsupported loc Mutable_variables
  | Texp_letmodule _ -> unsupported loc Local_modules
  | Texp_letexception _ -> unsupported loc Local_exceptions
  | Texp_assert (e, loc) -> Stexp_assert (spectree bound e, loc)
  | Texp_lazy e -> Stexp_lazy (spectree bound e)
  | Texp_pack _ -> unsupported loc First_class_modules
  | Texp_letop _ -> unsupported loc Binding_operators
  | Texp_unreachable -> unsupported loc Refutation_cases
  | Texp_extension_constructor _ -> unsupported loc Extension_constructors
  | Texp_open _ -> unsupported loc Local_opens
  | Texp_probe _ | Texp_probe_is_enabled _ -> unsupported loc Probes
  | Texp_exclave _ -> unsupported loc Exclave
  | Texp_src_pos -> unsupported loc Source_positions
  | Texp_overwrite _ | Texp_hole _ -> unsupported loc Overwrite
  | Texp_quote _ | Texp_splice _ -> unsupported loc Quotations

and ident_desc bound loc path (lid : Longident.t Location.loc)
    (val_kind : Types.value_kind) kind ident : spec_expression_desc =
  match val_kind, kind with
  | Val_reg sort, Id_value ->
    begin match path with
    | Path.Pident id when Ident.Set.mem id bound ->
      Stexp_var { id; name_loc = lid.loc; sort; ident }
    | _ -> Stexp_global { path = global loc path; lid; sort; ident }
    end
  | Val_prim prim, Id_prim (locality, sort, yielding) ->
    check_primitive loc prim;
    Stexp_primitive
      { path = global loc path; lid; prim; locality; sort; yielding; ident }
  | Val_mut _, _ -> unsupported loc Mutable_variables
  | (Val_ivar _ | Val_self _ | Val_anc _), _ -> unsupported loc Objects
  | Val_reg _, Id_prim _ | Val_prim _, Id_value ->
    Misc.fatal_error "Translspec.spectree: bad Texp_ident"

and labelled bound (lbl, e, sort) = lbl, spectree bound e, sort

and value_binding bound (vb : Typedtree.value_binding) : spec_value_binding =
  check_attributes vb.vb_loc vb.vb_attributes;
  let svb_pat = spectree_pattern vb.vb_pat in
  { svb_pat;
    svb_expr = spectree bound vb.vb_expr;
    svb_rec_kind = vb.vb_rec_kind;
    svb_sort = vb.vb_sort;
    svb_loc = vb.vb_loc
  }

and case : type k. Ident.Set.t -> k Typedtree.case -> k spec_case =
 fun bound c ->
  let sc_lhs = spectree_pattern c.c_lhs in
  (match c.c_cont with
  | None -> ()
  | Some _ -> unsupported c.c_lhs.pat_loc Effect_handlers);
  let bound = add_bound bound (pat_bound_idents c.c_lhs) in
  { sc_lhs;
    sc_guard = Option.map (spectree bound) c.c_guard;
    sc_rhs = spectree bound c.c_rhs
  }

and definition bound : record_label_definition -> spec_label_definition =
  function
  | Kept (ty, mut, unique_use) -> Skept (ty, mut, unique_use)
  | Overridden (lid, e) -> Soverridden (lid, spectree bound e)

and argument bound loc (lbl, (arg : apply_arg)) =
  match arg with
  | Omitted _ -> unsupported loc Omitted_arguments
  | Arg (e, sort) -> arg_label loc lbl, spectree bound e, sort

let spectree ~bound e = spectree bound e

(* Specification expressions. Sort variables are defaulted as in the other
   items of a signature. *)

let constant (c : Typedtree.constant) : Spec.constant =
  match c with
  | Const_int n -> Const_int n
  | Const_char c -> Const_char c
  | Const_untagged_char c -> Const_untagged_char c
  | Const_string (s, _, _) -> Const_string s
  | Const_float f -> Const_float f
  | Const_float32 f -> Const_float32 f
  | Const_unboxed_float f -> Const_unboxed_float f
  | Const_unboxed_float32 f -> Const_unboxed_float32 f
  | Const_int8 n -> Const_int8 n
  | Const_int16 n -> Const_int16 n
  | Const_int32 n -> Const_int32 n
  | Const_int64 n -> Const_int64 n
  | Const_nativeint n -> Const_nativeint n
  | Const_untagged_int n -> Const_untagged_int n
  | Const_untagged_int8 n -> Const_untagged_int8 n
  | Const_untagged_int16 n -> Const_untagged_int16 n
  | Const_unboxed_int32 n -> Const_unboxed_int32 n
  | Const_unboxed_int64 n -> Const_unboxed_int64 n
  | Const_unboxed_nativeint n -> Const_unboxed_nativeint n

let constructor (cstr : Data_types.constructor_description) : Spec.constructor =
  match cstr.cstr_tag with
  | Ordinary _ | Null ->
    Constructor
      { type_path = Data_types.cstr_res_type_path cstr; name = cstr.cstr_name }
  | Extension _ -> Misc.fatal_error "Translspec.spec: extension constructor"

let label (lbl : _ Data_types.gen_label_description) : Spec.label =
  { type_path = Data_types.gen_lbl_res_type_path lbl; name = lbl.lbl_name }

let sort = Jkind.Sort.default_for_transl_and_get

let rec pattern : type k. k spec_general_pattern -> Spec.pattern =
 fun p ->
  let labelled (lbl, p, s) = lbl, pattern p, sort s in
  let field (_, lbl, p) = label lbl, pattern p in
  match p.stpat_desc with
  | Stpat_any -> Spat_any
  | Stpat_var { id; sort = s; _ } -> Spat_var (id, sort s)
  | Stpat_alias { pattern = p; id; sort = s; _ } ->
    Spat_alias (pattern p, id, sort s)
  | Stpat_constant c -> Spat_constant (constant c)
  | Stpat_unboxed_unit -> Spat_unboxed_unit
  | Stpat_unboxed_bool b -> Spat_unboxed_bool b
  | Stpat_tuple ps -> Spat_tuple (List.map labelled ps)
  | Stpat_unboxed_tuple ps -> Spat_unboxed_tuple (List.map labelled ps)
  | Stpat_construct (_, cstr, _, args) ->
    Spat_construct
      (constructor cstr, List.map (fun (s, p) -> sort s, pattern p) args)
  | Stpat_variant (lbl, arg, _) -> Spat_variant (lbl, Option.map pattern arg)
  | Stpat_record (fields, _, closed) ->
    Spat_record (List.map field fields, closed)
  | Stpat_record_unboxed_product (fields, _, closed) ->
    Spat_record_unboxed_product (List.map field fields, closed)
  | Stpat_lazy p -> Spat_lazy (pattern p)
  | Stpat_value p -> pattern p
  | Stpat_or (p1, p2) -> Spat_or (pattern p1, pattern p2)

let rec spec (e : spec_expression) : Spec.expression =
  let labelled (lbl, e, s) = lbl, spec e, sort s in
  match e.stexp_desc with
  | Stexp_var { id; _ } -> Sexp_var id
  | Stexp_global { path; _ } | Stexp_primitive { path; _ } -> Sexp_global path
  | Stexp_constant c -> Sexp_constant (constant c)
  | Stexp_apply { funct; args; _ } ->
    Sexp_apply
      (spec funct, List.map (fun (lbl, e, s) -> lbl, spec e, sort s) args)
  | Stexp_tuple (es, _) -> Sexp_tuple (List.map labelled es)
  | Stexp_unboxed_tuple es -> Sexp_unboxed_tuple (List.map labelled es)
  | Stexp_unboxed_unit -> Sexp_unboxed_unit
  | Stexp_unboxed_bool b -> Sexp_unboxed_bool b
  | Stexp_construct { constructor = cstr; args; _ } ->
    Sexp_construct
      (constructor cstr, List.map (fun (s, e) -> sort s, spec e) args)
  | Stexp_variant (lbl, arg) ->
    Sexp_variant (lbl, Option.map (fun (e, _) -> spec e) arg)
  | Stexp_record { fields; extended_expression; _ } ->
    Sexp_record
      { fields = Array.to_list (Array.map field_definition fields);
        extended_expression =
          Option.map (fun (e, s, _, _) -> spec e, sort s) extended_expression
      }
  | Stexp_record_unboxed_product { fields; extended_expression; _ } ->
    Sexp_record_unboxed_product
      { fields = Array.to_list (Array.map field_definition fields);
        extended_expression =
          Option.map (fun (e, s) -> spec e, sort s) extended_expression
      }
  | Stexp_field { record; record_sort; label = lbl; _ } ->
    Sexp_field
      { record = spec record; sort = sort record_sort; label = label lbl }
  | Stexp_unboxed_field { record; record_sort; label = lbl; _ } ->
    Sexp_unboxed_field
      { record = spec record; sort = sort record_sort; label = label lbl }
  | Stexp_ifthenelse (c, e1, e2) ->
    Sexp_ifthenelse (spec c, spec e1, Option.map spec e2)
  | Stexp_sequence (e1, s, e2) -> Sexp_sequence (spec e1, sort s, spec e2)
  | Stexp_assert (e, _) -> Sexp_assert (spec e)
  | Stexp_lazy e -> Sexp_lazy (spec e)
  | Stexp_let (rec_flag, vbs, body) ->
    let vb (vb : spec_value_binding) : Spec.value_binding =
      { vb_pat = pattern vb.svb_pat;
        vb_expr = spec vb.svb_expr;
        vb_sort = sort vb.svb_sort
      }
    in
    Sexp_let (rec_flag, List.map vb vbs, spec body)
  | Stexp_match (e, s, cases, _) ->
    Sexp_match (spec e, sort s, List.map spec_case cases)

and spec_case : type k. k spec_case -> Spec.case =
 fun c ->
  { c_lhs = pattern c.sc_lhs;
    c_guard = Option.map spec c.sc_guard;
    c_rhs = spec c.sc_rhs
  }

and field_definition :
    'a.
    'a Data_types.gen_label_description * Jkind.sort * spec_label_definition ->
    Spec.label * Spec.sort * Spec.field_definition =
 fun (lbl, s, def) ->
  let def : Spec.field_definition =
    match def with Skept _ -> Kept | Soverridden (_, e) -> Overridden (spec e)
  in
  label lbl, sort s, def

(* Typed expressions *)

let typedtree_arg_label : Spec.arg_label -> Types.arg_label = function
  | Nolabel -> Nolabel
  | Labelled s -> Labelled s
  | Optional s -> Optional s

let typedtree_extra : spec_extra -> exp_extra = function
  | Stexp_constraint cty -> Texp_constraint cty
  | Stexp_inspected_type inspection -> Texp_inspected_type inspection

let typedtree_pat_extra : spec_pat_extra -> pat_extra = function
  | Stpat_constraint cty ->
    Tpat_constraint (Some cty, Typemode.transl_mode_with_locality [])
  | Stpat_inspected_type inspection -> Tpat_inspected_type inspection

let rec typedtree_pattern : type k. k spec_general_pattern -> k general_pattern
    =
 fun p ->
  let data (pat_desc : k pattern_desc) : k general_pattern =
    { pat_desc;
      pat_loc = p.stpat_loc;
      pat_extra =
        List.map
          (fun (extra, loc) -> typedtree_pat_extra extra, loc, [])
          p.stpat_extra;
      pat_type = p.stpat_type;
      pat_env = p.stpat_env;
      pat_attributes = [];
      pat_unique_barrier = p.stpat_unique_barrier
    }
  in
  let labelled (lbl, p, sort) = lbl, typedtree_pattern p, sort in
  let field (lid, lbl, p) = lid, lbl, typedtree_pattern p in
  match p.stpat_desc with
  | Stpat_any -> data Tpat_any
  | Stpat_var { id; name; uid; sort; mode } ->
    data (Tpat_var { id; name; uid; sort; mode })
  | Stpat_alias { pattern; id; name; uid; sort; mode; type_expr } ->
    let pattern = typedtree_pattern pattern in
    data (Tpat_alias { pattern; id; name; uid; sort; mode; type_expr })
  | Stpat_constant c -> data (Tpat_constant c)
  | Stpat_unboxed_unit -> data Tpat_unboxed_unit
  | Stpat_unboxed_bool b -> data (Tpat_unboxed_bool b)
  | Stpat_tuple ps -> data (Tpat_tuple (List.map labelled ps))
  | Stpat_unboxed_tuple ps -> data (Tpat_unboxed_tuple (List.map labelled ps))
  | Stpat_construct (lid, cstr, repr, args) ->
    let args = List.map (fun (sort, p) -> sort, typedtree_pattern p) args in
    data (Tpat_construct (lid, cstr, repr, args, None))
  | Stpat_variant (lbl, arg, row) ->
    data (Tpat_variant (lbl, Option.map typedtree_pattern arg, row))
  | Stpat_record (fields, repr, closed) ->
    data (Tpat_record (List.map field fields, repr, closed))
  | Stpat_record_unboxed_product (fields, repr, closed) ->
    data (Tpat_record_unboxed_product (List.map field fields, repr, closed))
  | Stpat_lazy p -> data (Tpat_lazy (typedtree_pattern p))
  | Stpat_value p -> as_computation_pattern (typedtree_pattern p)
  | Stpat_or (p1, p2) ->
    data (Tpat_or (typedtree_pattern p1, typedtree_pattern p2, None))

let typedtree_value_description (desc : spec_value_description) val_kind :
    Types.value_description =
  { val_type = desc.sval_type;
    val_modalities = desc.sval_modalities;
    val_kind;
    val_lpoly = desc.sval_lpoly;
    val_loc = desc.sval_loc;
    val_zero_alloc = desc.sval_zero_alloc;
    val_attributes = desc.sval_attributes;
    val_uid = desc.sval_uid;
    val_alias = desc.sval_alias
  }

let typedtree_ident path lid val_kind kind (ident : spec_ident) :
    Typedtree.expression_desc =
  Texp_ident
    { path;
      lid;
      desc = typedtree_value_description ident.sid_desc val_kind;
      kind;
      unique_use = ident.sid_unique_use;
      staticity = ident.sid_staticity;
      mode = ident.sid_mode
    }

let rec typedtree (e : spec_expression) : Typedtree.expression =
  { exp_desc = typedtree_desc e.stexp_desc;
    exp_loc = e.stexp_loc;
    exp_extra =
      List.map
        (fun (extra, loc) -> typedtree_extra extra, loc, [])
        e.stexp_extra;
    exp_type = e.stexp_type;
    exp_env = e.stexp_env;
    exp_attributes = []
  }

and typedtree_desc : spec_expression_desc -> Typedtree.expression_desc =
  function
  | Stexp_var { id; name_loc; sort; ident } ->
    let lid = Location.mkloc (Longident.Lident (Ident.name id)) name_loc in
    typedtree_ident (Path.Pident id) lid (Types.Val_reg sort) Id_value ident
  | Stexp_global { path; lid; sort; ident } ->
    typedtree_ident path lid (Types.Val_reg sort) Id_value ident
  | Stexp_primitive { path; lid; prim; locality; sort; yielding; ident } ->
    typedtree_ident path lid (Types.Val_prim prim)
      (Id_prim (locality, sort, yielding))
      ident
  | Stexp_constant c -> Texp_constant c
  | Stexp_apply { funct; args; ret_mode; yielding } ->
    Texp_apply
      ( typedtree funct,
        List.map typedtree_argument args,
        Default,
        ret_mode,
        yielding,
        None )
  | Stexp_tuple (es, mode) -> Texp_tuple (List.map typedtree_labelled es, mode)
  | Stexp_unboxed_tuple es ->
    Texp_unboxed_tuple (List.map typedtree_labelled es)
  | Stexp_unboxed_unit -> Texp_unboxed_unit
  | Stexp_unboxed_bool b -> Texp_unboxed_bool b
  | Stexp_construct { lid; constructor; repr; args; mode } ->
    Texp_construct
      ( lid,
        constructor,
        repr,
        List.map (fun (sort, e) -> sort, typedtree e) args,
        mode )
  | Stexp_variant (lbl, arg) ->
    Texp_variant (lbl, Option.map (fun (e, mode) -> typedtree e, mode) arg)
  | Stexp_record { fields; representation; extended_expression; locality_mode }
    ->
    let fields =
      Array.map
        (fun (lbl, sort, def) -> lbl, sort, typedtree_definition def)
        fields
    in
    let extended_expression =
      Option.map
        (fun (e, sort, repr, barrier) -> typedtree e, sort, repr, barrier)
        extended_expression
    in
    Texp_record { fields; representation; extended_expression; locality_mode }
  | Stexp_record_unboxed_product { fields; representation; extended_expression }
    ->
    let fields =
      Array.map
        (fun (lbl, sort, def) -> lbl, sort, typedtree_definition def)
        fields
    in
    let extended_expression =
      Option.map (fun (e, sort) -> typedtree e, sort) extended_expression
    in
    Texp_record_unboxed_product { fields; representation; extended_expression }
  | Stexp_field
      { record; record_sort; record_repres; lid; label; boxing; unique_barrier }
    ->
    Texp_field
      { record = typedtree record;
        record_sort;
        record_repres;
        lid;
        label;
        boxing;
        unique_barrier
      }
  | Stexp_unboxed_field
      { record; record_sort; record_repres; lid; label; unique_use } ->
    Texp_unboxed_field
      { record = typedtree record;
        record_sort;
        record_repres;
        lid;
        label;
        unique_use
      }
  | Stexp_ifthenelse (c, e1, e2) ->
    Texp_ifthenelse (typedtree c, typedtree e1, Option.map typedtree e2)
  | Stexp_sequence (e1, sort, e2) ->
    Texp_sequence (typedtree e1, sort, typedtree e2)
  | Stexp_assert (e, loc) -> Texp_assert (typedtree e, loc)
  | Stexp_lazy e -> Texp_lazy (typedtree e)
  | Stexp_let (rec_flag, vbs, body) ->
    let vb (vb : spec_value_binding) : Typedtree.value_binding =
      { vb_pat = typedtree_pattern vb.svb_pat;
        vb_expr = typedtree vb.svb_expr;
        vb_rec_kind = vb.svb_rec_kind;
        vb_sort = vb.svb_sort;
        vb_attributes = [];
        vb_loc = vb.svb_loc
      }
    in
    Texp_let (rec_flag, List.map vb vbs, typedtree body)
  | Stexp_match (e, sort, cases, partial) ->
    Texp_match (typedtree e, sort, List.map typedtree_case cases, [], partial)

and typedtree_case : type k. k spec_case -> k Typedtree.case =
 fun c ->
  { c_lhs = typedtree_pattern c.sc_lhs;
    c_cont = None;
    c_guard = Option.map typedtree c.sc_guard;
    c_rhs = typedtree c.sc_rhs
  }

and typedtree_labelled (lbl, e, sort) = lbl, typedtree e, sort

and typedtree_definition : spec_label_definition -> record_label_definition =
  function
  | Skept (ty, mut, unique_use) -> Kept (ty, mut, unique_use)
  | Soverridden (lid, e) -> Overridden (lid, typedtree e)

and typedtree_argument (lbl, e, sort) =
  typedtree_arg_label lbl, Arg (typedtree e, sort)

(* Errors *)

let describe_primitive = function Location -> "location primitives"

let describe = function
  | Arrays -> "arrays"
  | Atomic_fields -> "atomic fields"
  | Attributes -> "attributes"
  | Binding_operators -> "binding operators"
  | Block_indices -> "block indices"
  | Borrow -> "borrow_"
  | Coercions -> "coercions"
  | Comprehensions -> "comprehensions"
  | Effect_handlers -> "effect handlers"
  | Exception_handlers -> "exception handlers"
  | Exclave -> "exclave_"
  | Extension_constructors -> "extension constructors"
  | First_class_modules -> "first-class modules"
  | Functions -> "functions"
  | Layout_polymorphism -> "layout-polymorphic bindings"
  | Local_exceptions -> "local exceptions"
  | Local_modules -> "local modules"
  | Local_opens -> "local opens"
  | Locally_abstract_types -> "locally abstract types"
  | Type_patterns -> "type patterns"
  | Loops -> "loops"
  | Mode_annotations -> "mode annotations"
  | Mutable_variables -> "mutable variables"
  | Mutation -> "mutation"
  | Objects -> "objects"
  | Omitted_arguments -> "applications with omitted arguments"
  | Overwrite -> "overwrite_"
  | Polymorphic_annotations -> "polymorphic type annotations"
  | Position_arguments -> "source position arguments"
  | Primitive kind -> describe_primitive kind
  | Probes -> "probes"
  | Quotations -> "quotations"
  | Refutation_cases -> "refutation cases"
  | Source_positions -> "source positions"
  | Stack_allocation -> "stack_"
  | Values_through_applications -> "values through functor applications"

let report_error ppf = function
  | Unsupported what ->
    Format_doc.fprintf ppf "Laws do not support %s." (describe what)

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer ~loc report_error err)
    | _ -> None)
