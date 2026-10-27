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
  | Assertions
  | Atomic_fields
  | Attributes
  | Binding_operators
  | Block_indices
  | Borrow
  | Coercions
  | Comprehensions
  | Conditionals
  | Exception_handlers
  | Exclave
  | Extension_constructors
  | Field_accesses
  | First_class_modules
  | Functions
  | Layout_polymorphism
  | Lazy_values
  | Let_bindings
  | Local_exceptions
  | Local_modules
  | Local_opens
  | Locally_abstract_types
  | Loops
  | Mode_annotations
  | Mutable_variables
  | Mutation
  | Objects
  | Omitted_arguments
  | Overwrite
  | Pattern_matching
  | Polymorphic_annotations
  | Position_arguments
  | Primitive of primitive_kind
  | Probes
  | Quotations
  | Records
  | Refutation_cases
  | Sequences
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
  | Texp_let _ -> unsupported loc Let_bindings
  | Texp_letmutable _ -> unsupported loc Mutable_variables
  | Texp_function _ -> unsupported loc Functions
  | Texp_match _ -> unsupported loc Pattern_matching
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
  | Texp_record _ | Texp_record_unboxed_product _ -> unsupported loc Records
  | Texp_atomic_loc _ -> unsupported loc Atomic_fields
  | Texp_field _ | Texp_unboxed_field _ -> unsupported loc Field_accesses
  | Texp_setfield _ -> unsupported loc Mutation
  | Texp_array _ -> unsupported loc Arrays
  | Texp_idx _ -> unsupported loc Block_indices
  | Texp_list_comprehension _ | Texp_array_comprehension _ ->
    unsupported loc Comprehensions
  | Texp_ifthenelse _ -> unsupported loc Conditionals
  | Texp_sequence _ -> unsupported loc Sequences
  | Texp_while _ | Texp_for _ -> unsupported loc Loops
  | Texp_send _ | Texp_new _ | Texp_instvar _ | Texp_setinstvar _
  | Texp_override _ | Texp_object _ ->
    unsupported loc Objects
  | Texp_mutvar _ | Texp_setmutvar _ -> unsupported loc Mutable_variables
  | Texp_letmodule _ -> unsupported loc Local_modules
  | Texp_letexception _ -> unsupported loc Local_exceptions
  | Texp_assert _ -> unsupported loc Assertions
  | Texp_lazy _ -> unsupported loc Lazy_values
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

let sort = Jkind.Sort.default_for_transl_and_get

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

(* Typed expressions *)

let typedtree_arg_label : Spec.arg_label -> Types.arg_label = function
  | Nolabel -> Nolabel
  | Labelled s -> Labelled s
  | Optional s -> Optional s

let typedtree_extra : spec_extra -> exp_extra = function
  | Stexp_constraint cty -> Texp_constraint cty
  | Stexp_inspected_type inspection -> Texp_inspected_type inspection

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

and typedtree_labelled (lbl, e, sort) = lbl, typedtree e, sort

and typedtree_argument (lbl, e, sort) =
  typedtree_arg_label lbl, Arg (typedtree e, sort)

(* Errors *)

let describe_primitive = function Location -> "location primitives"

let describe = function
  | Arrays -> "arrays"
  | Assertions -> "assertions"
  | Atomic_fields -> "atomic fields"
  | Attributes -> "attributes"
  | Binding_operators -> "binding operators"
  | Block_indices -> "block indices"
  | Borrow -> "borrow_"
  | Coercions -> "coercions"
  | Comprehensions -> "comprehensions"
  | Conditionals -> "conditionals"
  | Exception_handlers -> "exception handlers"
  | Exclave -> "exclave_"
  | Extension_constructors -> "extension constructors"
  | Field_accesses -> "field accesses"
  | First_class_modules -> "first-class modules"
  | Functions -> "functions"
  | Layout_polymorphism -> "layout-polymorphic bindings"
  | Lazy_values -> "lazy values"
  | Let_bindings -> "let-bindings"
  | Local_exceptions -> "local exceptions"
  | Local_modules -> "local modules"
  | Local_opens -> "local opens"
  | Locally_abstract_types -> "locally abstract types"
  | Loops -> "loops"
  | Mode_annotations -> "mode annotations"
  | Mutable_variables -> "mutable variables"
  | Mutation -> "mutation"
  | Objects -> "objects"
  | Omitted_arguments -> "applications with omitted arguments"
  | Overwrite -> "overwrite_"
  | Pattern_matching -> "pattern matching"
  | Polymorphic_annotations -> "polymorphic type annotations"
  | Position_arguments -> "source position arguments"
  | Primitive kind -> describe_primitive kind
  | Probes -> "probes"
  | Quotations -> "quotations"
  | Records -> "records"
  | Refutation_cases -> "refutation cases"
  | Sequences -> "sequences"
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
