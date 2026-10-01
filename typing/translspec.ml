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

open Asttypes
open Typedtree
open Spec

type unsupported =
  | Arrays
  | Atomic_fields
  | Binding_operators
  | Block_indices
  | Borrow
  | Coercions
  | Comprehensions
  | Effect_handlers
  | Exclave
  | Existential_patterns
  | First_class_modules
  | Layout_polymorphism
  | Local_exceptions
  | Local_modules
  | Local_opens
  | Locally_abstract_types
  | Loops
  | Mutable_variables
  | Mutation
  | Objects
  | Overwrite
  | Polymorphic_annotations
  | Position_arguments
  | Probes
  | Quotations
  | Refutation_cases
  | Source_locations
  | Source_positions
  | Stack_allocation
  | Values_through_applications

type error =
  | Unsupported of unsupported

exception Error of Location.t * error

let unsupported loc what = raise (Error (loc, Unsupported what))

(* Sorts and modes *)

let sort s = Jkind.Sort.default_for_transl_and_get s

let locality_r m = locality_mode_r_zap_to_ceil m

let locality_l m = locality_mode_l_zap_to_floor m

let return_mode m = return_mode_zap_to_floor_exn m

(* Constants and partiality *)

let constant (c : Typedtree.constant) : Spec.constant =
  match c with
  | Const_int n -> Const_int n
  | Const_char c -> Const_char c
  | Const_untagged_char c -> Const_untagged_char c
  | Const_string (s, loc, d) -> Const_string (s, loc, d)
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

let partial (p : Typedtree.partial) : Spec.partial =
  match p with
  | Partial -> Partial
  | Total -> Total

(* Labels of arguments *)

let arg_label loc (lbl : Types.arg_label) : Asttypes.arg_label =
  match lbl with
  | Nolabel -> Nolabel
  | Labelled s -> Labelled s
  | Optional s -> Optional s
  | Position _ -> unsupported loc Position_arguments

(* Constructors and labels *)

(* A functor application names no particular instance of its values. *)
let check_global loc (path : Path.t) =
  if Path.contains_apply path then unsupported loc Values_through_applications

let global loc path = check_global loc path; path

let constructor loc (cstr : Data_types.constructor_description) =
  match cstr.cstr_tag with
  | Extension path -> Extension_constructor (global loc path)
  | Ordinary _ | Null ->
      Constructor
        { type_path = Data_types.cstr_res_type_path cstr;
          name = cstr.cstr_name }

let label (lbl : _ Data_types.gen_label_description) =
  { type_path = Data_types.gen_lbl_res_type_path lbl; name = lbl.lbl_name }

(* Patterns *)

let rec pattern : type k. k general_pattern -> _ Spec.pattern = fun p ->
  List.iter
    (fun (extra, loc, _) ->
       match extra with
       | Tpat_constraint (Some { ctyp_desc = Ttyp_poly _; _ }, _) ->
           unsupported loc Polymorphic_annotations
       | Tpat_constraint _ | Tpat_type _ | Tpat_inspected_type _
       | Tpat_open _ -> ()
       | Tpat_unpack -> unsupported loc First_class_modules)
    p.pat_extra;
  { spat_desc = pattern_desc p.pat_loc p.pat_desc;
    spat_type = p.pat_type;
    spat_loc = p.pat_loc }

and pattern_desc :
  type k. Location.t -> k Typedtree.pattern_desc -> _ Spec.pattern_desc =
  fun loc desc ->
  match desc with
  | Tpat_any -> Spat_any
  | Tpat_var { id; sort = s; _ } -> Spat_var (id, sort s)
  | Tpat_alias { pattern = p; id; sort = s; _ } ->
      Spat_alias (pattern p, id, sort s)
  | Tpat_fun_layout _ -> unsupported loc Layout_polymorphism
  | Tpat_constant c -> Spat_constant (constant c)
  | Tpat_unboxed_unit -> Spat_unboxed_unit
  | Tpat_unboxed_bool b -> Spat_unboxed_bool b
  | Tpat_tuple ps -> Spat_tuple (List.map (fun (l, p) -> (l, pattern p)) ps)
  | Tpat_unboxed_tuple ps ->
      Spat_unboxed_tuple
        (List.map (fun (l, p, s) -> (l, pattern p, sort s)) ps)
  | Tpat_construct (_, cstr, _, args, constr) ->
      if not (List.is_empty cstr.cstr_existentials) then
        unsupported loc Existential_patterns;
      (match constr with
       | Some (_ :: _, _) -> unsupported loc Locally_abstract_types
       | None | Some ([], _) -> ());
      Spat_construct
        (constructor loc cstr,
         List.map (fun (s, p) -> (sort s, pattern p)) args)
  | Tpat_variant (lbl, arg, _) -> Spat_variant (lbl, Option.map pattern arg)
  | Tpat_record (fields, _, closed) ->
      Spat_record
        (List.map (fun (_, lbl, p) -> (label lbl, pattern p)) fields, closed)
  | Tpat_record_unboxed_product (fields, _, closed) ->
      Spat_record_unboxed_product
        (List.map (fun (_, lbl, p) -> (label lbl, pattern p)) fields, closed)
  | Tpat_array _ -> unsupported loc Arrays
  | Tpat_lazy p -> Spat_lazy (pattern p)
  | Tpat_value p -> (pattern (p :> value general_pattern)).spat_desc
  | Tpat_exception p -> Spat_exception (pattern p)
  | Tpat_or (p1, p2, _) -> Spat_or (pattern p1, pattern p2)

(* Expressions. [bound] contains the local variables in scope. *)

let bind bound ids = List.fold_left (fun s id -> Ident.Set.add id s) bound ids

let bind_pattern bound (p : _ Spec.pattern) =
  bind bound (Spec.pattern_variables p)

let check_extra loc extra =
  match extra with
  | Texp_constraint _ | Texp_mode _ | Texp_inspected_type _ -> ()
  | Texp_poly _ -> unsupported loc Polymorphic_annotations
  | Texp_coerce _ -> unsupported loc Coercions
  | Texp_newtype _ -> unsupported loc Locally_abstract_types
  | Texp_stack -> unsupported loc Stack_allocation
  | Texp_borrowed -> unsupported loc Borrow
  | Texp_ghost_region -> unsupported loc Borrow

let rec path_of_module (mexp : module_expr) =
  match mexp.mod_desc with
  | Tmod_ident (p, _) -> Some p
  | Tmod_apply (f, arg, _, _, _) ->
      Option.bind (path_of_module f) (fun f ->
        Option.map (fun arg -> Path.Papply (f, arg)) (path_of_module arg))
  | Tmod_constraint (mexp, _, Tmodtype_implicit, _) -> path_of_module mexp
  | Tmod_structure _ | Tmod_functor _ | Tmod_apply_unit _ | Tmod_unpack _
  | Tmod_constraint (_, _, Tmodtype_explicit _, _) ->
      None

let rec expression bound (e : Typedtree.expression) =
  List.iter (fun (extra, loc, _) -> check_extra loc extra) e.exp_extra;
  { sexp_desc = expression_desc bound e.exp_loc e.exp_desc;
    sexp_type = e.exp_type;
    sexp_loc = e.exp_loc }

and expression_desc bound loc desc =
  match desc with
  | Texp_ident { path = Pident id; _ } when Ident.Set.mem id bound ->
      Sexp_var id
  | Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ }
    when String.starts_with ~prefix:"%loc_" prim.prim_name ->
      (* [__LINE__] and the like depend on the location of their
         occurrence (see [Translprim]). *)
      unsupported loc Source_locations
  | Texp_ident { path; _ } -> Sexp_global (global loc path)
  | Texp_apply_layout _ -> unsupported loc Layout_polymorphism
  | Texp_constant c -> Sexp_constant (constant c)
  | Texp_let (rf, vbs, body) ->
      let pats = List.map (fun vb -> pattern vb.vb_pat) vbs in
      let bound_body = List.fold_left bind_pattern bound pats in
      let bound_rhs =
        match rf with Recursive -> bound_body | Nonrecursive -> bound
      in
      let vbs =
        List.map2
          (fun vb svb_pat ->
             { svb_pat;
               svb_expr = expression bound_rhs vb.vb_expr;
               svb_rec_kind = vb.vb_rec_kind;
               svb_sort = sort vb.vb_sort })
          vbs pats
      in
      Sexp_let (rf, vbs, expression bound_body body)
  | Texp_letmutable _ -> unsupported loc Mutable_variables
  | Texp_function { params; body; ret_mode; ret_sort; locality_mode; _ } ->
      let bound, params = function_params bound params in
      let body = function_body bound body in
      Sexp_function
        { params;
          body;
          ret_sort = sort ret_sort;
          ret_mode = return_mode ret_mode.mode_modes;
          locality = locality_r locality_mode }
  | Texp_apply (funct, args, _, ret_mode, _, _) ->
      let args =
        List.map
          (fun (lbl, (arg : apply_arg)) ->
             let lbl = arg_label loc lbl in
             match arg with
             | Omitted _ -> (lbl, Omitted)
             | Arg (e, s) -> (lbl, Arg (expression bound e, sort s)))
          args
      in
      Sexp_apply
        { funct = expression bound funct;
          args;
          ret_mode = return_mode ret_mode }
  | Texp_match (scrutinee, s, cases, effect_cases, p) ->
      if not (List.is_empty effect_cases) then unsupported loc Effect_handlers;
      Sexp_match
        { scrutinee = expression bound scrutinee;
          sort = sort s;
          cases = List.map (case bound) cases;
          partial = partial p }
  | Texp_try (e, cases, effect_cases) ->
      if not (List.is_empty effect_cases) then unsupported loc Effect_handlers;
      Sexp_try (expression bound e, List.map (case bound) cases)
  | Texp_unboxed_unit -> Sexp_unboxed_unit
  | Texp_unboxed_bool b -> Sexp_unboxed_bool b
  | Texp_tuple (es, mode) ->
      Sexp_tuple
        (List.map (fun (l, e) -> (l, expression bound e)) es,
         locality_r mode)
  | Texp_unboxed_tuple es ->
      Sexp_unboxed_tuple
        (List.map (fun (l, e, s) -> (l, expression bound e, sort s)) es)
  | Texp_construct (_, cstr, _, args, mode) ->
      Sexp_construct
        { constructor = constructor loc cstr;
          args = List.map (fun (s, e) -> (expression bound e, sort s)) args;
          locality = Option.map locality_r mode }
  | Texp_variant (lbl, arg) ->
      Sexp_variant
        (lbl,
         Option.map (fun (e, mode) -> (expression bound e, locality_r mode))
           arg)
  | Texp_record { fields; extended_expression; locality_mode; _ } ->
      Sexp_record
        { fields = record_fields bound fields;
          extended_expression =
            Option.map
              (fun (e, s, _, _) -> (expression bound e, sort s))
              extended_expression;
          locality = Option.map locality_r locality_mode }
  | Texp_record_unboxed_product { fields; extended_expression; _ } ->
      Sexp_record_unboxed_product
        { fields = record_fields bound fields;
          extended_expression =
            Option.map
              (fun (e, s) -> (expression bound e, sort s))
              extended_expression }
  | Texp_atomic_loc _ -> unsupported loc Atomic_fields
  | Texp_field { record; record_sort; label = lbl; _ } ->
      Sexp_field
        { record = expression bound record;
          sort = sort record_sort;
          label = label lbl }
  | Texp_unboxed_field { record; record_sort; label = lbl; _ } ->
      Sexp_unboxed_field
        { record = expression bound record;
          sort = sort record_sort;
          label = label lbl }
  | Texp_setfield _ -> unsupported loc Mutation
  | Texp_array _ -> unsupported loc Arrays
  | Texp_idx _ -> unsupported loc Block_indices
  | Texp_list_comprehension _ | Texp_array_comprehension _ ->
      unsupported loc Comprehensions
  | Texp_ifthenelse (c, t, e) ->
      Sexp_ifthenelse
        (expression bound c, expression bound t,
         Option.map (expression bound) e)
  | Texp_sequence (e1, s, e2) ->
      Sexp_sequence (expression bound e1, sort s, expression bound e2)
  | Texp_while _ | Texp_for _ -> unsupported loc Loops
  | Texp_send _ | Texp_new _ | Texp_instvar _ | Texp_setinstvar _
  | Texp_override _ | Texp_object _ ->
      unsupported loc Objects
  | Texp_mutvar _ | Texp_setmutvar _ -> unsupported loc Mutable_variables
  | Texp_letmodule (Some id, _, _, mexp, body) -> begin
      (* [let module M = P in e], with [P] a module path, stands for [e]
         with [M] replaced by [P]. *)
      match path_of_module mexp with
      | None -> unsupported loc Local_modules
      | Some path ->
          let s = Subst.add_module id path Subst.identity in
          let body =
            Spec.map ~ty:(Subst.type_expr s) ~value_path:(Subst.value_path s)
              ~type_path:(Subst.type_path s) (expression bound body)
          in
          List.iter
            (fun ((ns : Spec.namespace), p) ->
               match ns with
               | Value | Extension -> check_global loc p
               | Type -> ())
            (Spec.paths body);
          body.sexp_desc
    end
  | Texp_letmodule (None, _, _, _, _) -> unsupported loc Local_modules
  | Texp_letexception _ -> unsupported loc Local_exceptions
  | Texp_assert (e, _) -> Sexp_assert (expression bound e)
  | Texp_lazy e -> Sexp_lazy (expression bound e)
  | Texp_pack _ -> unsupported loc First_class_modules
  | Texp_letop _ -> unsupported loc Binding_operators
  | Texp_unreachable -> unsupported loc Refutation_cases
  | Texp_extension_constructor (_, path) ->
      Sexp_extension_constructor (global loc path)
  | Texp_open (od, e) -> begin
      match path_of_module od.open_expr with
      | Some path when not (Path.contains_apply path) ->
          (expression bound e).sexp_desc
      | Some _ | None -> unsupported loc Local_opens
    end
  | Texp_probe _ | Texp_probe_is_enabled _ -> unsupported loc Probes
  | Texp_exclave _ -> unsupported loc Exclave
  | Texp_src_pos -> unsupported loc Source_positions
  | Texp_overwrite _ | Texp_hole _ -> unsupported loc Overwrite
  | Texp_quote _ | Texp_splice _ -> unsupported loc Quotations

and record_fields :
  type r. _ -> (r Data_types.gen_label_description * _ * _) array -> _ =
  fun bound fields ->
  Array.to_list fields
  |> List.map (fun (lbl, s, (def : record_label_definition)) ->
       let def : _ Spec.field_definition =
         match def with
         | Kept (ty, _, _) -> Kept ty
         | Overridden (_, e) -> Overridden (expression bound e)
       in
       (label lbl, sort s, def))

and case : type k. _ -> k Typedtree.case -> _ Spec.case =
  fun bound c ->
  (match c.c_cont with
   | None -> ()
   | Some _ -> unsupported c.c_rhs.exp_loc Effect_handlers);
  let sc_lhs = pattern c.c_lhs in
  let bound = bind_pattern bound sc_lhs in
  { sc_lhs;
    sc_guard = Option.map (expression bound) c.c_guard;
    sc_rhs = expression bound c.c_rhs }

and function_params bound params =
  let bound, rev_params =
    List.fold_left
      (fun (bound, acc) (fp : Typedtree.function_param) ->
         if not (List.is_empty fp.fp_newtypes) then
           unsupported fp.fp_loc Locally_abstract_types;
         let sfp_arg_label = arg_label fp.fp_loc fp.fp_arg_label in
         let bound = Ident.Set.add fp.fp_param bound in
         let sfp_kind, bound =
           match fp.fp_kind with
           | Tparam_pat p ->
               let p = pattern p in
               Sparam_pat p, bind_pattern bound p
           | Tparam_optional_default (p, default, s) ->
               (* The default is evaluated in the scope of the preceding
                  parameters. *)
               let default = expression bound default in
               let p = pattern p in
               Sparam_optional_default (p, default, sort s),
               bind_pattern bound p
         in
         let param =
           { sfp_arg_label;
             sfp_param = fp.fp_param;
             sfp_kind;
             sfp_sort = sort fp.fp_sort;
             sfp_mode = locality_l fp.fp_mode.mode_modes;
             sfp_partial = partial fp.fp_partial }
         in
         bound, param :: acc)
      (bound, []) params
  in
  bound, List.rev rev_params

and function_body bound body =
  match body with
  | Tfunction_body e -> Sfunction_body (expression bound e)
  | Tfunction_cases fc ->
      List.iter (check_extra fc.fc_loc) fc.fc_exp_extra;
      let bound = Ident.Set.add fc.fc_param bound in
      Sfunction_cases
        { cases = List.map (case bound) fc.fc_cases;
          param = fc.fc_param;
          arg_sort = sort fc.fc_arg_sort;
          arg_mode = locality_l fc.fc_arg_mode;
          ret_type = fc.fc_ret_type;
          partial = partial fc.fc_partial }

let expression ~bound e = expression bound e

(* Errors *)

let describe = function
  | Arrays -> "arrays"
  | Atomic_fields -> "atomic fields"
  | Binding_operators -> "binding operators"
  | Block_indices -> "block indices"
  | Borrow -> "borrow_"
  | Coercions -> "coercions"
  | Comprehensions -> "comprehensions"
  | Effect_handlers -> "effect handlers"
  | Exclave -> "exclave_"
  | Existential_patterns -> "constructors with existential types in patterns"
  | First_class_modules -> "first-class modules"
  | Layout_polymorphism -> "layout-polymorphic bindings"
  | Local_exceptions -> "local exceptions"
  | Local_modules -> "local modules that are not module paths"
  | Local_opens -> "local opens that are not of module paths"
  | Locally_abstract_types -> "locally abstract types"
  | Loops -> "loops"
  | Mutable_variables -> "mutable variables"
  | Mutation -> "mutation"
  | Objects -> "objects"
  | Overwrite -> "overwrite_"
  | Polymorphic_annotations -> "polymorphic type annotations"
  | Position_arguments -> "source position arguments"
  | Probes -> "probes"
  | Quotations -> "quotations"
  | Refutation_cases -> "refutation cases"
  | Source_locations -> "source locations"
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
