module Diagnostic = Structured_diagnostic

module Domain = struct
  type t =
    | Mode
    | Kind
    | Type
    | Module
    | Attribute_or_extension
    | Unsupported
end

let typecore_domain : Typecore.error -> Domain.t = function
  | Invalid_atomic_loc_payload | Label_not_atomic _ | Atomic_in_pattern _
  | Modalities_on_atomic_field _ ->
    Type
  | Block_index_modality_mismatch _ | Submode_failed _
  | Curried_application_complete _ | Mode_mismatch _
  | Uncurried_function_escapes_comonadic _ | Uncurried_function_escapes_locality
  | Tail_call_local_returning | Exclave_in_nontail_position
  | Exclave_returns_not_local | Always_heap_allocation _
  | Always_static_allocation _ | Not_allocation | Overwrite_of_invalid_term ->
    Mode
  | Bad_tail_annotation _ -> Type
  | Non_value_object _ | Non_value_let_rec _ | Existential_jkind_mismatch _
  | Function_type_not_rep _ | Record_projection_not_rep _ | Record_not_rep _
  | Mutable_var_not_rep _ | Field_value_not_rep _
  | Constructor_arg_projection_not_rep _ | Constructor_arg_value_not_rep _
  | Impossible_function_jkind _ ->
    Kind
  | Label_not_mutable _ | Instance_variable_not_mutable _ | Unexpected_mutable _
  | Illegal_mutable_pat | Function_returns_local | Atomic_in_functional_update _
  | Mixed_record_atomic_loc _ | Polymorphic_atomic_loc _
  | Mutable_block_index_polymorphic_field _ | Useless_lpoly ->
    Attribute_or_extension
  | Constructor_arity_mismatch _ | Label_mismatch _ | Pattern_type_clash _
  | Or_pattern_type_clash _ | Multiply_bound_variable _ | Orpat_vars _
  | Expr_type_clash _ | Function_arity_type_clash _ | Apply_non_function _
  | Apply_wrong_label _ | Label_multiply_defined _ | Label_missing _
  | Wrong_name _ | Name_type_mismatch _ | Invalid_format _ | Not_an_object _
  | Undefined_method _ | Undefined_self_method _ | Virtual_class _
  | Private_type _ | Private_label _ | Private_constructor _
  | Unbound_instance_variable _ | Not_subtype _ | Outside_class
  | Value_multiply_overridden _ | Coercion_failure _ | Not_a_function _
  | Too_many_arguments _ | Abstract_wrong_label _ | Scoping_let_module _
  | Not_a_polymorphic_variant_type _ | Incoherent_label_order | Less_general _
  | Modules_not_allowed | Cannot_infer_signature | Not_a_packed_module _
  | Unexpected_existential _ | Invalid_interval | Invalid_for_loop_index
  | Invalid_comprehension_for_range_iterator_index | No_value_clauses
  | Exception_pattern_disallowed
  | Mixed_value_and_exception_patterns_under_guard
  | Effect_pattern_below_toplevel | Invalid_continuation_pattern
  | Inlined_record_escape | Inlined_record_expected | Unrefuted_pattern _
  | Invalid_extension_constructor_payload | Not_an_extension_constructor
  | Probe_format | Probe_name_format _ | Probe_name_undefined _
  | Probe_is_enabled_format | Extension_not_enabled _ | Literal_overflow _
  | Unknown_literal _ | Float32_literal _ | Int8_literal _ | Int16_literal _
  | Untagged_char_literal _ | Illegal_letrec_pat | Illegal_letrec_expr
  | Mixed_poly_nonpoly_bindings | Illegal_class_expr | Letop_type_clash _
  | Andop_type_clash _ | Bindings_type_clash _ | Unbound_existential _
  | Bind_existential _ | Missing_type_constraint | Wrong_expected_kind _
  | Expr_not_a_record_type _ | Constructor_labeled_arg
  | Partial_tuple_pattern_bad_type | Extra_tuple_label _ | Missing_tuple_label _
  | Repeated_tuple_exp_label _ | Repeated_tuple_pat_label _
  | Wrong_expected_record_boxing _ | Expr_record_type_has_wrong_boxing _
  | Invalid_unboxed_access _ | Block_access_bad_record _ | Optional_poly_param
  | Unboxed_int_literals_not_supported | Invalid_label_for_src_pos _
  | Nonoptional_call_pos_label _ | Unexpected_hole
  | Let_poly_not_yet_implemented | Let_poly_not_syntactic_value
  | Layout_poly_inst_not_yet_supported _ | Function_type_escapes_partial_match _
    ->
    Unsupported

let typemod_domain : Typemod.error -> Domain.t = function
  | Not_included _ | Not_included_functor _ | With_mismatch _
  | With_makes_applicative_functor_ill_typed _ | Strengthening_mismatch _ ->
    Module
  | Cannot_apply _ | Cannot_eliminate_dependency _ | Signature_expected
  | Structure_expected _ | Functor_expected _ | Signature_parameter_expected _
  | Signature_result_expected _ | Recursive_include_functor
  | With_no_component _ | With_changes_module_alias _
  | With_cannot_remove_constrained_type | With_package_manifest _
  | Repeated_name _ | Non_generalizable _ | Non_generalizable_module _
  | Implementation_is_required _ | Interface_not_compiled _
  | Not_allowed_in_functor_body _ | Not_includable_in_functor_body _
  | Not_a_packed_module _ | Incomplete_packed_module _ | Scoping_pack _
  | Recursive_module_require_explicit_type | Apply_generative
  | Cannot_scrape_alias _ | Cannot_scrape_package_type _
  | Badly_formed_signature _ | Cannot_hide_id _ | Invalid_type_subst_rhs
  | Non_packable_local_modtype_subst _ | With_cannot_remove_packed_modtype _
  | Cannot_alias _ | Cannot_pack_parameter
  | Compiling_as_parameterised_parameter
  | Cannot_compile_implementation_as_parameter | Cannot_implement_parameter _
  | Argument_for_non_parameter _ | Cannot_find_argument_type _
  | Inconsistent_argument_types _ | Duplicate_parameter_name _ ->
    Unsupported

let typedecl_domain : Typedecl.error -> Domain.t = function
  | Unboxed_mutable_label | Atomic_field_must_be_mutable _
  | Non_value_atomic_field ->
    Type
  | Definition_mismatch _ | Unsafe_mode_crossing_on_invalid_type_kind
  | Constructor_submode_failed _ ->
    Mode
  | Jkind_mismatch_of_type _ | Jkind_mismatch_of_path _
  | Jkind_mismatch_due_to_bad_inference _ | Jkind_sort _ | Jkind_empty_record
  | Non_representable_in_module _ | Invalid_jkind_in_block _ | Illegal_baggage _
  | Recursive_jkind_definition _ ->
    Kind
  | Local_not_enabled | Zero_alloc_attr_unsupported _
  | Zero_alloc_attr_non_function | Zero_alloc_attr_bad_user_arity
  | Missing_immediate_all_void_constructor_attribute _ ->
    Attribute_or_extension
  | Repeated_parameter | Duplicate_constructor _ | Too_many_constructors
  | Duplicate_label _ | Recursive_abbrev _ | Cycle_in_def _
  | Unboxed_recursion _ | Constraint_failed _ | Inconsistent_constraint _
  | Type_clash _ | Non_regular _ | Null_arity_external | Missing_native_external
  | Unbound_type_var _ | Cannot_extend_private_type _ | Not_extensible_type _
  | Extension_mismatch _ | Rebind_wrong_type _ | Rebind_mismatch _
  | Rebind_private _ | Variance _ | Unavailable_type_constructor _
  | Unbound_type_var_ext _ | Val_in_structure | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type _ | Deep_unbox_or_untag_attribute _
  | Illegal_mixed_product _ | Separability _ | Bad_unboxed_attribute _
  | Poly_not_yet_implemented | Boxed_and_unboxed | Nonrec_gadt
  | Invalid_private_row_declaration _ | Unexpected_layout_any_in_primitive _
  | Useless_layout_poly | Bad_or_null_attribute _ | Invalid_reexport _
  | Non_abstract_reexport _ | No_unboxed_version _ | Layout_poly_unsupported
  | Layout_poly_variable_representation | Misplaced_flatten_floats
  | Bad_represent_as_float_array_attribute ->
    Unsupported

let typetexp_domain : Typetexp.error -> Domain.t = function
  | Bad_jkind_annot _ | Bad_univar_jkind _ | Non_value _ | Non_sort _
  | Mismatched_jkind_annotation _ ->
    Kind
  | Unbound_type_variable _ | No_type_wildcards _ | Undefined_type_constructor _
  | Type_arity_mismatch _ | Bound_type_variable _ | Recursive_type
  | Type_mismatch _ | Alias_type_mismatch _ | Present_has_conjunction _
  | Present_has_no_type _ | Constructor_mismatch _ | Not_a_variant _
  | Variant_tags _ | Invalid_variable_name _ | Cannot_quantify _
  | Multiple_constraints_on_type _ | Method_mismatch _ | Opened_object _
  | Not_an_object _ | Repeated_tuple_label _ | Unsupported_extension _
  | Polymorphic_optional_param | Did_you_mean_unboxed _
  | Invalid_label_for_call_pos _ | Invalid_variable_stage _ | Lpoly_unsupported
  | Val_poly_and_layout ->
    Unsupported

let diagnostic_of_report report : Diagnostic.t =
  let printer = Location.batch_mode_printer in
  let text =
    Format.asprintf "%a"
      (fun ppf report -> printer.pp printer ppf report)
      report
    |> String.trim
  in
  { loc = report.Location.main.loc;
    entities = Diagnostic.Entities.empty;
    glossary = Diagnostic.Glossary.empty;
    body =
      [ { kind = Diagnostic.Kind.Explanation;
          content = [Diagnostic.Inline.Text text];
          children = []
        } ]
  }

let mode_input_of_exception exn =
  let embedded () = Mode_diagnostics.Embedded_mode_error exn in
  let mode_or_embedded domain direct =
    match (domain : Domain.t) with
    | Mode | Module -> direct
    | Kind | Type | Attribute_or_extension | Unsupported -> embedded ()
  in
  match exn with
  | Typecore.Error (loc, env, error) ->
    mode_or_embedded (typecore_domain error)
      (Mode_diagnostics.Typecore_error { loc; env; error })
  | Typemod.Error (loc, env, error) ->
    mode_or_embedded (typemod_domain error)
      (Mode_diagnostics.Typemod_error { loc; env; error })
  | Includemod.Apply_error { env; app_name; mty_f; args; _ } ->
    Mode_diagnostics.Includemod_apply_error { env; app_name; mty_f; args }
  | Typedecl.Error (loc, error) ->
    mode_or_embedded (typedecl_domain error)
      (Mode_diagnostics.Typedecl_error { loc; error })
  | Typetexp.Error (_loc, _env, error) ->
    let (_ : Domain.t) = typetexp_domain error in
    embedded ()
  | Env.Error
      (Env.Lookup_error (loc, _env, (Env.Local_value_used_in_exclave _ as error)))
  | Env.Error
      (Env.Lookup_error
         (loc, _env, (Env.Mutable_value_used_in_closure _ as error))) ->
    Mode_diagnostics.Env_lookup_error { loc; error }
  | Uniqueness_analysis.Usage.Unique_use_during_borrowing error ->
    Mode_diagnostics.Unique_use_during_borrowing error
  | Uniqueness_analysis.Error error -> Mode_diagnostics.Uniqueness_error error
  | _ -> embedded ()

let mode_diagnostic_of_exception report exn =
  let loc = report.Location.main.loc in
  let source = Diagnostic_source.load loc in
  let input = mode_input_of_exception exn in
  Mode_diagnostics.diagnose_without_context ~source ~loc input

let kind_error_of_exception = function
  | Typetexp.Error
      (loc, _env, Typetexp.Bad_jkind_annot (_type_expression, error)) ->
    Some
      (Kind_diagnostics.Crossing
         { loc; subject = "this type"; error = Ikind.Jkind_error error })
  | Typedecl.Error (loc, Typedecl.Jkind_mismatch_of_type (_env, _type, error))
    ->
    Some (Kind_diagnostics.Crossing { loc; subject = "this type"; error })
  | Typedecl.Error (loc, Typedecl.Jkind_mismatch_of_path (_env, path, error)) ->
    Some
      (Kind_diagnostics.Crossing
         { loc; subject = "type " ^ Path.name path; error })
  | _ -> None

let kind_diagnostic_of_exception report exn =
  match kind_error_of_exception exn with
  | None -> None
  | Some error ->
    Option.map
      (Diagnostic_plan.to_diagnostic ~loc:report.Location.main.loc)
      (Kind_diagnostics.diagnose error)

let type_error_of_exception = function
  | Typedecl.Error (loc, Typedecl.Atomic_field_must_be_mutable name) ->
    Some (Type_diagnostics.Atomic_field_must_be_mutable { loc; name })
  | Typedecl.Error (loc, Typedecl.Non_value_atomic_field) ->
    Some (Type_diagnostics.Non_value_atomic_field loc)
  | Typedecl.Error (loc, Typedecl.Unboxed_mutable_label) ->
    Some (Type_diagnostics.Mutable_field_in_unboxed_record loc)
  | Typecore.Error (loc, _env, Typecore.Atomic_in_pattern field) ->
    Some (Type_diagnostics.Atomic_field_in_pattern { loc; field })
  | Typecore.Error (loc, _env, Typecore.Label_not_atomic field) ->
    Some (Type_diagnostics.Non_atomic_field_access { loc; field })
  | Typecore.Error (loc, _env, Typecore.Modalities_on_atomic_field field) ->
    Some (Type_diagnostics.Modalities_on_atomic_field { loc; field })
  | Typecore.Error (loc, _env, Typecore.Invalid_atomic_loc_payload) ->
    Some (Type_diagnostics.Invalid_atomic_access loc)
  | Typecore.Error (loc, _env, Typecore.Bad_tail_annotation kind) ->
    Some (Type_diagnostics.Bad_tail_annotation { loc; kind })
  | _ -> None

let type_diagnostic_of_exception report exn =
  Option.map
    (fun error ->
      Type_diagnostics.diagnose error
      |> Diagnostic_plan.to_diagnostic ~loc:report.Location.main.loc)
    (type_error_of_exception exn)

let domain_handlers =
  [ kind_diagnostic_of_exception;
    type_diagnostic_of_exception;
    mode_diagnostic_of_exception ]

let rec first_diagnostic report exn = function
  | [] -> None
  | handler :: handlers -> (
    match handler report exn with
    | Some diagnostic -> Some diagnostic
    | None -> first_diagnostic report exn handlers)

let structured_diagnostic_of_exception report exn =
  first_diagnostic report exn domain_handlers

let json_emitter ppf exn report =
  let diagnostic =
    match exn with
    | Some exn -> (
      match structured_diagnostic_of_exception report exn with
      | Some diagnostic -> diagnostic
      | None -> diagnostic_of_report report)
    | None -> diagnostic_of_report report
  in
  Format.fprintf ppf "%s@." (Diagnostic.to_json diagnostic)

let enable_structured_diagnostics () =
  Clflags.structured_diagnostics := true;
  Location.set_emitter json_emitter
