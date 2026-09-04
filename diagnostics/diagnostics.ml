module Diagnostic = Structured_diagnostic

let diagnostic_of_report report : Diagnostic.t =
  let printer = Location.batch_mode_printer in
  let text =
    Format.asprintf "%a"
      (fun ppf report -> printer.pp printer ppf report)
      report
    |> String.trim
  in
  { loc = report.Location.main.loc;
    body =
      [ { kind = Diagnostic.Kind.Explanation;
          content = [Diagnostic.Inline.Text text];
          children = []
        } ]
  }

let realize report stories =
  Diagnostic_term.realize ~loc:report.Location.main.loc stories

let mode_diagnostic report error =
  let loc = report.Location.main.loc in
  Mode_diagnostics.diagnose ~loc error

let module_diagnostic report error =
  let loc = report.Location.main.loc in
  Module_diagnostics.diagnose ~loc error

let kind_diagnostic report error =
  Option.map (realize report) (Kind_diagnostics.diagnose error)

let type_diagnostic report error =
  Some (realize report (Type_diagnostics.diagnose error))

let expression_diagnostic report loc error =
  mode_diagnostic report (Mode_diagnostics.Expression_error { loc; error })

let direct_mode_diagnostic report exn =
  let add_step ~mode ~pinpoint ~hint steps =
    { Mode_diagnostics.mode; pinpoint; kind = hint } :: steps
  in
  Option.bind (Mode.fold_error_exn ~init:[] ~step:add_step exn) (fun axes ->
      mode_diagnostic report (Mode_diagnostics.Folded_mismatch axes))

let typetexp_diagnostic _report _loc (error : Typetexp.error) =
  let open Typetexp in
  match error with
  | Bad_jkind_annot _ | Unbound_type_variable _ | No_type_wildcards _
  | Undefined_type_constructor _ | Type_arity_mismatch _ | Bound_type_variable _
  | Recursive_type | Type_mismatch _ | Alias_type_mismatch _
  | Present_has_conjunction _ | Present_has_no_type _ | Constructor_mismatch _
  | Not_a_variant _ | Variant_tags _ | Invalid_variable_name _
  | Cannot_quantify _ | Bad_univar_jkind _ | Multiple_constraints_on_type _
  | Method_mismatch _ | Opened_object _ | Not_an_object _
  | Repeated_tuple_label _ | Unsupported_extension _
  | Polymorphic_optional_param | Non_value _ | Non_sort _
  | Did_you_mean_unboxed _ | Invalid_label_for_call_pos _
  | Invalid_variable_stage _ | Mismatched_jkind_annotation _ | Lpoly_unsupported
  | Val_poly_and_layout ->
    None

let typedecl_diagnostic report loc (error : Typedecl.error) =
  let open Typedecl in
  match error with
  | Jkind_mismatch_of_type (_env, _type, error) ->
    kind_diagnostic report
      (Kind_diagnostics.Crossing { loc; subject = "this type"; error })
  | Jkind_mismatch_of_path (_env, path, error) ->
    kind_diagnostic report
      (Kind_diagnostics.Crossing
         { loc; subject = "type " ^ Path.name path; error })
  | Atomic_field_must_be_mutable name ->
    type_diagnostic report
      (Type_diagnostics.Atomic_field_must_be_mutable { loc; name })
  | Non_value_atomic_field ->
    type_diagnostic report (Type_diagnostics.Non_value_atomic_field loc)
  | Unboxed_mutable_label ->
    type_diagnostic report
      (Type_diagnostics.Mutable_field_in_unboxed_record loc)
  | Unsafe_mode_crossing_on_invalid_type_kind ->
    type_diagnostic report
      (Type_diagnostics.Unsafe_mode_crossing_on_invalid_type_kind loc)
  | Definition_mismatch (type_expr, env, Some mismatch) ->
    module_diagnostic report
      (Module_diagnostics.Type_definition_mismatch
         { loc; type_expr; env; mismatch })
  | Constructor_submode_failed error ->
    mode_diagnostic report
      (Mode_diagnostics.Constructor_submode_failed { loc; error })
  | Repeated_parameter | Duplicate_constructor _ | Too_many_constructors
  | Duplicate_label _ | Recursive_abbrev _ | Cycle_in_def _
  | Unboxed_recursion _
  | Definition_mismatch (_, _, None)
  | Constraint_failed _ | Inconsistent_constraint _ | Type_clash _
  | Non_regular _ | Null_arity_external | Missing_native_external
  | Unbound_type_var _ | Cannot_extend_private_type _ | Not_extensible_type _
  | Extension_mismatch _ | Rebind_wrong_type _ | Rebind_mismatch _
  | Rebind_private _ | Variance _ | Unavailable_type_constructor _
  | Unbound_type_var_ext _ | Val_in_structure | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type _ | Deep_unbox_or_untag_attribute _
  | Jkind_mismatch_due_to_bad_inference _ | Jkind_sort _ | Jkind_empty_record
  | Non_representable_in_module _ | Invalid_jkind_in_block _
  | Illegal_mixed_product _ | Separability _ | Bad_unboxed_attribute _
  | Poly_not_yet_implemented | Boxed_and_unboxed | Nonrec_gadt
  | Invalid_private_row_declaration _ | Local_not_enabled
  | Unexpected_layout_any_in_primitive _ | Useless_layout_poly
  | Bad_or_null_attribute _ | Zero_alloc_attr_unsupported _
  | Zero_alloc_attr_non_function | Zero_alloc_attr_bad_user_arity
  | Invalid_reexport _ | Non_abstract_reexport _ | Illegal_baggage _
  | No_unboxed_version _ | Layout_poly_unsupported
  | Layout_poly_variable_representation | Misplaced_flatten_floats
  | Recursive_jkind_definition _ | Bad_represent_as_float_array_attribute ->
    None

let typecore_diagnostic report loc (error : Typecore.error) =
  let open Typecore in
  match error with
  | Atomic_in_pattern field ->
    type_diagnostic report
      (Type_diagnostics.Atomic_field_in_pattern { loc; field })
  | Label_not_atomic field ->
    type_diagnostic report
      (Type_diagnostics.Non_atomic_field_access { loc; field })
  | Modalities_on_atomic_field field ->
    type_diagnostic report
      (Type_diagnostics.Modalities_on_atomic_field { loc; field })
  | Invalid_atomic_loc_payload ->
    type_diagnostic report (Type_diagnostics.Invalid_atomic_access loc)
  | Bad_tail_annotation kind ->
    type_diagnostic report (Type_diagnostics.Bad_tail_annotation { loc; kind })
  | Block_index_modality_mismatch { mut; err } ->
    expression_diagnostic report loc
      (Mode_diagnostics.Block_index_modality_mismatch
         { mutable_elements = mut; error = err })
  | Submode_failed (error, context) ->
    expression_diagnostic report loc
      (Mode_diagnostics.Submode_failed { error; context })
  | Curried_application_complete (label, error, part) ->
    expression_diagnostic report loc
      (Mode_diagnostics.Curried_application_complete { label; error; part })
  | Uncurried_function_escapes_comonadic error ->
    expression_diagnostic report loc
      (Mode_diagnostics.Uncurried_function_escapes_comonadic error)
  | Overwrite_of_invalid_term ->
    expression_diagnostic report loc Mode_diagnostics.Overwrite_of_invalid_term
  | Exclave_in_nontail_position ->
    expression_diagnostic report loc
      Mode_diagnostics.Exclave_in_nontail_position
  | Exclave_returns_not_local ->
    expression_diagnostic report loc Mode_diagnostics.Exclave_returns_not_local
  | Tail_call_local_returning ->
    expression_diagnostic report loc Mode_diagnostics.Tail_call_local_returning
  | Always_heap_allocation allocation ->
    expression_diagnostic report loc
      (Mode_diagnostics.Always_heap_allocation allocation)
  | Always_static_allocation allocation ->
    expression_diagnostic report loc
      (Mode_diagnostics.Always_static_allocation allocation)
  | Not_allocation ->
    expression_diagnostic report loc Mode_diagnostics.Not_allocation
  | Non_value_object _ | Non_value_let_rec _ | Existential_jkind_mismatch _
  | Function_type_not_rep _ | Record_projection_not_rep _ | Record_not_rep _
  | Mutable_var_not_rep _ | Field_value_not_rep _
  | Constructor_arg_projection_not_rep _ | Constructor_arg_value_not_rep _
  | Impossible_function_jkind _ | Label_not_mutable _
  | Instance_variable_not_mutable _ | Unexpected_mutable _ | Illegal_mutable_pat
  | Function_returns_local | Atomic_in_functional_update _
  | Mixed_record_atomic_loc _ | Polymorphic_atomic_loc _
  | Mutable_block_index_polymorphic_field _ | Useless_lpoly
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
  | Let_poly_not_yet_implemented | Let_poly_not_function
  | Layout_poly_inst_not_yet_supported _ | Function_type_escapes_partial_match _
  | Uncurried_function_escapes_locality ->
    None

let typemod_diagnostic report loc (error : Typemod.error) =
  let open Typemod in
  match error with
  | Not_included explanation | Not_included_functor explanation ->
    module_diagnostic report
      (Module_diagnostics.Not_included { loc; explanation })
  | Strengthening_mismatch (path, explanation) ->
    module_diagnostic report
      (Module_diagnostics.Strengthening_mismatch { loc; path; explanation })
  | With_makes_applicative_functor_ill_typed
      (constrained, type_path, explanation) ->
    module_diagnostic report
      (Module_diagnostics.Applicative_functor_mismatch
         { loc; constrained; type_path; explanation })
  | With_mismatch (path, explanation) ->
    module_diagnostic report
      (Module_diagnostics.Substitution_mismatch { loc; path; explanation })
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
    None

let env_lookup_diagnostic report loc (error : Env.lookup_error) =
  let open Env in
  match error with
  | Local_value_used_in_exclave description ->
    mode_diagnostic report
      (Mode_diagnostics.Local_value_used_in_exclave { loc; description })
  | Mutable_value_used_in_closure pinpoint ->
    mode_diagnostic report
      (Mode_diagnostics.Mutable_value_used_in_closure { loc; pinpoint })
  | Unbound_value _ | Unbound_type _ | Unbound_constructor _ | Unbound_label _
  | Unbound_module _ | Unbound_class _ | Unbound_modtype _ | Unbound_cltype _
  | Unbound_jkind _ | Unbound_settable_variable _ | Not_a_settable_variable _
  | Masked_instance_variable _ | Masked_self_variable _
  | Masked_ancestor_variable _ | Structure_used_as_functor _
  | Abstract_used_as_functor _ | Functor_used_as_structure _
  | Abstract_used_as_structure _ | Generative_used_as_applicative _
  | Illegal_reference_to_recursive_module _
  | Illegal_reference_to_recursive_class_type _ | Cannot_scrape_alias _
  | Non_value_used_in_object _ | No_unboxed_version _
  | Error_from_persistent_env _ | Incompatible_stage _ | Unbound_in_stage _ ->
    None

let env_diagnostic report (error : Env.error) =
  let open Env in
  match error with
  | Lookup_error (loc, _env, error) -> env_lookup_diagnostic report loc error
  | Missing_module _ | Illegal_value_name _ | Incomplete_instantiation _
  | Initial_stage_splice _ | Unsupported_inside_quotation _ | Cmi_not_found _ ->
    None

let structured_diagnostic_of_exception report = function
  | Typetexp.Error (loc, _env, error) -> typetexp_diagnostic report loc error
  | Typedecl.Error (loc, error) -> typedecl_diagnostic report loc error
  | Typecore.Error (loc, _env, error) -> typecore_diagnostic report loc error
  | Typemod.Error (loc, _env, error) -> typemod_diagnostic report loc error
  | Env.Error error -> env_diagnostic report error
  | Includemod.Apply_error { env; app_name; mty_f; args; _ } ->
    module_diagnostic report
      (Module_diagnostics.Functor_application_mismatch
         { env; app_name; mty_f; args })
  | Uniqueness_analysis.Usage.Unique_use_during_borrowing error ->
    mode_diagnostic report (Mode_diagnostics.Unique_use_during_borrowing error)
  | Uniqueness_analysis.Error error ->
    mode_diagnostic report (Mode_diagnostics.Uniqueness_error error)
  (* [exn] is extensible: unknown exception families use the normal report. *)
  | _ -> None

let diagnostic_of_exception report exn =
  match structured_diagnostic_of_exception report exn with
  | Some diagnostic -> Some diagnostic
  | None -> direct_mode_diagnostic report exn

let structured_emitter ppf exn report =
  let diagnostic =
    match exn with
    | Some exn -> (
      match diagnostic_of_exception report exn with
      | Some diagnostic -> diagnostic
      | None -> diagnostic_of_report report)
    | None -> diagnostic_of_report report
  in
  Format.fprintf ppf "%s@." (Diagnostic.to_json diagnostic)

let enable_structured_diagnostics () =
  Clflags.structured_diagnostics := true;
  Location.set_emitter structured_emitter
