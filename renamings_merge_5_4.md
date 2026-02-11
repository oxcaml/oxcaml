# Renamings for the OCaml 5.4 merge

This file collects renamings from various upstream PRs in fully
module-expanded form. Use this as a reference when resolving merge conflicts
in `.ml` files.

## PR #13336: Printtyp refactoring

Upstream split `printtyp.mli` into `out_type.mli` (outcometree generation,
naming contexts, preparation) and `errortrace_report.mli` (error reporting).
The remaining `printtyp.mli` exposes a `Printers` module type with `Doc` and
`Format`-compatible instantiations.

Functions that remain in `Printtyp` via `include Printers` (such as
`wrap_printing_env`, `type_expr`, `type_scheme`, `type_expansion`,
`constructor_arguments`, etc.) do **not** need renaming.

### Printtyp.Naming_context → Out_type.Ident_names

- `Printtyp.Naming_context.enable` → `Out_type.Ident_names.enable`
- `Printtyp.Naming_context.reset` → `Out_type.Ident_names.reset` (oxcaml-specific; see CR in out_type.mli)

### Printtyp.Conflicts → Out_type.Ident_conflicts

- `Printtyp.Conflicts.exists` → `Out_type.Ident_conflicts.exists`
- `Printtyp.Conflicts.list_explanations` → `Out_type.Ident_conflicts.list_explanations`
- `Printtyp.Conflicts.print_located_explanations` → `Out_type.Ident_conflicts.print_located_explanations`
- `Printtyp.Conflicts.reset` → `Out_type.Ident_conflicts.reset`

### Printtyp.Out_name → Out_type.Out_name

- `Printtyp.Out_name.create` → `Out_type.Out_name.create`
- `Printtyp.Out_name.print` → `Out_type.Out_name.print`

### Printtyp top-level → Out_type

- `Printtyp.reset` → `Out_type.reset`
- `Printtyp.prepare_for_printing` → `Out_type.prepare_for_printing`
- `Printtyp.add_type_to_preparation` → `Out_type.add_type_to_preparation`
- `Printtyp.prepared_type_expr` → `Out_type.prepared_type_expr`
- `Printtyp.prepared_type_scheme` → `Out_type.prepared_type_scheme`
- `Printtyp.prepared_constructor` → `Out_type.prepared_constructor`
- `Printtyp.prepared_extension_constructor` → `Out_type.prepared_extension_constructor`
- `Printtyp.prepared_type_declaration` → `Out_type.prepared_type_declaration`
- `Printtyp.add_type_declaration_to_preparation` → `Out_type.add_type_declaration_to_preparation`
- `Printtyp.add_constructor_to_preparation` → `Out_type.add_constructor_to_preparation`
- `Printtyp.add_extension_constructor_to_preparation` → `Out_type.add_extension_constructor_to_preparation`
- `Printtyp.prepare_expansion` → `Out_type.prepare_expansion`
- `Printtyp.tree_of_path` → `Out_type.tree_of_path`
- `Printtyp.tree_of_typexp` → `Out_type.tree_of_typexp`
- `Printtyp.tree_of_type_scheme` → `Out_type.tree_of_type_scheme`
- `Printtyp.tree_of_type_declaration` → `Out_type.tree_of_type_declaration`
- `Printtyp.tree_of_value_description` → `Out_type.tree_of_value_description`
- `Printtyp.tree_of_extension_constructor` → `Out_type.tree_of_extension_constructor`
- `Printtyp.tree_of_module` → `Out_type.tree_of_module`
- `Printtyp.tree_of_modtype` → `Out_type.tree_of_modtype`
- `Printtyp.tree_of_modtype_declaration` → `Out_type.tree_of_modtype_declaration`
- `Printtyp.tree_of_signature` → `Out_type.tree_of_signature`
- `Printtyp.tree_of_class_declaration` → `Out_type.tree_of_class_declaration`
- `Printtyp.tree_of_cltype_declaration` → `Out_type.tree_of_cltype_declaration`
- `Printtyp.type_or_scheme` → `Out_type.type_or_scheme`
- `Printtyp.print_items` → `Out_type.print_items`
- `Printtyp.rewrite_double_underscore_paths` → `Out_type.rewrite_double_underscore_paths`

### Printtyp top-level → Errortrace_report

- `Printtyp.report_ambiguous_type_error` → `Errortrace_report.ambiguous_type`
- `Printtyp.report_unification_error` → `Errortrace_report.unification`
- `Printtyp.report_equality_error` → `Errortrace_report.equality`
- `Printtyp.report_moregen_error` → `Errortrace_report.moregen`
- `Printtyp.report_comparison_error` → `Errortrace_report.comparison`
- `Printtyp.Subtype.report_error` → `Errortrace_report.subtype`

### Removed from .mli (internal only)

- `Printtyp.functor_parameters` — no external callers

### oxcaml-specific signature changes applied to Out_type

These are oxcaml modifications to functions that moved to `out_type.mli`.
The implementations remain initially in `printtyp.ml`; see CRs in `out_type.mli`.

- `Out_type.tree_of_module`: takes `module_declaration` (not `module_type`)
- `Out_type.tree_of_modtype`: added `?abbrev:bool`
- `Out_type.tree_of_modtype_declaration`: added `?abbrev:bool`
- `Out_type.Ident_names.reset`: oxcaml-specific addition
- `Out_type.tree_of_type_scheme`: removed upstream, re-exposed for oxcaml

### Printtyp.Conflicts.print_explanations

(Not part of #13336; this renaming happened in a different upstream PR.)

- `Printtyp.Conflicts.print_explanations` → `Out_type.Ident_conflicts.err_print`

## PR #13466: Types → Data_types

Upstream moved constructor and label description types from `Types` into a new
`Data_types` module. In `.ml` files, uses of these types need to be requalified.

Upstream moved:

- `Types.constructor_description` → `Data_types.constructor_description`
- `Types.constructor_tag` → `Data_types.constructor_tag`
- `Types.equal_constr` → `Data_types.equal_constr`
- `Types.may_equal_constr` → `Data_types.may_equal_constr`
- `Types.label_description` → `Data_types.label_description`

oxcaml additionally moved (these were oxcaml-specific types/functions already
in `Types`):

- `Types.equal_tag` → `Data_types.equal_tag`
- `Types.compare_tag` → `Data_types.compare_tag`
- `Types.gen_label_description` → `Data_types.gen_label_description`
- `Types.unboxed_label_description` → `Data_types.unboxed_label_description`
- `Types.record_form` → `Data_types.record_form`
- `Types.record_form_packed` → `Data_types.record_form_packed`
- `Types.record_form_to_string` → `Data_types.record_form_to_string`
- `Types.equal_record_representation` → `Data_types.equal_record_representation`
- `Types.equal_record_unboxed_product_representation` → `Data_types.equal_record_unboxed_product_representation`
- `Types.equal_variant_representation` → `Data_types.equal_variant_representation`
- `Types.mixed_block_element_of_const_sort` → `Data_types.mixed_block_element_of_const_sort`

### Note

Many files `open Data_types`, so unqualified references to these types may still
work. Check each file's opens when resolving conflicts.

## Misc

### Includemod: modtypes_with_shape → modtypes_constraint

- `Includemod.modtypes_with_shape` → `Includemod.modtypes_constraint`
