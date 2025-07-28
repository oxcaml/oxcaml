type t =
  | Array_type
  | Class_type
  | Entry_point
  | Enumeration_type
  | Formal_parameter
  | Imported_declaration
  | Label
  | Lexical_block
  | Member
  | Pointer_type
  | Reference_type
  | Compile_unit
  | String_type
  | Structure_type
  | Subroutine_type
  | Typedef
  | Union_type
  | Unspecified_parameters
  | Variant
  | Common_block
  | Common_inclusion
  | Inheritance
  | Inlined_subroutine
  | Module
  | Ptr_to_member_type
  | Set_type
  | Subrange_type
  | With_stmt
  | Access_declaration
  | Base_type
  | Catch_block
  | Const_type
  | Constant
  | Enumerator
  | File_type
  | Friend
  | Namelist
  | Namelist_item
  | Packed_type
  | Subprogram
  | Template_type_parameter
  | Template_value_parameter
  | Thrown_type
  | Try_block
  | Variant_part
  | Variable
  | Volatile_type
  | Dwarf_procedure
  | Restrict_type
  | Interface_type
  | Namespace
  | Imported_module
  | Unspecified_type
  | Partial_unit
  | Imported_unit
  | Condition
  | Shared_type
  | Type_unit
  | Rvalue_reference_type
  | Template_alias
  | Coarray_type
  | Generic_subrange
  | Dynamic_type
  | Atomic_type
  | Call_site
  | Call_site_parameter
  | Skeleton_unit
  | Immutable_type
  | Lo_user
  | Hi_user
  (* GNU extensions *)
  | GNU_BINCL
  | GNU_EINCL
  | GNU_template_template_param
  | GNU_template_parameter_pack
  | GNU_formal_parameter_pack
  | GNU_call_site
  | GNU_call_site_parameter


let of_int_exn = function
  | 0x01 -> Array_type
  | 0x02 -> Class_type
  | 0x03 -> Entry_point
  | 0x04 -> Enumeration_type
  | 0x05 -> Formal_parameter
  | 0x08 -> Imported_declaration
  | 0x0a -> Label
  | 0x0b -> Lexical_block
  | 0x0d -> Member
  | 0x0f -> Pointer_type
  | 0x10 -> Reference_type
  | 0x11 -> Compile_unit
  | 0x12 -> String_type
  | 0x13 -> Structure_type
  | 0x15 -> Subroutine_type
  | 0x16 -> Typedef
  | 0x17 -> Union_type
  | 0x18 -> Unspecified_parameters
  | 0x19 -> Variant
  | 0x1a -> Common_block
  | 0x1b -> Common_inclusion
  | 0x1c -> Inheritance
  | 0x1d -> Inlined_subroutine
  | 0x1e -> Module
  | 0x1f -> Ptr_to_member_type
  | 0x20 -> Set_type
  | 0x21 -> Subrange_type
  | 0x22 -> With_stmt
  | 0x23 -> Access_declaration
  | 0x24 -> Base_type
  | 0x25 -> Catch_block
  | 0x26 -> Const_type
  | 0x27 -> Constant
  | 0x28 -> Enumerator
  | 0x29 -> File_type
  | 0x2a -> Friend
  | 0x2b -> Namelist
  | 0x2c -> Namelist_item
  | 0x2d -> Packed_type
  | 0x2e -> Subprogram
  | 0x2f -> Template_type_parameter
  | 0x30 -> Template_value_parameter
  | 0x31 -> Thrown_type
  | 0x32 -> Try_block
  | 0x33 -> Variant_part
  | 0x34 -> Variable
  | 0x35 -> Volatile_type
  | 0x36 -> Dwarf_procedure
  | 0x37 -> Restrict_type
  | 0x38 -> Interface_type
  | 0x39 -> Namespace
  | 0x3a -> Imported_module
  | 0x3b -> Unspecified_type
  | 0x3c -> Partial_unit
  | 0x3d -> Imported_unit
  | 0x3f -> Condition
  | 0x40 -> Shared_type
  | 0x41 -> Type_unit
  | 0x42 -> Rvalue_reference_type
  | 0x43 -> Template_alias
  | 0x44 -> Coarray_type
  | 0x45 -> Generic_subrange
  | 0x46 -> Dynamic_type
  | 0x47 -> Atomic_type
  | 0x48 -> Call_site
  | 0x49 -> Call_site_parameter
  | 0x4a -> Skeleton_unit
  | 0x4b -> Immutable_type
  | 0x4080 -> Lo_user
  | 0xffff -> Hi_user
  | 0x4104 -> GNU_BINCL
  | 0x4105 -> GNU_EINCL
  | 0x4106 -> GNU_template_template_param
  | 0x4107 -> GNU_template_parameter_pack
  | 0x4108 -> GNU_formal_parameter_pack
  | 0x4109 -> GNU_call_site
  | 0x410a -> GNU_call_site_parameter
  | w    -> failwith (Printf.sprintf "invalid tag name 0x%x" w)


let to_string = function
  | Array_type -> "DW_TAG_array_type"
  | Class_type -> "DW_TAG_class_type"
  | Entry_point -> "DW_TAG_entry_point"
  | Enumeration_type -> "DW_TAG_enumeration_type"
  | Formal_parameter -> "DW_TAG_formal_parameter"
  | Imported_declaration -> "DW_TAG_imported_declaration"
  | Label -> "DW_TAG_label"
  | Lexical_block -> "DW_TAG_lexical_block"
  | Member -> "DW_TAG_member"
  | Pointer_type -> "DW_TAG_pointer_type"
  | Reference_type -> "DW_TAG_reference_type"
  | Compile_unit -> "DW_TAG_compile_unit"
  | String_type -> "DW_TAG_string_type"
  | Structure_type -> "DW_TAG_structure_type"
  | Subroutine_type -> "DW_TAG_subroutine_type"
  | Typedef -> "DW_TAG_typedef"
  | Union_type -> "DW_TAG_union_type"
  | Unspecified_parameters -> "DW_TAG_unspecified_parameters"
  | Variant -> "DW_TAG_variant"
  | Common_block -> "DW_TAG_common_block"
  | Common_inclusion -> "DW_TAG_common_inclusion"
  | Inheritance -> "DW_TAG_inheritance"
  | Inlined_subroutine -> "DW_TAG_inlined_subroutine"
  | Module -> "DW_TAG_module"
  | Ptr_to_member_type -> "DW_TAG_ptr_to_member_type"
  | Set_type -> "DW_TAG_set_type"
  | Subrange_type -> "DW_TAG_subrange_type"
  | With_stmt -> "DW_TAG_with_stmt"
  | Access_declaration -> "DW_TAG_access_declaration"
  | Base_type -> "DW_TAG_base_type"
  | Catch_block -> "DW_TAG_catch_block"
  | Const_type -> "DW_TAG_const_type"
  | Constant -> "DW_TAG_constant"
  | Enumerator -> "DW_TAG_enumerator"
  | File_type -> "DW_TAG_file_type"
  | Friend -> "DW_TAG_friend"
  | Namelist -> "DW_TAG_namelist"
  | Namelist_item -> "DW_TAG_namelist_item"
  | Packed_type -> "DW_TAG_packed_type"
  | Subprogram -> "DW_TAG_subprogram"
  | Template_type_parameter -> "DW_TAG_template_type_param"
  | Template_value_parameter -> "DW_TAG_template_value_param"
  | Thrown_type -> "DW_TAG_thrown_type"
  | Try_block -> "DW_TAG_try_block"
  | Variant_part -> "DW_TAG_variant_part"
  | Variable -> "DW_TAG_variable"
  | Volatile_type -> "DW_TAG_volatile_type"
  | Dwarf_procedure -> "DW_TAG_dwarf_procedure"
  | Restrict_type -> "DW_TAG_restrict_type"
  | Interface_type -> "DW_TAG_interface_type"
  | Namespace -> "DW_TAG_namespace"
  | Imported_module -> "DW_TAG_imported_module"
  | Unspecified_type -> "DW_TAG_unspecified_type"
  | Partial_unit -> "DW_TAG_partial_unit"
  | Imported_unit -> "DW_TAG_imported_unit"
  | Condition -> "DW_TAG_condition"
  | Shared_type -> "DW_TAG_shared_type"
  | Type_unit -> "DW_TAG_type_unit"
  | Rvalue_reference_type -> "DW_TAG_rvalue_reference_type"
  | Template_alias -> "DW_TAG_template_alias"
  | Coarray_type -> "DW_TAG_coarray_type"
  | Generic_subrange -> "DW_TAG_generic_subrange"
  | Dynamic_type -> "DW_TAG_dynamic_type"
  | Atomic_type -> "DW_TAG_atomic_type"
  | Call_site -> "DW_TAG_call_site"
  | Call_site_parameter -> "DW_TAG_call_site_parameter"
  | Skeleton_unit -> "DW_TAG_skeleton_unit"
  | Immutable_type -> "DW_TAG_immutable_type"
  | Lo_user -> "DW_TAG_lo_user"
  | Hi_user -> "DW_TAG_hi_user"
  | GNU_BINCL -> "DW_TAG_GNU_BINCL"
  | GNU_EINCL -> "DW_TAG_GNU_EINCL"
  | GNU_template_template_param -> "DW_TAG_GNU_template_template_param"
  | GNU_template_parameter_pack -> "DW_TAG_GNU_template_parameter_pack"
  | GNU_formal_parameter_pack -> "DW_TAG_GNU_formal_parameter_pack"
  | GNU_call_site -> "DW_TAG_GNU_call_site"
  | GNU_call_site_parameter -> "DW_TAG_GNU_call_site_parameter"
