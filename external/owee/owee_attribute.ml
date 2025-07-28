module T = struct
  type t =
    | Sibling
    | Location
    | Name
    | Ordering
    | Byte_size
    | Bit_offset
    | Bit_size
    | Stmt_list
    | Low_pc
    | High_pc
    | Language
    | Discr
    | Discr_value
    | Visibility
    | Import
    | String_length
    | Common_reference
    | Comp_dir
    | Const_value
    | Containing_type
    | Default_value
    | Inline
    | Is_optional
    | Lower_bound
    | Producer
    | Prototyped
    | Return_addr
    | Start_scope
    | Bit_stride
    | Upper_bound
    | Abstract_origin
    | Accessibility
    | Address_class
    | Artificial
    | Base_types
    | Calling_convention
    | Count
    | Data_member_location
    | Decl_column
    | Decl_file
    | Decl_line
    | Declaration
    | Discr_list
    | Encoding
    | External
    | Frame_base
    | Friend
    | Identifier_case
    | Macro_info
    | Namelist_item
    | Priority
    | Segment
    | Specification
    | Static_link
    | Type
    | Use_location
    | Variable_parameter
    | Virtuality
    | Vtable_elem_location
    | Allocated
    | Associated
    | Data_location
    | Byte_stride
    | Entry_pc
    | Use_utf8
    | Extension
    | Ranges
    | Trampoline
    | Call_column
    | Call_file
    | Call_line
    | Description
    | Binary_scale
    | Decimal_scale
    | Small
    | Decimal_sign
    | Digit_count
    | Picture_string
    | Mutable
    | Threads_scaled
    | Explicit
    | Object_pointer
    | Endianity
    | Elemental
    | Pure
    | Recursive
    | Signature
    | Main_subprogram
    | Data_bit_offset
    | Const_expr
    | Enum_class
    | Linkage_name
    | String_length_bit_size
    | String_length_byte_size
    | Rank
    | Str_offsets_base
    | Addr_base
    | Rnglists_base
    | Dwo_name
    | Reference
    | Rvalue_reference
    | Macros
    | Call_all_calls
    | Call_all_source_calls
    | Call_all_tail_calls
    | Call_return_pc
    | Call_value
    | Call_origin
    | Call_parameter
    | Call_pc
    | Call_tail_call
    | Call_target
    | Call_target_clobbered
    | Call_data_location
    | Call_data_value
    | Noreturn
    | Alignment
    | Export_symbols
    | Deleted
    | Defaulted
    | Loclists_base
    | Lo_user
    | Hi_user
    (* GNU extensions *)
    | Sf_names
    | Src_info
    | Mac_info
    | Src_coords
    | Body_begin
    | Body_end
    | GNU_vector
    | GNU_odr_signature
    | GNU_template_name
    | GNU_call_site_value
    | GNU_call_site_data_value
    | GNU_call_site_target
    | GNU_call_site_target_clobbered
    | GNU_tail_call
    | GNU_all_tail_call_sites
    | GNU_all_call_sites
    | GNU_all_source_call_sites
    | GNU_macros
    | GNU_deleted
    | GNU_locviews
    | GNU_entryviews

  let compare a b =
    (* Using the polymorphic compare -- it's fine in that case as [t] is a basic enum. *)
    compare a b
end

include T
module Map = Map.Make(T)

let of_int_exn = function
  | 0x01 -> Sibling
  | 0x02 -> Location
  | 0x03 -> Name
  | 0x09 -> Ordering
  | 0x0b -> Byte_size
  | 0x0c -> Bit_offset
  | 0x0d -> Bit_size
  | 0x10 -> Stmt_list
  | 0x11 -> Low_pc
  | 0x12 -> High_pc
  | 0x13 -> Language
  | 0x15 -> Discr
  | 0x16 -> Discr_value
  | 0x17 -> Visibility
  | 0x18 -> Import
  | 0x19 -> String_length
  | 0x1a -> Common_reference
  | 0x1b -> Comp_dir
  | 0x1c -> Const_value
  | 0x1d -> Containing_type
  | 0x1e -> Default_value
  | 0x20 -> Inline
  | 0x21 -> Is_optional
  | 0x22 -> Lower_bound
  | 0x25 -> Producer
  | 0x27 -> Prototyped
  | 0x2a -> Return_addr
  | 0x2c -> Start_scope
  | 0x2e -> Bit_stride
  | 0x2f -> Upper_bound
  | 0x31 -> Abstract_origin
  | 0x32 -> Accessibility
  | 0x33 -> Address_class
  | 0x34 -> Artificial
  | 0x35 -> Base_types
  | 0x36 -> Calling_convention
  | 0x37 -> Count
  | 0x38 -> Data_member_location
  | 0x39 -> Decl_column
  | 0x3a -> Decl_file
  | 0x3b -> Decl_line
  | 0x3c -> Declaration
  | 0x3d -> Discr_list
  | 0x3e -> Encoding
  | 0x3f -> External
  | 0x40 -> Frame_base
  | 0x41 -> Friend
  | 0x42 -> Identifier_case
  | 0x43 -> Macro_info
  | 0x44 -> Namelist_item
  | 0x45 -> Priority
  | 0x46 -> Segment
  | 0x47 -> Specification
  | 0x48 -> Static_link
  | 0x49 -> Type
  | 0x4a -> Use_location
  | 0x4b -> Variable_parameter
  | 0x4c -> Virtuality
  | 0x4d -> Vtable_elem_location
  | 0x4e -> Allocated
  | 0x4f -> Associated
  | 0x50 -> Data_location
  | 0x51 -> Byte_stride
  | 0x52 -> Entry_pc
  | 0x53 -> Use_utf8
  | 0x54 -> Extension
  | 0x55 -> Ranges
  | 0x56 -> Trampoline
  | 0x57 -> Call_column
  | 0x58 -> Call_file
  | 0x59 -> Call_line
  | 0x5a -> Description
  | 0x5b -> Binary_scale
  | 0x5c -> Decimal_scale
  | 0x5d -> Small
  | 0x5e -> Decimal_sign
  | 0x5f -> Digit_count
  | 0x60 -> Picture_string
  | 0x61 -> Mutable
  | 0x62 -> Threads_scaled
  | 0x63 -> Explicit
  | 0x64 -> Object_pointer
  | 0x65 -> Endianity
  | 0x66 -> Elemental
  | 0x67 -> Pure
  | 0x68 -> Recursive
  | 0x69 -> Signature
  | 0x6a -> Main_subprogram
  | 0x6b -> Data_bit_offset
  | 0x6c -> Const_expr
  | 0x6d -> Enum_class
  | 0x6e -> Linkage_name
  | 0x6f -> String_length_bit_size
  | 0x70 -> String_length_byte_size
  | 0x71 -> Rank
  | 0x72 -> Str_offsets_base
  | 0x73 -> Addr_base
  | 0x74 -> Rnglists_base
  | 0x76 -> Dwo_name
  | 0x77 -> Reference
  | 0x78 -> Rvalue_reference
  | 0x79 -> Macros
  | 0x7a -> Call_all_calls
  | 0x7b -> Call_all_source_calls
  | 0x7c -> Call_all_tail_calls
  | 0x7d -> Call_return_pc
  | 0x7e -> Call_value
  | 0x7f -> Call_origin
  | 0x80 -> Call_parameter
  | 0x81 -> Call_pc
  | 0x82 -> Call_tail_call
  | 0x83 -> Call_target
  | 0x84 -> Call_target_clobbered
  | 0x85 -> Call_data_location
  | 0x86 -> Call_data_value
  | 0x87 -> Noreturn
  | 0x88 -> Alignment
  | 0x89 -> Export_symbols
  | 0x8a -> Deleted
  | 0x8b -> Defaulted
  | 0x8c -> Loclists_base
  | 0x2000 -> Lo_user
  | 0x3fff -> Hi_user
  (* GNU extentensions *)
  | 0x2101 -> Sf_names
  | 0x2102 -> Src_info
  | 0x2103 -> Mac_info
  | 0x2104 -> Src_coords
  | 0x2105 -> Body_begin
  | 0x2106 -> Body_end
  | 0x2107 -> GNU_vector
  | 0x210f -> GNU_odr_signature
  | 0x2110 -> GNU_template_name
  | 0x2111 -> GNU_call_site_value
  | 0x2112 -> GNU_call_site_data_value
  | 0x2113 -> GNU_call_site_target
  | 0x2114 -> GNU_call_site_target_clobbered
  | 0x2115 -> GNU_tail_call
  | 0x2116 -> GNU_all_tail_call_sites
  | 0x2117 -> GNU_all_call_sites
  | 0x2118 -> GNU_all_source_call_sites
  | 0x2119 -> GNU_macros
  | 0x211a -> GNU_deleted
  | 0x2137 -> GNU_locviews
  | 0x2138 -> GNU_entryviews
  | w    -> failwith (Printf.sprintf "invalid attribute name 0x%x" w)

let to_string = function
  | Sibling -> "DW_AT_sibling"
  | Location -> "DW_AT_location"
  | Name -> "DW_AT_name"
  | Ordering -> "DW_AT_ordering"
  | Byte_size -> "DW_AT_byte_size"
  | Bit_offset -> "DW_AT_bit_offset"
  | Bit_size -> "DW_AT_bit_size"
  | Stmt_list -> "DW_AT_stmt_list"
  | Low_pc -> "DW_AT_low_pc"
  | High_pc -> "DW_AT_high_pc"
  | Language -> "DW_AT_language"
  | Discr -> "DW_AT_discr"
  | Discr_value -> "DW_AT_discr_value"
  | Visibility -> "DW_AT_visibility"
  | Import -> "DW_AT_import"
  | String_length -> "DW_AT_string_length"
  | Common_reference -> "DW_AT_common_reference"
  | Comp_dir -> "DW_AT_comp_dir"
  | Const_value -> "DW_AT_const_value"
  | Containing_type -> "DW_AT_containing_type"
  | Default_value -> "DW_AT_default_value"
  | Inline -> "DW_AT_inline"
  | Is_optional -> "DW_AT_is_optional"
  | Lower_bound -> "DW_AT_lower_bound"
  | Producer -> "DW_AT_producer"
  | Prototyped -> "DW_AT_prototyped"
  | Return_addr -> "DW_AT_return_addr"
  | Start_scope -> "DW_AT_start_scope"
  | Bit_stride -> "DW_AT_bit_stride"
  | Upper_bound -> "DW_AT_upper_bound"
  | Abstract_origin -> "DW_AT_abstract_origin"
  | Accessibility -> "DW_AT_accessibility"
  | Address_class -> "DW_AT_address_class"
  | Artificial -> "DW_AT_artificial"
  | Base_types -> "DW_AT_base_types"
  | Calling_convention -> "DW_AT_calling_convention"
  | Count -> "DW_AT_count"
  | Data_member_location -> "DW_AT_data_member_location"
  | Decl_column -> "DW_AT_decl_column"
  | Decl_file -> "DW_AT_decl_file"
  | Decl_line -> "DW_AT_decl_line"
  | Declaration -> "DW_AT_declaration"
  | Discr_list -> "DW_AT_discr_list"
  | Encoding -> "DW_AT_encoding"
  | External -> "DW_AT_external"
  | Frame_base -> "DW_AT_frame_base"
  | Friend -> "DW_AT_friend"
  | Identifier_case -> "DW_AT_identifier_case"
  | Macro_info -> "DW_AT_macro_info"
  | Namelist_item -> "DW_AT_namelist_item"
  | Priority -> "DW_AT_priority"
  | Segment -> "DW_AT_segment"
  | Specification -> "DW_AT_specification"
  | Static_link -> "DW_AT_static_link"
  | Type -> "DW_AT_type"
  | Use_location -> "DW_AT_use_location"
  | Variable_parameter -> "DW_AT_variable_parameter"
  | Virtuality -> "DW_AT_virtuality"
  | Vtable_elem_location -> "DW_AT_vtable_elem_location"
  | Allocated -> "DW_AT_allocated"
  | Associated -> "DW_AT_associated"
  | Data_location -> "DW_AT_data_location"
  | Byte_stride -> "DW_AT_byte_stride"
  | Entry_pc -> "DW_AT_entry_pc"
  | Use_utf8 -> "DW_AT_use_utf8"
  | Extension -> "DW_AT_extension"
  | Ranges -> "DW_AT_ranges"
  | Trampoline -> "DW_AT_trampoline"
  | Call_column -> "DW_AT_call_column"
  | Call_file -> "DW_AT_call_file"
  | Call_line -> "DW_AT_call_line"
  | Description -> "DW_AT_description"
  | Binary_scale -> "DW_AT_binary_scale"
  | Decimal_scale -> "DW_AT_decimal_scale"
  | Small -> "DW_AT_small"
  | Decimal_sign -> "DW_AT_decimal_sign"
  | Digit_count -> "DW_AT_digit_count"
  | Picture_string -> "DW_AT_picture_string"
  | Mutable -> "DW_AT_mutable"
  | Threads_scaled -> "DW_AT_threads_scaled"
  | Explicit -> "DW_AT_explicit"
  | Object_pointer -> "DW_AT_object_pointer"
  | Endianity -> "DW_AT_endianity"
  | Elemental -> "DW_AT_elemental"
  | Pure -> "DW_AT_pure"
  | Recursive -> "DW_AT_recursive"
  | Signature -> "DW_AT_signature"
  | Main_subprogram -> "DW_AT_main_subprogram"
  | Data_bit_offset -> "DW_AT_data_bit_offset"
  | Const_expr -> "DW_AT_const_expr"
  | Enum_class -> "DW_AT_enum_class"
  | Linkage_name -> "DW_AT_linkage_name"
  | String_length_bit_size -> "DW_AT_string_length_bit_size"
  | String_length_byte_size -> "DW_AT_string_length_byte_size"
  | Rank -> "DW_AT_rank"
  | Str_offsets_base -> "DW_AT_str_offsets_base"
  | Addr_base -> "DW_AT_addr_base"
  | Rnglists_base -> "DW_AT_rnglists_base"
  | Dwo_name -> "DW_AT_dwo_name"
  | Reference -> "DW_AT_reference"
  | Rvalue_reference -> "DW_AT_rvalue_reference"
  | Macros -> "DW_AT_macros"
  | Call_all_calls -> "DW_AT_call_all_calls"
  | Call_all_source_calls -> "DW_AT_call_all_source_calls"
  | Call_all_tail_calls -> "DW_AT_call_all_tail_calls"
  | Call_return_pc -> "DW_AT_call_return_pc"
  | Call_value -> "DW_AT_call_value"
  | Call_origin -> "DW_AT_call_origin"
  | Call_parameter -> "DW_AT_call_parameter"
  | Call_pc -> "DW_AT_call_pc"
  | Call_tail_call -> "DW_AT_call_tail_call"
  | Call_target -> "DW_AT_call_target"
  | Call_target_clobbered -> "DW_AT_call_target_clobbered"
  | Call_data_location -> "DW_AT_call_data_location"
  | Call_data_value -> "DW_AT_call_data_value"
  | Noreturn -> "DW_AT_noreturn"
  | Alignment -> "DW_AT_alignment"
  | Export_symbols -> "DW_AT_export_symbols"
  | Deleted -> "DW_AT_deleted"
  | Defaulted -> "DW_AT_defaulted"
  | Loclists_base -> "DW_AT_loclists_base"
  | Lo_user -> "DW_AT_lo_user"
  | Hi_user -> "DW_AT_hi_user"
  | Sf_names -> "DW_AT_sf_names"
  | Src_info -> "DW_AT_src_info"
  | Mac_info -> "DW_AT_mac_info"
  | Src_coords -> "DW_AT_src_coords"
  | Body_begin -> "DW_AT_body_begin"
  | Body_end -> "DW_AT_body_end"
  | GNU_vector -> "DW_AT_GNU_vector"
  | GNU_odr_signature -> "DW_AT_GNU_odr_signature"
  | GNU_template_name -> "DW_AT_GNU_template_name"
  | GNU_call_site_value -> "DW_AT_GNU_call_site_value"
  | GNU_call_site_data_value -> "DW_AT_GNU_call_site_data_value"
  | GNU_call_site_target -> "DW_AT_GNU_call_site_target"
  | GNU_call_site_target_clobbered -> "DW_AT_GNU_call_site_target_clobbered"
  | GNU_tail_call -> "DW_AT_GNU_tail_call"
  | GNU_all_tail_call_sites -> "DW_AT_GNU_all_tail_call_sites"
  | GNU_all_call_sites -> "DW_AT_GNU_all_call_sites"
  | GNU_all_source_call_sites -> "DW_AT_GNU_all_source_call_sites"
  | GNU_macros -> "DW_AT_GNU_macros"
  | GNU_deleted -> "DW_AT_GNU_deleted"
  | GNU_locviews -> "DW_AT_GNU_locviews"
  | GNU_entryviews -> "DW_AT_GNU_entryviews"
