(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Tomasz Nowak and Mark Shinwell, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Dwarf_low
open! Dwarf_high
module Uid = Flambda2_identifiers.Flambda_debug_uid
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state
module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module Sort = Jkind_types.Sort
module Layout = Sort.Const
module S = Shape
module String = Misc.Stdlib.String

type base_layout = Sort.base

module Debugging_the_compiler = struct
  let enabled () = !Dwarf_flags.ddwarf_types

  let die_description_table = String.Tbl.create 0

  let add ~reference info =
    if enabled ()
    then
      String.Tbl.add die_description_table
        (Asm_targets.Asm_label.encode reference)
        info

  let add_alias ~from_ref ~to_ref =
    if enabled ()
    then
      let desc =
        Format.asprintf "-> %s" (Asm_targets.Asm_label.encode to_ref)
      in
      String.Tbl.add die_description_table
        (Asm_targets.Asm_label.encode from_ref)
        desc

  let add_enum ~reference constructors =
    if enabled ()
    then
      let desc = Format.asprintf "= %s" (String.concat " | " constructors) in
      String.Tbl.add die_description_table
        (Asm_targets.Asm_label.encode reference)
        desc

  let add_ptr ~reference ~inner =
    if enabled ()
    then
      let desc =
        Format.asprintf "%s ptr" (Asm_targets.Asm_label.encode inner)
      in
      String.Tbl.add die_description_table
        (Asm_targets.Asm_label.encode reference)
        desc

  let print ~die =
    if enabled ()
    then
      let indent = ref 0 in
      Proto_die.depth_first_fold die ~init:() ~f:(fun () d ->
          match d with
          | DIE { label; tag; has_children; attribute_values = _; _ } ->
            let indentation = String.make !indent ' ' in
            let info =
              (String.Tbl.find_opt die_description_table)
                (Asm_targets.Asm_label.encode label)
            in
            Format.eprintf "%s+ %a(%s) %a\n" indentation
              Asm_targets.Asm_label.print label (Dwarf_tag.tag_name tag)
              (Format.pp_print_option Format.pp_print_string)
              info;
            if has_children = Child_determination.Yes then indent := !indent + 2
          | End_of_siblings -> indent := !indent - 2)
end

module Shape_reduction_diagnostics : sig
  type t

  val create : string -> Layout.t -> t

  val shape_reduce_diagnostics : t -> Shape_reduce.Diagnostics.t

  val shape_evaluation_diagnostics : t -> Type_shape.Evaluation_diagnostics.t

  val record_before_reduction : t -> Shape.t -> unit

  val record_after_reduction : t -> Shape.t -> unit

  val record_after_evaluation : t -> Shape.t -> unit

  val record_before_dwarf_generation : t -> Proto_die.t -> unit

  val record_after_dwarf_generation : t -> Proto_die.t -> unit

  val append_diagnostics_to_dwarf_state : DS.t -> t -> unit
end = struct
  type d =
    { type_name : string;
      type_layout : Layout.t;
      mutable shape_size_before_reduction_in_bytes : int;
      mutable shape_size_after_reduction_in_bytes : int;
      mutable shape_size_after_evaluation_in_bytes : int;
      mutable dwarf_die_size_before : int;
      mutable dwarf_die_size_after : int;
      shape_reduction_diagnostics : Shape_reduce.Diagnostics.t;
      shape_evaluation_diagnostics : Type_shape.Evaluation_diagnostics.t
    }

  type t = d option

  let create type_name type_layout =
    if !Dwarf_flags.ddwarf_metrics
    then
      Some
        { type_name;
          type_layout;
          shape_size_before_reduction_in_bytes = 0;
          shape_size_after_reduction_in_bytes = 0;
          shape_size_after_evaluation_in_bytes = 0;
          dwarf_die_size_before = 0;
          dwarf_die_size_after = 0;
          shape_reduction_diagnostics =
            Shape_reduce.Diagnostics.create_diagnostics ();
          shape_evaluation_diagnostics =
            Type_shape.Evaluation_diagnostics.create_diagnostics ()
        }
    else None

  let shape_reduce_diagnostics d =
    match d with
    | None -> Shape_reduce.Diagnostics.no_diagnostics
    | Some d -> d.shape_reduction_diagnostics

  let shape_evaluation_diagnostics d =
    match d with
    | None -> Type_shape.Evaluation_diagnostics.no_diagnostics
    | Some d -> d.shape_evaluation_diagnostics

  let record_before_reduction d shape =
    match d with
    | None -> ()
    | Some d ->
      d.shape_size_before_reduction_in_bytes
        <- Obj.reachable_words (Obj.repr shape) * Sys.word_size

  let record_after_reduction d shape =
    match d with
    | None -> ()
    | Some d ->
      d.shape_size_after_reduction_in_bytes
        <- Obj.reachable_words (Obj.repr shape) * Sys.word_size

  let record_after_evaluation d shape =
    match d with
    | None -> ()
    | Some d ->
      d.shape_size_after_evaluation_in_bytes
        <- Obj.reachable_words (Obj.repr shape) * Sys.word_size

  let compute_die_size die =
    Proto_die.depth_first_fold die ~init:0 ~f:(fun acc d ->
        match d with DIE _ -> acc + 1 | End_of_siblings -> acc)

  let record_before_dwarf_generation d die =
    match d with
    | None -> ()
    | Some d -> d.dwarf_die_size_before <- compute_die_size die

  let record_after_dwarf_generation d die =
    match d with
    | None -> ()
    | Some d -> d.dwarf_die_size_after <- compute_die_size die

  let append_diagnostics_to_dwarf_state state d =
    match d with
    | None -> ()
    | Some d ->
      if !Dwarf_flags.ddwarf_metrics
      then
        let diagnostic : DS.Diagnostics.variable_reduction =
          { shape_size_before_reduction_in_bytes =
              d.shape_size_before_reduction_in_bytes;
            shape_size_after_reduction_in_bytes =
              d.shape_size_after_reduction_in_bytes;
            shape_size_after_evaluation_in_bytes =
              d.shape_size_after_evaluation_in_bytes;
            reduction_steps =
              Shape_reduce.Diagnostics.reduction_steps
                d.shape_reduction_diagnostics;
            evaluation_steps =
              Type_shape.Evaluation_diagnostics.get_reduction_steps
                d.shape_evaluation_diagnostics;
            type_name = d.type_name;
            type_layout = d.type_layout;
            dwarf_die_size = d.dwarf_die_size_after - d.dwarf_die_size_before;
            cms_files_loaded =
              Shape_reduce.Diagnostics.cms_files_loaded
                d.shape_reduction_diagnostics;
            cms_files_cached =
              Shape_reduce.Diagnostics.cms_files_cached
                d.shape_reduction_diagnostics;
            cms_files_missing =
              Shape_reduce.Diagnostics.cms_files_missing
                d.shape_reduction_diagnostics;
            cms_files_unreadable =
              Shape_reduce.Diagnostics.cms_files_unreadable
                d.shape_reduction_diagnostics
          }
        in
        DS.add_variable_reduction_diagnostic state diagnostic
end

let base_layout_to_byte_size (sort : base_layout) =
  match sort with
  | Void -> 0
  | Float32 -> 4
  | Float64 -> 8
  | Word -> Arch.size_addr
  | Bits8 -> 1
  | Bits16 -> 2
  | Bits32 -> 4
  | Bits64 -> 8
  | Untagged_immediate -> Arch.size_addr
  | Vec128 -> 16
  | Vec256 -> 32
  | Vec512 -> 64
  | Value -> Arch.size_addr

(* CR sspies: This handling is incorrect for [Void] layout. Once we support
   putting [Void] data into records, we have to adjust the code below to filter
   out void fields from mixed records and mixed variants. Moreover e.g. records
   whose members are all void are actually represented as unit! *)
(* Smaller entries in mixed blocks are padded to be of, at least, word size *)
let base_layout_to_byte_size_in_mixed_block (sort : base_layout) =
  Int.max (base_layout_to_byte_size sort) Arch.size_addr

let attribute_list_with_optional_name name attributes =
  match name with
  | None -> attributes
  | Some name -> DAH.create_name name :: attributes

let wrap_die_under_a_pointer ~proto_die ~reference ~parent_proto_die =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Reference_type
    ~attribute_values:
      [DAH.create_byte_size_exn ~byte_size:8; DAH.create_type ~proto_die]
    ();
  Debugging_the_compiler.add_ptr ~reference
    ~inner:(Proto_die.reference proto_die)

let create_typedef_die ~reference ~parent_proto_die ?name child_die =
  Debugging_the_compiler.add_alias ~from_ref:reference ~to_ref:child_die;
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Typedef
    ~attribute_values:
      ([DAH.create_type_from_reference ~proto_die_reference:child_die]
      |> attribute_list_with_optional_name name)
    ()

let create_array_die ~reference ~parent_proto_die ~child_die ?name () =
  let array_die =
    Proto_die.create ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Array_type
      ~attribute_values:
        [ DAH.create_type_from_reference ~proto_die_reference:child_die;
          (* We can't use DW_AT_byte_size or DW_AT_bit_size since we don't know
             how large the array might be. *)
          (* DW_AT_byte_stride probably isn't required strictly speaking, but
             let's add it for the avoidance of doubt. *)
          DAH.create_byte_stride ~bytes:(Int8.of_int_exn Arch.size_addr) ]
      ()
  in
  Proto_die.create_ignore ~parent:(Some array_die) ~tag:Dwarf_tag.Subrange_type
    ~attribute_values:
      [ (* Thankfully, all that lldb cares about is DW_AT_count. *)
        DAH.create_count_const 0L ]
    ();
  (* OxCaml LLDB currently uses custom printing for arrays instead of respecting
     the name attached to the [Array_type] node. Thus, we introduce a typedef
     with the right name here if the name is provided. *)
  let pointer_reference =
    match name with
    | None -> reference (* no need to add a typedef *)
    | Some _ -> Proto_die.create_reference ()
  in
  wrap_die_under_a_pointer ~proto_die:array_die ~reference:pointer_reference
    ~parent_proto_die;
  match name with
  | None -> ()
  | Some name ->
    create_typedef_die ~reference ~parent_proto_die ~name pointer_reference

let create_char_die ~reference ~parent_proto_die ?name () =
  (* As a char is an immediate value, we have to ignore the first bit.
     Unfortunately lldb supports bit offsets only on members of structs, so
     instead, we create a hacky enum containing all possible char values. *)
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:8]
        |> attribute_list_with_optional_name name)
        (* CR sspies: The name here is displayed as ["enum " ^ name] in gdb, but
           correctly as [name] in lldb. *)
      ()
  in
  Debugging_the_compiler.add ~reference "char enum";
  List.iter
    (fun i ->
      Proto_die.create_ignore ~parent:(Some enum) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [ DAH.create_const_value ~value:(Int64.of_int ((2 * i) + 1));
            DAH.create_name (Printf.sprintf "%C" (Char.chr i)) ]
        ())
    (List.init 256 (fun i -> i))

let create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
    encoding =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Base_type
    ~attribute_values:
      ([DAH.create_byte_size_exn ~byte_size; DAH.create_encoding ~encoding]
      |> attribute_list_with_optional_name name)
    ()

let create_unnamed_unboxed_base_layout_die ~reference ~parent_proto_die
    ~byte_size ~encoding =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Base_type
    ~attribute_values:
      [DAH.create_byte_size_exn ~byte_size; DAH.create_encoding ~encoding]
    ()

let create_record_die ~reference ~parent_proto_die ?name ~fields () =
  let total_size =
    List.fold_left (fun acc (_, field_size, _) -> acc + field_size) 0 fields
  in
  let structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:total_size]
        |> attribute_list_with_optional_name name)
      ()
  in
  let offset = ref 0 in
  List.iter
    (fun (field_name, field_size, field_die) ->
      let field =
        Proto_die.create ~parent:(Some structure) ~tag:Dwarf_tag.Member
          ~attribute_values:
            [ DAH.create_name field_name;
              DAH.create_type_from_reference ~proto_die_reference:field_die;
              DAH.create_data_member_location_offset
                ~byte_offset:(Int64.of_int !offset) ]
          ()
      in
      Debugging_the_compiler.add
        ~reference:(Proto_die.reference field)
        ("." ^ field_name);
      offset := !offset + field_size)
    fields;
  wrap_die_under_a_pointer ~proto_die:structure ~reference ~parent_proto_die

(* The following function handles records annotated with [[@@unboxed]]. These
   may only have a single field. ("Unboxed records" of the form [#{ ... }] are
   destructed into their component parts by unarization.) *)
let create_attribute_unboxed_record_die ~reference ~parent_proto_die ?name
    ~field_die ~field_name ~field_size () =
  let structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:field_size]
        |> attribute_list_with_optional_name name)
      ()
  in
  Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_name field_name;
        DAH.create_type_from_reference ~proto_die_reference:field_die;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ()

let create_simple_variant_die ~reference ~parent_proto_die ?name
    simple_constructors =
  Debugging_the_compiler.add_enum ~reference simple_constructors;
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:8]
        |> attribute_list_with_optional_name name)
      ()
  in
  List.iteri
    (fun i constructor ->
      Proto_die.create_ignore ~parent:(Some enum) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [ DAH.create_const_value ~value:(Int64.of_int ((2 * i) + 1));
            DAH.create_name constructor ]
        ())
    simple_constructors

(* CR mshinwell: it seems like this should move to the frontend *)
let rec layout_to_types_layout (ly : Layout.t) : Types.mixed_block_element =
  match ly with
  | Base base -> (
    match base with
    | Value -> Value
    | Float64 -> Float64
    (* This is a case, where we potentially have mapped [Float_boxed] to
       [Float64], but that is fine, because they are reordered like other mixed
       fields. *)
    (* CR sspies: Is this true? It currently means we will add an extra hash to
       the debugging output. But it's not clear that that is a bad thing. *)
    | Float32 -> Float32
    | Bits8 -> Bits8
    | Bits16 -> Bits16
    | Bits32 -> Bits32
    | Bits64 -> Bits64
    | Vec128 -> Vec128
    | Vec256 -> Vec256
    | Vec512 -> Vec512
    | Word -> Word
    | Untagged_immediate -> Word
    | Void -> Product [||])
  | Product lys -> Product (Array.of_list (List.map layout_to_types_layout lys))

let rec project_layout (layout : Layout.t) path =
  match layout, path with
  | Base b, [] -> b
  | Product p, i :: path -> project_layout (List.nth p i) path
  | _, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf
        "project_layout: unexpected layout/path combination: layout=%a path=%a"
        Layout.format layout
        (Format.pp_print_list Format.pp_print_int)
        path
    else Sort.Value

let rec field_name_with_path base path =
  match path with
  | [] -> base
  | i :: path -> field_name_with_path base path ^ ".#" ^ Int.to_string i

type 'layout projected_field = string option * S.t * 'layout

let project_field_given_path (fields : Layout.t projected_field array) path :
    base_layout projected_field =
  match path with
  | [] ->
    if !Clflags.dwarf_pedantic
    then
      let pp_projected_field fmt (name, shape, layout) =
        Format.fprintf fmt "(%a, %a, %a)"
          (Format.pp_print_option Format.pp_print_string)
          name Shape.print shape Layout.format layout
      in
      Misc.fatal_errorf
        "Empty path provided to [field_project_path] for projecting from %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_projected_field)
        (Array.to_list fields)
      (* field should exist *)
    else None, Shape.leaf' None, Sort.Value
  | [i] -> (
    match Array.get fields i with
    | name, sh, Base ly -> name, sh, ly
    | name, sh, Product prod_layouts ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf
          "Product type found: record field flattening has failed for field %a \
           with layout %a"
          (Format.pp_print_option Format.pp_print_string)
          name Layout.format (Layout.Product prod_layouts)
      else
        name, sh, Sort.Value
        (* If this is a product type, then the flattening of the record fields
           has failed. *))
  | i :: subpath ->
    let field_name, field_type, field_layout = Array.get fields i in
    let field_name = Option.value ~default:("." ^ Int.to_string i) field_name in
    let field_name_with_projection = field_name_with_path field_name subpath in
    ( Some field_name_with_projection,
      Shape.leaf' None,
      (* CR sspies: To properly support unboxed records in mixed records, we we
         need to propagate the right shape information here. *)
      project_layout field_layout subpath )

let flatten_fields_in_mixed_record ~(mixed_block_shapes : Layout.t array)
    (fields : Layout.t projected_field list) =
  (* We go to arrays and back because it makes the reordering of the fields via
     accesses O(n) instead of O(n^2) *)
  let fields = Array.of_list fields in
  let mixed_block_shapes =
    Array.map layout_to_types_layout mixed_block_shapes
  in
  let reordering =
    Mixed_block_shape.of_mixed_block_elements
      ~print_locality:(fun _ _ -> ())
      (Lambda.transl_mixed_product_shape mixed_block_shapes)
  in
  let fields =
    Array.init (Mixed_block_shape.new_block_length reordering) (fun i ->
        let old_path = Mixed_block_shape.new_index_to_old_path reordering i in
        project_field_given_path fields old_path)
  in
  Array.to_list fields

(* CR sspies: This is a very hacky way of doing an unboxed variant with just a
   single constructor. DWARF variants expect to have a discriminator. So what we
   do is pick the first bit of the contents of the unboxed variant as the
   discriminator and then we simply ouput the same DWARF information for both
   cases. *)
(* This function deals with variants that are annotated with [@@unboxed]. They
   are only allowed to have a single constructor. *)
let create_attribute_unboxed_variant_die ~reference ~parent_proto_die ?name
    ~constr_name ~arg_name ~arg_layout ~arg_die () =
  let width = base_layout_to_byte_size arg_layout in
  let structure_ref = reference in
  let variant_part_ref = Proto_die.create_reference () in
  let variant_member_ref = Proto_die.create_reference () in
  let enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:width]
      ()
  in
  let structure_die =
    Proto_die.create ~reference:structure_ref ~parent:(Some parent_proto_die)
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:width]
        |> attribute_list_with_optional_name name)
      ~tag:Dwarf_tag.Structure_type ()
  in
  let variant_part_die =
    Proto_die.create ~reference:variant_part_ref ~parent:(Some structure_die)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:variant_member_ref]
      ~tag:Dwarf_tag.Variant_part ()
  in
  Proto_die.create_ignore ~reference:variant_member_ref
    ~parent:(Some variant_part_die) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference
          ~proto_die_reference:(Proto_die.reference enum_die);
        DAH.create_bit_size (Int8.of_int_exn 1);
        DAH.create_data_bit_offset ~bit_offset:(Int8.of_int_exn 0);
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ();
  for i = 0 to 1 do
    (* We create two identical discriminants. First, we create an enum case for
       both with the constructor name. *)
    Proto_die.create_ignore ~parent:(Some enum_die) ~tag:Dwarf_tag.Enumerator
      ~attribute_values:
        [ DAH.create_const_value ~value:(Int64.of_int i);
          DAH.create_name constr_name ]
      ();
    (* Then we create variant entries of the variant parts with the same
       discriminant. *)
    let constructor_variant =
      Proto_die.create ~parent:(Some variant_part_die) ~tag:Dwarf_tag.Variant
        ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
        ()
    in
    (* Lastly, we add the constructor argument as a member to the variant. *)
    Proto_die.create_ignore ~parent:(Some constructor_variant)
      ~tag:Dwarf_tag.Member
      ~attribute_values:
        ([ DAH.create_type_from_reference ~proto_die_reference:arg_die;
           DAH.create_byte_size_exn ~byte_size:width;
           DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0)
         ]
        |> attribute_list_with_optional_name arg_name)
      ()
  done

let create_complex_variant_die ~reference ~parent_proto_die ?name
    simple_constructors
    (complex_constructors :
      (string * (string option * Proto_die.reference * base_layout) list) list)
    =
  let complex_constructors_names =
    List.map (fun (name, _) -> name) complex_constructors
  in
  Debugging_the_compiler.add_enum ~reference
    (simple_constructors @ complex_constructors_names);
  let value_size = Arch.size_addr in
  let variant_part_immediate_or_pointer =
    let int_or_ptr_structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~attribute_values:
          ([DAH.create_byte_size_exn ~byte_size:value_size]
          |> attribute_list_with_optional_name name)
        ~tag:Dwarf_tag.Structure_type ()
    in
    Proto_die.create ~parent:(Some int_or_ptr_structure) ~attribute_values:[]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let enum_immediate_or_pointer =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:[DAH.create_byte_size_exn ~byte_size:value_size]
        ()
    in
    Debugging_the_compiler.add_enum
      ~reference:(Proto_die.reference enum_die)
      ["Immediate"; "Pointer"];
    List.iteri
      (fun i name ->
        Proto_die.create_ignore ~parent:(Some enum_die)
          ~tag:Dwarf_tag.Enumerator
          ~attribute_values:
            [ DAH.create_name name;
              DAH.create_const_value ~value:(Int64.of_int i) ]
          ())
      ["Pointer"; "Immediate"];
    enum_die
  in
  let _discriminant_immediate_or_pointer =
    let member_die =
      Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
        ~attribute_values:
          [ DAH.create_type ~proto_die:enum_immediate_or_pointer;
            DAH.create_bit_size (Int8.of_int_exn 1);
            DAH.create_data_bit_offset ~bit_offset:(Int8.of_int_exn 0);
            DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
            (* Making a member artificial will mark the struct as artificial,
               which will not print the enum name when the struct is a
               variant. *)
            DAH.create_artificial () ]
        ~tag:Dwarf_tag.Member ()
    in
    Debugging_the_compiler.add
      ~reference:(Proto_die.reference member_die)
      ("discriminant "
      ^ Asm_targets.Asm_label.encode
          (Proto_die.reference enum_immediate_or_pointer));
    Proto_die.add_or_replace_attribute_value variant_part_immediate_or_pointer
      (DAH.create_discr ~proto_die_reference:(Proto_die.reference member_die))
  in
  let _enum_simple_constructor =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:[DAH.create_byte_size_exn ~byte_size:value_size]
        ()
    in
    Debugging_the_compiler.add_enum
      ~reference:(Proto_die.reference enum_die)
      simple_constructors;
    List.iteri
      (fun i name ->
        Proto_die.create_ignore ~parent:(Some enum_die)
          ~tag:Dwarf_tag.Enumerator
          ~attribute_values:
            [ DAH.create_const_value ~value:(Int64.of_int i);
              DAH.create_name name ]
          ())
      simple_constructors;
    let variant_immediate_case =
      Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
        ~tag:Dwarf_tag.Variant
        ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 1)]
        ()
    in
    let discr =
      Proto_die.create ~parent:(Some variant_immediate_case)
        ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_type ~proto_die:enum_die;
            DAH.create_bit_size (Int8.of_int_exn ((value_size * 8) - 1));
            DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
            DAH.create_data_bit_offset ~bit_offset:(Int8.of_int_exn 1) ]
        ()
    in
    Debugging_the_compiler.add_alias
      ~from_ref:(Proto_die.reference discr)
      ~to_ref:(Proto_die.reference enum_die)
  in
  let _variant_complex_constructors =
    let ptr_case_structure =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_ocaml_offset_record_from_pointer
              ~value:(Int64.of_int (-value_size)) ]
        ()
    in
    let _attached_structure_to_pointer_variant =
      let ptr_case_pointer_to_structure =
        Proto_die.create ~parent:(Some parent_proto_die)
          ~tag:Dwarf_tag.Reference_type
          ~attribute_values:
            [ DAH.create_byte_size_exn ~byte_size:value_size;
              DAH.create_type ~proto_die:ptr_case_structure ]
          ()
      in
      Debugging_the_compiler.add_ptr
        ~reference:(Proto_die.reference ptr_case_pointer_to_structure)
        ~inner:(Proto_die.reference ptr_case_structure);
      let variant_pointer =
        Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
          ~tag:Dwarf_tag.Variant
          ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 0)]
          ()
      in
      let ptr_mem =
        Proto_die.create ~parent:(Some variant_pointer) ~tag:Dwarf_tag.Member
          ~attribute_values:
            [ DAH.create_type ~proto_die:ptr_case_pointer_to_structure;
              DAH.create_data_member_location_offset
                ~byte_offset:(Int64.of_int 0) ]
          ()
      in
      Debugging_the_compiler.add_alias
        ~from_ref:(Proto_die.reference ptr_mem)
        ~to_ref:(Proto_die.reference ptr_case_pointer_to_structure)
    in
    let variant_part_pointer =
      Proto_die.create ~parent:(Some ptr_case_structure) ~attribute_values:[]
        ~tag:Dwarf_tag.Variant_part ()
    in
    let _enum_complex_constructor =
      let enum_die =
        Proto_die.create ~parent:(Some parent_proto_die)
          ~tag:Dwarf_tag.Enumeration_type
          ~attribute_values:[DAH.create_byte_size_exn ~byte_size:1]
          ()
      in
      List.iteri
        (fun i (name, _) ->
          Proto_die.create_ignore ~parent:(Some enum_die)
            ~tag:Dwarf_tag.Enumerator
            ~attribute_values:
              [ DAH.create_const_value ~value:(Int64.of_int i);
                DAH.create_name name ]
            ())
        complex_constructors;
      let discriminant =
        Proto_die.create ~parent:(Some variant_part_pointer)
          ~attribute_values:
            [ DAH.create_type ~proto_die:enum_die;
              DAH.create_data_member_location_offset
                ~byte_offset:(Int64.of_int 0) ]
          ~tag:Dwarf_tag.Member ()
      in
      Proto_die.add_or_replace_attribute_value variant_part_pointer
        (DAH.create_discr
           ~proto_die_reference:(Proto_die.reference discriminant))
    in
    List.iteri
      (fun i (constructor_name, fields) ->
        let subvariant =
          Proto_die.create ~parent:(Some variant_part_pointer)
            ~tag:Dwarf_tag.Variant
            ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
            ()
        in
        Debugging_the_compiler.add
          ~reference:(Proto_die.reference subvariant)
          constructor_name;
        let offset = ref 0 in
        List.iter
          (fun (field_name, field_type, ly) ->
            let member_size = base_layout_to_byte_size_in_mixed_block ly in
            let member_die =
              Proto_die.create ~parent:(Some subvariant) ~tag:Dwarf_tag.Member
                ~attribute_values:
                  [ DAH.create_data_member_location_offset
                      ~byte_offset:(Int64.of_int (!offset + value_size));
                    (* members start after the block header, hence we add
                       [value_size] *)
                    DAH.create_byte_size_exn ~byte_size:member_size;
                    DAH.create_type_from_reference
                      ~proto_die_reference:field_type ]
                ()
            in
            Debugging_the_compiler.add_alias
              ~from_ref:(Proto_die.reference member_die)
              ~to_ref:field_type;
            offset := !offset + member_size;
            match field_name with
            | Some name ->
              Proto_die.add_or_replace_attribute_value member_die
                (DAH.create_name name)
            | None -> ())
          fields)
      complex_constructors
  in
  ()

type immediate_or_pointer =
  | Immediate
  | Pointer

let tag_bit = function Immediate -> 1 | Pointer -> 0

let create_immediate_or_block ~reference ~parent_proto_die ?name ~immediate_type
    ~pointer_type () =
  let value_size = Arch.size_addr in
  let int_or_ptr_structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:value_size]
        |> attribute_list_with_optional_name name)
      ~tag:Dwarf_tag.Structure_type ()
  in
  (* We create the reference early to use it already for the variant, before we
     allocate the child die for the discriminant below. *)
  let discriminant_reference = Proto_die.create_reference () in
  let variant_part_immediate_or_pointer =
    Proto_die.create ~parent:(Some int_or_ptr_structure)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:discriminant_reference]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:value_size]
      ()
  in
  Debugging_the_compiler.add_enum
    ~reference:(Proto_die.reference enum_die)
    ["Immediate"; "Pointer"];
  List.iter
    (fun elem ->
      Proto_die.create_ignore ~parent:(Some enum_die) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [DAH.create_const_value ~value:(Int64.of_int (tag_bit elem))]
        ())
    [Immediate; Pointer];
  let _discriminant_die =
    Proto_die.create ~reference:discriminant_reference
      ~parent:(Some variant_part_immediate_or_pointer)
      ~attribute_values:
        [ DAH.create_type ~proto_die:enum_die;
          DAH.create_bit_size (Int8.of_int_exn 1);
          DAH.create_data_bit_offset ~bit_offset:(Int8.of_int_exn 0);
          DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
          (* Making a member artificial will mark the struct as artificial,
             which will not print the enum name when the struct is a variant. *)
          DAH.create_artificial () ]
      ~tag:Dwarf_tag.Member ()
  in
  let variant_immediate_case =
    Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 1)]
      ()
  in
  (* Unlike in the code above, we include the tag bit in this representation. *)
  Proto_die.create_ignore ~parent:(Some variant_immediate_case)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:immediate_type;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ();
  let variant_pointer =
    Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 0)]
      ()
  in
  Proto_die.create_ignore ~parent:(Some variant_pointer) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:pointer_type;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ()

(*= The runtime representation of polymorphic variants is different from that
    of regular blocks. At runtime, the variant type

      [type t = [`Foo | `Bar of int | `Baz of int * string]]

    is represented as follows:

    For the constant constructors (i.e., here [`Foo]), the representation is
    the hash value of the constructor name tagged as an immediate. Specifically,
    it is [(Btype.hash_variant name) * 2 + 1], where [name] does not include the
    backtick.

    For the inconstant constructors (i.e., here [`Bar] and [`Baz]), the
    representation is a pointer to a block with the following layout:

      ---------------------------------------------------------
      | tagged constructor hash | arg 1 | arg 2 | ... | arg n |
      ---------------------------------------------------------

    In other words, the first field (offset 0) is the hash of the constructor
    name (tagged as in the constant constructor case) and the subsequent fields
    store the arguments of the constructor.
*)

let create_type_shape_to_dwarf_die_poly_variant ~reference ~parent_proto_die
    ?name constructors =
  let enum_constructor_for_poly_variant ~parent name =
    let hash = Btype.hash_variant name in
    let tagged_constructor_hash =
      Int64.add (Int64.mul (Int64.of_int hash) 2L) 1L
    in
    Proto_die.create_ignore ~parent:(Some parent) ~tag:Dwarf_tag.Enumerator
      ~attribute_values:
        [ DAH.create_const_value ~value:tagged_constructor_hash;
          DAH.create_name ("`" ^ name) ]
      ();
    tagged_constructor_hash
  in
  let simple_constructors, complex_constructors =
    List.partition_map
      (fun ({ pv_constr_name; pv_constr_args } : _ S.poly_variant_constructor) ->
        match pv_constr_args with
        | [] -> Left pv_constr_name
        | _ :: _ -> Right (pv_constr_name, pv_constr_args))
      constructors
  in
  (* For the constant constructors, it is enough to create an enum with the
     right numbers for the constructor labels. *)
  let simple_constructor_enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
      ()
  in
  List.iter
    (fun constr_name ->
      ignore
        (enum_constructor_for_poly_variant ~parent:simple_constructor_enum_die
           constr_name))
    simple_constructors;
  (* For the inconstant constructors, we create a reference to a structure. The
     structure uses the first field in the block to discriminate between the
     different constructor cases. *)
  let complex_constructor_enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
      ()
  in
  let complex_constructors_struct =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
      (* CR sspies: This is not really the width of the structure type, but the
         code seems to work fine. The true width of the block depends on how
         many arguments the constructor has. *)
      ()
  in
  let constructor_discriminant_ref = Proto_die.create_reference () in
  let variant_part_constructor =
    Proto_die.create ~parent:(Some complex_constructors_struct)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:constructor_discriminant_ref]
      ~tag:Dwarf_tag.Variant_part ()
  in
  Proto_die.create_ignore ~reference:constructor_discriminant_ref
    ~parent:(Some complex_constructors_struct) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:complex_constructor_enum_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ();
  List.iter
    (fun (name, args) ->
      let tag_value =
        enum_constructor_for_poly_variant ~parent:complex_constructor_enum_die
          name
      in
      let constructor_variant =
        Proto_die.create ~parent:(Some variant_part_constructor)
          ~tag:Dwarf_tag.Variant
          ~attribute_values:[DAH.create_discr_value ~value:tag_value]
          ()
      in
      List.iteri
        (fun i arg ->
          Proto_die.create_ignore ~parent:(Some constructor_variant)
            ~tag:Dwarf_tag.Member
            ~attribute_values:
              [ DAH.create_type_from_reference ~proto_die_reference:arg;
                DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
                (* We add an offset of [Arch.size_addr], because the first field
                   in the block stores the hash of the constructor name. *)
                DAH.create_data_member_location_offset
                  ~byte_offset:(Int64.of_int ((i + 1) * Arch.size_addr)) ]
            ())
        args)
    complex_constructors;
  let ptr_case_pointer_to_structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Reference_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_type ~proto_die:complex_constructors_struct ]
      ()
  in
  create_immediate_or_block ~reference ?name ~parent_proto_die
    ~immediate_type:simple_constructor_enum_die
    ~pointer_type:ptr_case_pointer_to_structure ()

let create_exception_die ~reference ~fallback_value_die ~parent_proto_die ?name
    () =
  let exn_structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:Arch.size_addr]
        |> attribute_list_with_optional_name name)
      ()
  in
  let constructor_ref = Proto_die.create_reference () in
  Proto_die.create_ignore ~parent:(Some exn_structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:constructor_ref;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:0L;
        DAH.create_name "exn" ]
    ();
  Proto_die.create_ignore ~parent:(Some exn_structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:0L;
        DAH.create_name "raw" ]
    ();
  (* CR sspies: Instead of printing the raw exception, it would be nice if we
     could encode this as a variant. Unfortunately, the DWARF LLDB support is
     not expressive enough to support a variant, whose discriminant is not
     directly a member of the surrounding struct. Moreover, for the number of
     arguments, we would need support for some form of arrays without a pointer
     indirection. *)
  let structure_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_ocaml_offset_record_from_pointer
            ~value:(Int64.of_int (-Arch.size_addr)) ]
      ()
  in
  Proto_die.create_ignore ~reference:constructor_ref
    ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Reference_type
    ~attribute_values:
      [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_type ~proto_die:structure_type ]
    ();
  let tag_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:1;
          DAH.create_encoding ~encoding:Encoding_attribute.signed ]
      ~tag:Dwarf_tag.Base_type ()
  in
  let exception_tag_discriminant_ref = Proto_die.create_reference () in
  Proto_die.create_ignore ~parent:(Some structure_type)
    ~reference:exception_tag_discriminant_ref ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type ~proto_die:tag_type;
        DAH.create_byte_size_exn ~byte_size:1;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
        DAH.create_artificial () ]
    ();
  let variant_part_exception =
    Proto_die.create ~parent:(Some structure_type)
      ~attribute_values:
        [DAH.create_discr ~proto_die_reference:exception_tag_discriminant_ref]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let exception_without_arguments_variant =
    Proto_die.create ~parent:(Some variant_part_exception)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:248L]
      ()
  in
  Proto_die.create_ignore ~parent:(Some exception_without_arguments_variant)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:8L;
        DAH.create_name "name" ]
    ();
  Proto_die.create_ignore ~parent:(Some exception_without_arguments_variant)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:16L;
        DAH.create_name "id" ]
    ();
  let exception_with_arguments_variant =
    Proto_die.create ~parent:(Some variant_part_exception)
      ~tag:Dwarf_tag.Variant
      ~attribute_values:[DAH.create_discr_value ~value:0L]
      ()
  in
  let inner_exn_block =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:(3 * Arch.size_addr);
          DAH.create_ocaml_offset_record_from_pointer ~value:(-8L) ]
      ()
  in
  Proto_die.create_ignore ~parent:(Some inner_exn_block) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:8L;
        DAH.create_name "name" ]
    ();
  Proto_die.create_ignore ~parent:(Some inner_exn_block) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference ~proto_die_reference:fallback_value_die;
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:16L;
        DAH.create_name "id" ]
    ();
  let outer_reference =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Reference_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_type_from_reference
            ~proto_die_reference:(Proto_die.reference inner_exn_block) ]
      ()
  in
  Proto_die.create_ignore ~parent:(Some exception_with_arguments_variant)
    ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_type_from_reference
          ~proto_die_reference:(Proto_die.reference outer_reference);
        DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
        DAH.create_data_member_location_offset ~byte_offset:8L ]
    ()

let create_tuple_die ~reference ~parent_proto_die ?name fields =
  let structure_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        ([DAH.create_byte_size_exn ~byte_size:(List.length fields * 8)]
        |> attribute_list_with_optional_name name)
      ()
  in
  List.iteri
    (fun i field_die ->
      let member_attributes =
        [ DAH.create_type_from_reference ~proto_die_reference:field_die;
          DAH.create_data_member_location_offset
            ~byte_offset:(Int64.of_int (8 * i)) ]
      in
      let member =
        Proto_die.create ~parent:(Some structure_type) ~tag:Dwarf_tag.Member
          ~attribute_values:member_attributes ()
      in
      Debugging_the_compiler.add_alias
        ~from_ref:(Proto_die.reference member)
        ~to_ref:field_die)
    fields;
  wrap_die_under_a_pointer ~proto_die:structure_type ~reference
    ~parent_proto_die

let unboxed_base_type_to_simd_vec_split (x : S.Predef.unboxed) =
  match x with
  | Unboxed_simd s -> Some s
  | Unboxed_float | Unboxed_float32 | Unboxed_nativeint | Unboxed_int64
  | Unboxed_int32 ->
    None

type vec_split_properties =
  { encoding : Encoding_attribute.t;
    count : int;
    size : int
  }

let vec_split_to_properties (vec_split : S.Predef.simd_vec_split) =
  let signed = Encoding_attribute.signed in
  let float = Encoding_attribute.float in
  match vec_split with
  | Int8x16 -> { encoding = signed; count = 16; size = 1 }
  | Int16x8 -> { encoding = signed; count = 8; size = 2 }
  | Int32x4 -> { encoding = signed; count = 4; size = 4 }
  | Int64x2 -> { encoding = signed; count = 2; size = 8 }
  | Float32x4 -> { encoding = float; count = 4; size = 4 }
  | Float64x2 -> { encoding = float; count = 2; size = 8 }
  | Int8x32 -> { encoding = signed; count = 32; size = 1 }
  | Int16x16 -> { encoding = signed; count = 16; size = 2 }
  | Int32x8 -> { encoding = signed; count = 8; size = 4 }
  | Int64x4 -> { encoding = signed; count = 4; size = 8 }
  | Float32x8 -> { encoding = float; count = 8; size = 4 }
  | Float64x4 -> { encoding = float; count = 4; size = 8 }
  | Int8x64 -> { encoding = signed; count = 64; size = 1 }
  | Int16x32 -> { encoding = signed; count = 32; size = 2 }
  | Int32x16 -> { encoding = signed; count = 16; size = 4 }
  | Int64x8 -> { encoding = signed; count = 8; size = 8 }
  | Float32x16 -> { encoding = float; count = 16; size = 4 }
  | Float64x8 -> { encoding = float; count = 8; size = 8 }

let create_simd_vec_split_base_layout_die ~reference ~parent_proto_die ?name
    ~byte_size ~(split : S.Predef.simd_vec_split option) () =
  match split with
  | None ->
    Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Base_type
      ~attribute_values:
        ([ DAH.create_encoding ~encoding:Encoding_attribute.unsigned;
           DAH.create_byte_size_exn ~byte_size ]
        |> attribute_list_with_optional_name name)
      ()
  | Some vec_split ->
    let structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          ([DAH.create_byte_size_exn ~byte_size]
          |> attribute_list_with_optional_name name)
        ()
    in
    let { encoding; count; size } = vec_split_to_properties vec_split in
    let base_type =
      Proto_die.create ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Base_type
        ~attribute_values:
          [ DAH.create_encoding ~encoding;
            DAH.create_byte_size_exn ~byte_size:size ]
        ()
    in
    for i = 0 to count - 1 do
      Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_type_from_reference
              ~proto_die_reference:(Proto_die.reference base_type);
            DAH.create_data_member_location_offset
              ~byte_offset:(Int64.of_int (i * size)) ]
        ()
    done

let create_base_layout_type ?(simd_vec_split = None) ~reference
    (sort : base_layout) ?name ~parent_proto_die ~fallback_value_die () =
  let byte_size = base_layout_to_byte_size sort in
  match sort with
  | Value ->
    create_typedef_die ~reference ~parent_proto_die ?name fallback_value_die
  | Float32 | Float64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.float
  | Void | Bits8 | Bits16 | Bits32 | Bits64 | Word | Untagged_immediate ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.signed
  | Vec128 | Vec256 | Vec512 ->
    create_simd_vec_split_base_layout_die ~reference ~parent_proto_die ?name
      ~byte_size ~split:simd_vec_split ()

(* In the rare case that we emit product layouts (e.g., inside arrays), this
   function will create a struct die. It does not use SIMD vector splits. *)
(* CR sspies: In the future, try to find a unifying approach for all the
   different packing strategies. Reconsider the treatment of layouts. *)
(* CR sspies: This code, especially for unboxed products in arrays, is still
   untested.*)
let rec create_packed_layout_type (layout : Layout.t) ~parent_proto_die
    ~fallback_value_die =
  match layout with
  | Base Value -> fallback_value_die, Arch.size_addr
  | Base b ->
    let encoding =
      match b with
      | Value -> assert false (* ruled out by the previous case *)
      | Float32 | Float64 -> Encoding_attribute.float
      | Void | Bits8 | Bits16 | Bits32 | Bits64 | Word | Untagged_immediate ->
        Encoding_attribute.signed
      | Vec128 | Vec256 | Vec512 -> Encoding_attribute.unsigned
    in
    let packed_byte_size = base_layout_to_byte_size_in_mixed_block b in
    (* size when packed into a block/struct *)
    let byte_size = base_layout_to_byte_size b in
    (* size of the actual data *)
    let die =
      Proto_die.create ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Base_type
        ~attribute_values:
          [DAH.create_byte_size_exn ~byte_size; DAH.create_encoding ~encoding]
        ()
    in
    Proto_die.reference die, packed_byte_size
  | Product p ->
    let components =
      List.map
        (create_packed_layout_type ~parent_proto_die ~fallback_value_die)
        p
    in
    let packed_byte_size =
      List.fold_left
        (fun acc (_, component_packed_byte_size) ->
          acc + component_packed_byte_size)
        0 components
    in
    let die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:[DAH.create_byte_size_exn ~byte_size:packed_byte_size]
        ()
    in
    let offset = ref 0 in
    List.iter
      (fun (reference, component_packed_byte_size) ->
        Proto_die.create_ignore ~parent:(Some die) ~tag:Dwarf_tag.Member
          ~attribute_values:
            [ DAH.create_type_from_reference ~proto_die_reference:reference;
              DAH.create_data_member_location_offset
                ~byte_offset:(Int64.of_int !offset) ]
          ();
        offset := !offset + component_packed_byte_size)
      components;
    Proto_die.reference die, packed_byte_size

module Shape_with_layout = struct
  type t =
    { type_shape : Shape.t;
      type_layout : Layout.t
    }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare
    (* CR sspies: Fix compare and equals on this type. Move the module to type
       shape once it is more cleaned up. *)

    let print fmt { type_shape; type_layout } =
      Format.fprintf fmt "%a @ %a" Shape.print type_shape Layout.format
        type_layout

    let hash { type_shape; type_layout } =
      Hashtbl.hash (type_shape.hash, type_layout)

    let equal ({ type_shape = x1; type_layout = y1 } : t)
        ({ type_shape = x2; type_layout = y2 } : t) =
      Shape.equal x1 x2 && Layout.equal y1 y2

    let output _oc _t = Misc.fatal_error "unimplemented"
  end)
end

module Cache = Shape_with_layout.Tbl
(* We add a cache based on type shapes to increase sharing the resulting DWARF.
   The cache caches the combination of type shape and type layout. We make sure
   to only cache closed shapes, as open types would require a context that binds
   the relevant de Bruijn indices. *)

(** This cache maps unnamed type shapes to their references. *)
let cache = Cache.create 16

let add_to_cache (type_shape : Shape.t) (type_layout : Layout.t) reference
    ~rec_env =
  (* [rec_env] being empty means that the shape is closed. *)
  if S.DeBruijn_env.is_empty rec_env
  then Cache.add cache { type_shape; type_layout } reference

let find_in_cache (type_shape : Shape.t) (type_layout : Layout.t) ~rec_env =
  if S.DeBruijn_env.is_empty rec_env
  then Cache.find_opt cache { type_shape; type_layout }
  else None

(** This second cache is for named type shapes. Every type name should be
    associated with at most one DWARF die, so this cache maps type names to
    type shapes and DWARF dies. *)
let name_cache = String.Tbl.create 16

(* CR sspies: We have to be careful here, because LLDB currently disambiguates
   type dies based on the type name; however, we can easily have types with the
   same name and different definitions. The function [type_shape_to_dwarf_die]
   only works for types without names, and types with names are handled in
   [type_shape_to_dwarf_die_with_aliased_name] below. *)
let rec type_shape_to_dwarf_die (type_shape : Shape.t)
    (type_layout : base_layout) ~parent_proto_die ~fallback_value_die ~rec_env =
  match find_in_cache type_shape (Base type_layout) ~rec_env with
  | Some reference -> reference
  | None ->
    let reference = Proto_die.create_reference () in
    add_to_cache type_shape (Base type_layout) reference ~rec_env;
    let name = None in
    (* Instead of omitting the name argument below, we fix it to be [None] here
       such that it is easier to change this code if in the future we want to
       change how the names of types are handled. *)
    (match type_shape.desc with
    | Leaf ->
      create_base_layout_type ~reference type_layout ?name ~parent_proto_die
        ~fallback_value_die ()
    | Constr _ ->
      create_base_layout_type ~reference type_layout ?name ~parent_proto_die
        ~fallback_value_die ()
      (* CR sspies: The [Constr] case should never trigger. Consider promoting
         this to a louder error. For now, we are conservative and allow it via a
         fallback. *)
    | Unboxed_tuple _ ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf "unboxed tuples cannot have base layout %a:@ %a"
          Layout.format (Layout.Base type_layout) S.print type_shape
      else
        create_base_layout_type ~reference type_layout ?name ~parent_proto_die
          ~fallback_value_die ()
    | Tuple fields ->
      type_shape_to_dwarf_die_tuple ~reference ~parent_proto_die
        ~fallback_value_die ?name ~rec_env fields
    | Predef (predef, args) ->
      type_shape_to_dwarf_die_predef ~reference ?name ~parent_proto_die
        ~fallback_value_die ~rec_env predef args
    | Poly_variant fields ->
      type_shape_to_dwarf_die_poly_variant ~reference ?name ~parent_proto_die
        ~fallback_value_die ~constructors:fields ~rec_env ()
    | Arrow _ ->
      type_shape_to_dwarf_die_arrow ~reference ?name ~parent_proto_die
        ~fallback_value_die ()
    | Record { fields; kind = Record_boxed | Record_floats } ->
      let fields =
        List.map
          (fun (name, _, type_shape, (type_layout : Layout.t)) ->
            let base_layout =
              match type_layout with
              | Base base_layout -> base_layout
              | Product _ ->
                if !Clflags.dwarf_pedantic
                then
                  Misc.fatal_errorf
                    "[Record_boxed] and [Record_floats] records must only have \
                     fields of [Base] layout:@ %a"
                    S.print type_shape
                else Sort.Value
            in
            ( name,
              Arch.size_addr,
              (* All fields here are machine word width *)
              type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
                type_shape ~rec_env base_layout ))
          fields
      in
      create_record_die ~reference ~parent_proto_die ?name ~fields ()
    | Record
        { fields = [(field_name, _, sh, Base base_layout)];
          kind = Record_unboxed
        } ->
      let field_die =
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die sh
          base_layout ~rec_env
      in
      let field_size = base_layout_to_byte_size base_layout in
      create_attribute_unboxed_record_die ~reference ~parent_proto_die ?name
        ~field_name ~field_size ~field_die ()
    | Record { fields; kind = Record_mixed mixed_block_shapes } ->
      let fields =
        List.map (fun (name, _, sh, ly) -> Some name, sh, ly) fields
      in
      let fields = flatten_fields_in_mixed_record ~mixed_block_shapes fields in
      let fields =
        List.map
          (fun (name, type_shape, base_layout) ->
            match name with
            | Some name ->
              ( name,
                base_layout_to_byte_size_in_mixed_block base_layout,
                type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
                  type_shape base_layout ~rec_env )
            | None ->
              if !Clflags.dwarf_pedantic
              then
                (* CR sspies: This should never happen, since we provide a name
                   above. However, when running in non-pedantic mode, [None]
                   could be returned. *)
                Misc.fatal_errorf
                  "Type_shape: mixed record shape with no name: %a" S.print
                  type_shape
              else "_unknown", 0, fallback_value_die)
          fields
      in
      create_record_die ~reference ~parent_proto_die ?name ~fields ()
    | Record { fields = _; kind = Record_unboxed_product }
    | Record { fields = [(_, _, _, Product _)]; kind = Record_unboxed } ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf
          "This form of record shape should have been flattened by \
           [flatten_shape]: %a"
          S.print type_shape
      else
        create_base_layout_type ~reference type_layout ?name ~parent_proto_die
          ~fallback_value_die ()
    | Record { fields = [] | _ :: _ :: _; kind = Record_unboxed } ->
      Misc.fatal_errorf
        "Records with [@unboxed] attributes must have exactly one field:@ %a"
        S.print type_shape
      (* [@@unboxed] records must have exactly one field; this should have been
         detected by earlier transformations such as at the point where the
         shape was created. *)
    | Variant constructors -> (
      let simple_constructors, complex_constructors =
        List.partition_map
          (fun { S.name; constr_uid; kind; args } ->
            match args with
            | [] -> Left name
            | _ :: _ -> Right { S.name; constr_uid; kind; args })
          constructors
      in
      match complex_constructors with
      | [] ->
        create_simple_variant_die ~reference ~parent_proto_die ?name
          simple_constructors
      | _ :: _ ->
        (* We flatten the fields of the constructors first *)
        let complex_constructors =
          List.map
            (fun { S.name; kind = mixed_block_shapes; args } ->
              ( name,
                flatten_fields_in_mixed_record ~mixed_block_shapes
                  (List.map
                     (fun { S.field_name = name;
                            field_uid = _;
                            field_value = sh, ly
                          } -> name, sh, ly)
                     args) ))
            complex_constructors
        in
        let complex_constructors =
          List.map
            (fun (constr_name, fields) ->
              ( constr_name,
                List.map
                  (fun (field_name, sh, ly) ->
                    ( field_name,
                      type_shape_to_dwarf_die ~parent_proto_die
                        ~fallback_value_die ~rec_env sh ly,
                      ly ))
                  fields ))
            complex_constructors
        in
        create_complex_variant_die ~reference ~parent_proto_die ?name
          simple_constructors complex_constructors)
    | Variant_unboxed { name = constr_name; arg_name; arg_shape; arg_layout } ->
      let base_layout =
        match arg_layout with
        | Base base_layout -> base_layout
        | Product _ ->
          if !Clflags.dwarf_pedantic
          then
            Misc.fatal_errorf
              "[Product] layout in [Variant_unboxed] constructor is not \
               allowed:@ %a"
              S.print type_shape
          else Sort.Value
      in
      let arg_die =
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die arg_shape
          base_layout ~rec_env
      in
      create_attribute_unboxed_variant_die ~reference ~parent_proto_die ?name
        ~constr_name ~arg_name ~arg_layout:base_layout ~arg_die ()
    | Rec_var de_bruijn_index -> (
      match S.DeBruijn_env.get_opt rec_env ~de_bruijn_index with
      | Some reference' ->
        create_typedef_die ~reference ~parent_proto_die ?name reference'
      | None ->
        if !Clflags.dwarf_pedantic
        then
          Misc.fatal_errorf
            "Recursive variable environment lookup failed: rec_env returned \
             None for de Bruijn index %a"
            S.DeBruijn_index.print de_bruijn_index
        else
          create_typedef_die ~reference ~parent_proto_die ?name
            fallback_value_die)
    | Mu sh ->
      let reference' =
        (* CR sspies: We are creating two typedefs for recursive types. One
           should be enough. *)
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die sh
          type_layout
          ~rec_env:(S.DeBruijn_env.push rec_env reference)
      in
      create_typedef_die ~reference ~parent_proto_die ?name reference'
    | Alias sh ->
      let reference' =
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die sh
          type_layout ~rec_env
      in
      (* CR sspies: This typedef is only needed if the name is [Some]. Reduce
         the number of typedefs here. *)
      create_typedef_die ~reference ~parent_proto_die ?name reference'
    | App _ | Error _ | Proj _ ->
      (* In these cases, something has gone wrong during reduction, because we
         do not have sufficient information. *)
      create_base_layout_type ~reference type_layout ?name ~parent_proto_die
        ~fallback_value_die ()
    | Proj_decl _ ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf
          "Projections from mutually recursive definitions should have been \
           resolved at this point. Found %a"
          Shape.print type_shape
      else
        create_base_layout_type ~reference type_layout ?name ~parent_proto_die
          ~fallback_value_die ()
    | Var _
    (* CR sspies: This case is currently triggered for free type variables. It
       should be taken care of when revisiting the layout computation. *)
    | Abs _ | Comp_unit _ | Struct _ | Mutrec _ ->
      (* CR sspies: Change this to a loud error in the future. These cases
         should not happen with the new shapes, but at least the [Struct] case
         does happen for the old Merlin shapes. Investigate when they do happen.
         For now, we simply return the base layout. *)
      create_base_layout_type ~reference type_layout ?name ~parent_proto_die
        ~fallback_value_die ());
    reference

and type_shape_to_dwarf_die_tuple ?name ~reference ~parent_proto_die
    ~fallback_value_die ~rec_env fields =
  let fields =
    List.map
      (fun sh ->
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die ~rec_env
          sh Sort.Value)
      fields
  in
  (* CR sspies: In the future, tuples will also be allowed to have unboxed
     fields. This code will need adjusting similar to other places that require
     computing the layout from the type. *)
  create_tuple_die ~reference ~parent_proto_die ?name fields

and type_shape_to_dwarf_die_predef ?name ~reference ~parent_proto_die
    ~fallback_value_die ~rec_env (predef : Shape.Predef.t) args =
  match predef, args with
  | Array, [element_type_shape] -> (
    let argument_layout =
      Type_shape.estimate_layout_from_type_shape element_type_shape
    in
    (* CR sspies: Check whether the elements of an array are always values and,
       if not, where that information is maintained.

       mshinwell: we need to handle unboxed arrays. See Cmm_helpers, but let's
       wait until after we change the representation of these not to use custom
       blocks.

       sspies: I've introduced a type estimation. There is a problem with this
       at the moment: The estimation can get stuck in cases where we have a type
       variable or we simply don't know. This case shows up in the [None] case
       below. We currently default to [Value] layout. *)
    let argument_layout =
      match argument_layout with
      | None -> Layout.Base Value
      (* CR sspies: Risky, can be false. A different default here would be
         better. *)
      | Some l -> l
    in
    match argument_layout with
    | Base base_layout ->
      let child_die =
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
          element_type_shape base_layout ~rec_env
      in
      create_array_die ~reference ~parent_proto_die ~child_die ?name ()
    | Product _ ->
      (* CR sspies: We handle products differently, because they are packed
         specially into arrays. Try to find a unifying approach here. *)
      let child_die, _ =
        create_packed_layout_type ~parent_proto_die ~fallback_value_die
          argument_layout
      in
      create_array_die ~reference ~parent_proto_die ~child_die ?name ())
  | Array, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf
        "[Array] predef shape must have exactly one argument:@ %a applied to %a"
        Shape.Predef.print predef
        (Format.pp_print_list Shape.print)
        args
    else
      create_base_layout_type ~reference Value ?name ~parent_proto_die
        ~fallback_value_die ()
  | Char, _ -> create_char_die ~reference ~parent_proto_die ?name ()
  | Unboxed b, _ ->
    let type_layout = Shape.Predef.unboxed_type_to_base_layout b in
    create_base_layout_type
      ~simd_vec_split:(unboxed_base_type_to_simd_vec_split b)
      ~reference type_layout ?name ~parent_proto_die ~fallback_value_die ()
  | Simd s, _ ->
    (* We represent these vectors as pointers of the form [struct {...} *].
       Their runtime representation is non-scannable mixed blocks (see
       Cmm_helpers). *)
    let base_ref = Proto_die.create_reference () in
    let byte_size = Shape.Predef.simd_vec_split_to_byte_size s in
    create_simd_vec_split_base_layout_die ~split:(Some s) ~reference:base_ref
      ~byte_size ~parent_proto_die ();
    Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Reference_type
      ~attribute_values:
        (attribute_list_with_optional_name name
           [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
             DAH.create_type_from_reference ~proto_die_reference:base_ref ])
      ()
  | Exception, _ ->
    create_exception_die ~reference ~fallback_value_die ~parent_proto_die ?name
      ()
  | ( ( Bytes | Extension_constructor | Float | Float32 | Floatarray | Int
      | Int32 | Int64 | Lazy_t | Nativeint | String ),
      _ ) ->
    create_base_layout_type ~reference Value ?name ~parent_proto_die
      ~fallback_value_die ()

and type_shape_to_dwarf_die_arrow ~reference ?name ~parent_proto_die
    ~fallback_value_die () =
  (* There is no need to inspect the argument and return value. *)
  create_typedef_die ~reference ~parent_proto_die ?name fallback_value_die

and type_shape_to_dwarf_die_poly_variant ~reference ~parent_proto_die
    ~fallback_value_die ?name ~constructors ~rec_env () =
  let constructors_with_references =
    S.poly_variant_constructors_map
      (fun sh ->
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die ~rec_env
          sh Sort.Value)
      (* At the moment, polymorphic variant constructor arguments always have
         layout [value]. *)
      constructors
  in
  create_type_shape_to_dwarf_die_poly_variant ~reference ~parent_proto_die ?name
    constructors_with_references

let rec flatten_to_base_sorts (sort : Layout.t) : base_layout list =
  match sort with
  | Base b -> [b]
  | Product sorts -> List.concat_map flatten_to_base_sorts sorts

(* This function performs the counterpart of unarization in the rest of the
   compiler. We flatten the type into a sequence that corresponds to the fields
   after unarization. In some cases, the type cannot be broken up (e.g., for
   type variables). In these cases, we produce the corresponding number of
   entries of the form [Unknown base_layout] for the fields. Otherwise, when the
   type is known, we produce [Known (type_shape, base_layout)] for the
   fields. *)

type shape_or_unknown =
  | Known of S.t * base_layout
  | Unknown of base_layout

let rec flatten_shape (type_shape : Shape.t) (type_layout : Layout.t) =
  let unknown_base_layouts layout =
    let base_sorts = flatten_to_base_sorts layout in
    List.map (fun base_sort -> Unknown base_sort) base_sorts
  in
  let known_value = [Known (type_shape, Sort.Value)] in
  match type_shape.desc, type_layout with
  | Leaf, _ -> unknown_base_layouts type_layout
  | Tuple _, Base Value ->
    known_value (* boxed tuples are only a single base layout wide *)
  | Tuple _, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf "tuple must have value layout, but got: %a"
        Layout.format type_layout
    else unknown_base_layouts type_layout
  | Unboxed_tuple shapes, _ -> (
    match type_layout with
    | Layout.Product layouts when List.compare_lengths layouts shapes = 0 ->
      let shapes_with_layout = List.combine shapes layouts in
      List.concat_map (fun (sh, ly) -> flatten_shape sh ly) shapes_with_layout
    | Layout.Product layouts ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf
          "unboxed tuple field mismatch, shape %a has %d fields, but layout %a \
           expects %d"
          Shape.print type_shape (List.length shapes) Layout.format type_layout
          (List.length layouts)
      else unknown_base_layouts type_layout
    | Layout.Base _ ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf "unboxed tuple must have product layout, but got: %a"
          Layout.format type_layout
      else unknown_base_layouts type_layout)
  | Constr _, Base b -> [Known (type_shape, b)]
  | Constr _, _ -> unknown_base_layouts type_layout
  (* CR sspies: These should not happen with support for recursive types. We
     conservatively give back defaults. *)
  | Predef _, Base base_layout -> [Known (type_shape, base_layout)]
  | Predef _, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf "predefined type must have base layout, but got: %a"
        Layout.format type_layout
    else unknown_base_layouts type_layout
  | Arrow _, Base Value -> known_value
  | Arrow _, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf "arrow must have value layout, but got: %a"
        Layout.format type_layout
    else unknown_base_layouts type_layout
  | Poly_variant _, Base Value -> known_value
  | Poly_variant _, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf "poly_variant must have value layout, but got: %a"
        Layout.format type_layout
    else unknown_base_layouts type_layout
  | ( Record { fields = _; kind = Record_boxed | Record_mixed _ | Record_floats },
      Base Value ) ->
    known_value
  | ( Record { fields = _; kind = Record_boxed | Record_mixed _ | Record_floats },
      _ ) ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf "record must have value layout, but got: %a"
        Layout.format type_layout
    else unknown_base_layouts type_layout
  | Record { fields = [(_, _, sh, ly)]; kind = Record_unboxed }, _
    when Layout.equal ly type_layout -> (
    match type_layout with
    | Product _ ->
      (* CR mshinwell: this should preserve the record type *)
      flatten_shape sh ly
    (* for unboxed products of the form [{ field: ty } [@@unboxed]] where [ty]
       is of product sort, we simply look through the unboxed product.
       Otherwise, we will create an additional DWARF entry for it. *)
    | Base b -> [Known (type_shape, b)])
  | Record { fields = [(_, _, _, ly)]; kind = Record_unboxed }, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf
        "unboxed record at different layout than its field, record layout: %a, \
         field_layout: %a"
        Layout.format type_layout Layout.format ly
    else unknown_base_layouts type_layout
  | Record { fields = ([] | _ :: _ :: _) as fields; kind = Record_unboxed }, _
    ->
    Misc.fatal_errorf "unboxed record must have exactly one field, found %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
      (List.map (fun (name, _, _, _) -> name) fields)
  | Record { fields; kind = Record_unboxed_product }, _ -> (
    match type_layout with
    | Product prod_shapes when List.compare_lengths prod_shapes fields = 0 ->
      List.concat_map (fun (_, _, sh, ly) -> flatten_shape sh ly) fields
    | Layout.Product prod_shapes ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf
          "unboxed record field mismatch, shape %a has %d fields, but layout \
           %a expects %d"
          Shape.print type_shape (List.length fields) Layout.format type_layout
          (List.length prod_shapes)
      else unknown_base_layouts type_layout
    | Layout.Base _ ->
      if !Clflags.dwarf_pedantic
      then
        Misc.fatal_errorf
          "unboxed record must have product layout, but has layout %a"
          Layout.format type_layout
      else unknown_base_layouts type_layout)
  | Variant _, Base Value -> known_value
  | Variant _, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf "variant must have value layout, but has layout %a"
        Layout.format type_layout
    else unknown_base_layouts type_layout
  | ( Variant_unboxed { name = _; arg_name = _; arg_layout; arg_shape = _ },
      Base Value )
    when Layout.equal arg_layout type_layout ->
    known_value
  | Variant_unboxed { name = _; arg_name = _; arg_layout; arg_shape = _ }, _ ->
    if !Clflags.dwarf_pedantic
    then
      Misc.fatal_errorf
        "unboxed variant must have value layout, and must have same layout as \
         its contents; got: %a and contents: %a"
        Layout.format type_layout Layout.format arg_layout
    else unknown_base_layouts type_layout
  | Rec_var i, _ ->
    unknown_base_layouts type_layout
    (* A projection should not reach the point of a recursive variable. *)
  | Mu sh, _ -> flatten_shape sh type_layout
  | Alias sh, _ -> flatten_shape sh type_layout
  | (App _ | Error _ | Proj _), _ ->
    (* In these cases, something has gone wrong during reduction, because we do
       not have sufficient information. *)
    unknown_base_layouts type_layout
  | Var _, _
  (* CR sspies: This case is currently triggered for free type variables. It
     should be taken care of when revisiting the layout computation. *)
  | (Abs _ | Comp_unit _ | Struct _ | Proj_decl _ | Mutrec _), _ ->
    (* CR sspies: Change this to a loud error in the future. These cases should
       not happen with the new shapes, but at least the [Struct] cases does
       happen for the old Merlin shapes. Investigate when they do happen. For
       now, we simply return the base layout. *)
    unknown_base_layouts type_layout

module With_cms_reduce = Shape_reduce.Make (struct
  let fuel = 10

  let projection_rules_for_merlin_enabled = false

  let cms_file_cache = String.Tbl.create 264

  let max_number_of_cms_files = ref 20
  (* A single compilation may not read more than 20 .cms files. *)
  (* CR sspies: Investigate the performance some more and the balance between
     different variables. *)

  let read_shape_from_cms ~diagnostics cms_file =
    match Load_path.find_normalized cms_file with
    | exception Not_found -> None
    | cms_path -> (
      match Cms_format.read cms_path with
      | exception Cms_format.(Error (Not_a_shape _)) ->
        if !Clflags.dwarf_pedantic
        then Misc.fatal_errorf "Version mismatch loading .cms file %s" cms_file
        else (
          Shape_reduce.Diagnostics.add_cms_file_unreadable diagnostics cms_file;
          None)
      | cms_infos -> Some cms_infos.cms_impl_shape)

  let read_shape_from_cmt ~diagnostics cmt_file =
    match Load_path.find_normalized cmt_file with
    | exception Not_found -> None
    | cmt_path -> (
      match Cmt_format.read cmt_path with
      | exception Cmt_format.(Error (Not_a_typedtree _)) ->
        if !Clflags.dwarf_pedantic
        then Misc.fatal_errorf "Version mismatch loading .cmt file %s" cmt_file
        else (
          Shape_reduce.Diagnostics.add_cms_file_unreadable diagnostics cmt_file;
          None)
      | _, Some cmt_infos -> Some cmt_infos.cmt_impl_shape
      | _ -> Some None)

  let read_unit_shape ~diagnostics ~unit_name =
    match String.Tbl.find_opt cms_file_cache unit_name with
    | Some shape ->
      Shape_reduce.Diagnostics.count_cms_file_cached diagnostics;
      shape
    | None ->
      if !max_number_of_cms_files <= 0
         (* CR sspies: This needs a command line flag. *)
      then None
      else
        let filename = String.uncapitalize_ascii unit_name in
        let cms_file = filename ^ ".cms" in
        let cmt_file = filename ^ ".cmt" in
        let shape_opt_opt =
          match read_shape_from_cms ~diagnostics cms_file with
          | Some shape -> Some shape
          | None -> read_shape_from_cmt ~diagnostics cmt_file
        in
        let shape_opt =
          match shape_opt_opt with
          | Some shape_opt ->
            decr max_number_of_cms_files;
            Shape_reduce.Diagnostics.count_cms_file_loaded diagnostics;
            shape_opt
          | None ->
            Shape_reduce.Diagnostics.add_cms_file_missing diagnostics cms_file;
            None
        in
        (* Note: [shape_opt] is an option, and we are also caching the [None]
           case. *)
        String.Tbl.add cms_file_cache unit_name shape_opt;
        shape_opt
end)

module D = Shape_reduction_diagnostics

(* Search for the first unused suffix-numbered version of [name] in the
   [name_cache] cache. If we come along a type of the same name and type shape,
   then we simply use that reference. *)
let find_unused_type_name_or_cached (name : string) (type_shape : S.t) :
    (Proto_die.reference, string) Either.t =
  let rec aux inc : _ Either.t =
    let name_suffix = if inc = 0 then "" else "/" ^ string_of_int inc in
    let name = name ^ name_suffix in
    match String.Tbl.find_opt name_cache name with
    | Some (type_shape', reference) ->
      if Shape.equal type_shape type_shape'
      then Left reference
      else aux (inc + 1)
    | None -> Right name
  in
  aux 0

(* We represent all types as DIE entries of the form [typedef ... type_name;]
   and use caching for types that have the same name and shape. For name
   conflicts, we search for the next available suffix-numbered version of the
   name, [type_name/n]. *)
let type_shape_to_dwarf_die_with_aliased_name (type_name : string)
    (type_shape : S.t) (type_layout : base_layout) ~parent_proto_die
    ~fallback_value_die : Proto_die.reference =
  match find_unused_type_name_or_cached type_name type_shape with
  | Left reference -> reference
  | Right name ->
    let unnamed_die =
      type_shape_to_dwarf_die type_shape type_layout ~parent_proto_die
        ~fallback_value_die (* note that we do not pass the type name here *)
        ~rec_env:S.DeBruijn_env.empty
    in
    let reference = Proto_die.create_reference () in
    let layout_name =
      Format.asprintf "%a" Layout.format (Layout.Base type_layout)
    in
    let full_name = name ^ " @ " ^ layout_name in
    String.Tbl.add name_cache name (type_shape, reference);
    create_typedef_die ~reference ~name:full_name ~parent_proto_die unnamed_die;
    reference

let variable_to_die state (var_uid : Uid.t) ~parent_proto_die =
  let fallback_value_die =
    Proto_die.reference (DS.value_type_proto_die state)
  in
  (* Once we reach the backend, layouts such as Product [Product [Bits64;
     Bits64]; Float64] have de facto been flattened into a sequence of base
     layouts [Bits64; Bits64; Float64]. Below, we compute the index into the
     flattened list (and later compute the flattened list itself). *)
  let uid_to_lookup, unboxed_projection =
    match var_uid with
    | Uid var_uid -> var_uid, None
    | Proj { uid = var_uid; unboxed_field = field } -> var_uid, Some field
  in
  match Shape.Uid.Tbl.find_opt Type_shape.all_type_shapes uid_to_lookup with
  | None ->
    (* CR mshinwell: we could reinstate this message under a flag *)
    (* Format.eprintf "variable_to_die: no type shape for %a@." Shape.Uid.print
       uid_to_lookup; *)
    fallback_value_die
  (* CR sspies: This is somewhat risky, since this is a variable for which we
     seem to have no declaration, and we also do not know the layout. Perhaps we
     should simply not emit any DWARF information for this variable instead.

     mshinwell: or emit an "unknown layout" type *)
  | Some { type_shape; type_name; type_layout } ->
    let reduction_diagnostics = D.create type_name type_layout in
    let shape_reduce =
      With_cms_reduce.reduce
        ~diagnostics:(D.shape_reduce_diagnostics reduction_diagnostics)
        Env.empty
    in
    D.record_before_reduction reduction_diagnostics type_shape;
    let type_shape = shape_reduce type_shape in
    D.record_after_reduction reduction_diagnostics type_shape;
    let type_shape =
      Type_shape.unfold_and_evaluate
        ~diagnostics:(D.shape_evaluation_diagnostics reduction_diagnostics)
        type_shape
    in
    D.record_after_evaluation reduction_diagnostics type_shape;
    let type_shape =
      match unboxed_projection, type_layout with
      | None, Base b -> Known (type_shape, b)
      | None, Product _ ->
        if !Clflags.dwarf_pedantic
        then
          Misc.fatal_errorf
            "uid %a: product layout not flattened by unarization for type \
             '%s':@ %a"
            Uid.print var_uid type_name S.print type_shape
        else Unknown Sort.Value
      | Some i, _ ->
        let flattened = flatten_shape type_shape type_layout in
        let flattened_length = List.length flattened in
        if i < 0 || i >= flattened_length
        then
          if !Clflags.dwarf_pedantic
          then
            Misc.fatal_errorf
              "uid %a: unboxed projection index %d out of bounds 0...%d:@ %a"
              Uid.print var_uid i (flattened_length - 1) S.print type_shape
          else Unknown Sort.Value
        else List.nth flattened i
    in
    let type_name =
      match unboxed_projection with
      | None -> type_name
      | Some i -> type_name ^ "_unboxed" ^ string_of_int i
      (* CR sspies: In case of unboxed projections, we do not have the type
         names of the individual fields available. And obtaining them in general
         is not straightforward, since they could be hidden behind a type alias
         (e.g., [type prod = #{ a: int64#; b: float# }]). What we currently do
         is match the style of unarization variables by appending "_unboxed" for
         the projections to indicate that the type is not the same. *)
    in
    D.record_before_dwarf_generation reduction_diagnostics parent_proto_die;
    let reference =
      match type_shape with
      | Known (type_shape, base_layout) ->
        let reference =
          type_shape_to_dwarf_die_with_aliased_name type_name type_shape
            base_layout ~parent_proto_die ~fallback_value_die
        in
        if Debugging_the_compiler.enabled ()
        then (
          Format.eprintf "%a has become %a@." Uid.print var_uid
            Asm_targets.Asm_label.print reference;
          Debugging_the_compiler.print ~die:parent_proto_die);
        reference
      | Unknown base_layout ->
        let reference = Proto_die.create_reference () in
        create_base_layout_type ~reference ~parent_proto_die
          ~name:("unknown @ " ^ Sort.to_string_base base_layout)
          ~fallback_value_die base_layout ();
        (* CR sspies: We do have the type name available here, so we could be
           more precise in principle. *)
        reference
    in
    D.record_after_dwarf_generation reduction_diagnostics parent_proto_die;
    D.append_diagnostics_to_dwarf_state state reduction_diagnostics;
    reference
