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

type base_layout = Sort.base

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

let wrap_die_under_a_pointer ~proto_die ~reference ~parent_proto_die =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Reference_type
    ~attribute_values:
      [DAH.create_byte_size_exn ~byte_size:8; DAH.create_type ~proto_die]
    ()

let create_array_die ~reference ~parent_proto_die ~child_die ~name =
  let array_die =
    Proto_die.create ~parent:(Some parent_proto_die) ~tag:Dwarf_tag.Array_type
      ~attribute_values:
        [ DAH.create_name name;
          DAH.create_type_from_reference ~proto_die_reference:child_die;
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
  wrap_die_under_a_pointer ~proto_die:array_die ~reference ~parent_proto_die

let create_char_die ~reference ~parent_proto_die ~name =
  (* As a char is an immediate value, we have to ignore the first bit.
     Unfortunately lldb supports bit offsets only on members of structs, so
     instead, we create a hacky enum containing all possible char values. *)
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_name name; DAH.create_byte_size_exn ~byte_size:8]
        (* CR sspies: The name here is displayed as ["enum " ^ name] in gdb, but
           correctly as [name] in lldb. *)
      ()
  in
  List.iter
    (fun i ->
      Proto_die.create_ignore ~parent:(Some enum) ~tag:Dwarf_tag.Enumerator
        ~attribute_values:
          [ DAH.create_const_value ~value:(Int64.of_int ((2 * i) + 1));
            DAH.create_name (Printf.sprintf "%C" (Char.chr i)) ]
        ())
    (List.init 256 (fun i -> i))

let create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
    ~encoding =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Base_type
    ~attribute_values:
      [ DAH.create_name name;
        DAH.create_byte_size_exn ~byte_size;
        DAH.create_encoding ~encoding ]
    ()

let create_typedef_die ~reference ~parent_proto_die ~child_die ~name =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Typedef
    ~attribute_values:
      [ DAH.create_name name;
        DAH.create_type_from_reference ~proto_die_reference:child_die ]
    ()

let create_record_die ~reference ~parent_proto_die ~name ~fields =
  let total_size =
    List.fold_left (fun acc (_, field_size, _) -> acc + field_size) 0 fields
  in
  let structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:total_size; DAH.create_name name]
      ()
  in
  let offset = ref 0 in
  List.iter
    (fun (field_name, field_size, field_die) ->
      Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_name field_name;
            DAH.create_type_from_reference ~proto_die_reference:field_die;
            DAH.create_data_member_location_offset
              ~byte_offset:(Int64.of_int !offset) ]
        ();
      offset := !offset + field_size)
    fields;
  wrap_die_under_a_pointer ~proto_die:structure ~reference ~parent_proto_die

(* The following function handles records annotated with [[@@unboxed]]. These
   may only have a single field. ("Unboxed records" of the form [#{ ... }] are
   destructed into their component parts by unarization.) *)
let create_attribute_unboxed_record_die ~reference ~parent_proto_die ~name
    ~field_die ~field_name ~field_size =
  let structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:field_size; DAH.create_name name]
      ()
  in
  Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
    ~attribute_values:
      [ DAH.create_name field_name;
        DAH.create_type_from_reference ~proto_die_reference:field_die;
        DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0) ]
    ()

let create_simple_variant_die ~reference ~parent_proto_die ~name
    ~simple_constructors =
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:8; DAH.create_name name]
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
    | Void -> Product [||])
  | Product lys -> Product (Array.of_list (List.map layout_to_types_layout lys))

let rec project_layout (layout : Layout.t) path =
  match layout, path with
  | Base b, [] -> b
  | Product p, i :: path -> project_layout (List.nth p i) path
  | _, _ -> assert false

let rec field_name_with_path base path =
  match path with
  | [] -> base
  | i :: path -> field_name_with_path base path ^ ".#" ^ Int.to_string i

type 'layout projected_field = string option * S.without_layout S.ts * 'layout

let project_field_given_path (fields : Layout.t projected_field array) path :
    base_layout projected_field =
  match path with
  | [] ->
    Misc.fatal_error
      "Empty path provided to [field_project_path]" (* field should exist *)
  | [i] -> (
    match Array.get fields i with
    | name, sh, Base ly -> name, sh, ly
    | name, sh, Product _ ->
      assert false
      (* If this is a product type, then the flattening of the record fields has
         failed. *))
  | i :: subpath ->
    let field_name, field_type, field_layout = Array.get fields i in
    let field_name = Option.value ~default:("." ^ Int.to_string i) field_name in
    let field_name_with_projection = field_name_with_path field_name subpath in
    ( Some field_name_with_projection,
      Ts_other Layout_to_be_determined,
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
      (Lambda.transl_mixed_product_shape
         ~get_value_kind:(fun _ -> Lambda.generic_value)
         (* We don't care about the value kind of values, because it is dropped
            again immediately afterwards. We only care about the layout
            remapping: we only need the reordering to get the fields right
            below. *)
         mixed_block_shapes)
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
let create_attribute_unboxed_variant_die ~reference ~parent_proto_die ~name
    ~constr_name ~arg_name ~(arg_layout : Layout.t) ~arg_die =
  let base_layout =
    match arg_layout with
    | Base base_layout -> base_layout
    | Product _ -> Misc.fatal_error "Not a base layout"
  in
  let width = base_layout_to_byte_size base_layout in
  let structure_ref = reference in
  let variant_part_ref = Proto_die.create_reference () in
  let variant_member_ref = Proto_die.create_reference () in
  let enum_die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:width; DAH.create_name name]
      ()
  in
  let structure_die =
    Proto_die.create ~reference:structure_ref ~parent:(Some parent_proto_die)
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:width; DAH.create_name name]
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
    let member_name =
      match arg_name with Some name -> [DAH.create_name name] | None -> []
    in
    Proto_die.create_ignore ~parent:(Some constructor_variant)
      ~tag:Dwarf_tag.Member
      ~attribute_values:
        ([ DAH.create_type_from_reference ~proto_die_reference:arg_die;
           DAH.create_byte_size_exn ~byte_size:width;
           DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0)
         ]
        @ member_name)
      ()
  done

let create_complex_variant_die ~reference ~parent_proto_die ~name
    ~simple_constructors
    ~(complex_constructors :
       (string * (string option * Proto_die.reference * base_layout) list) list)
    =
  let complex_constructors_names =
    List.map (fun (name, _) -> name) complex_constructors
  in
  let value_size = Arch.size_addr in
  let variant_part_immediate_or_pointer =
    let int_or_ptr_structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~attribute_values:
          [DAH.create_byte_size_exn ~byte_size:value_size; DAH.create_name name]
        ~tag:Dwarf_tag.Structure_type ()
    in
    Proto_die.create ~parent:(Some int_or_ptr_structure) ~attribute_values:[]
      ~tag:Dwarf_tag.Variant_part ()
  in
  let enum_immediate_or_pointer =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_name ("Enum ptr/immediate case " ^ name) ]
        ()
    in
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
    Proto_die.add_or_replace_attribute_value variant_part_immediate_or_pointer
      (DAH.create_discr ~proto_die_reference:(Proto_die.reference member_die))
  in
  let _enum_simple_constructor =
    let enum_die =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Enumeration_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_name
              (name ^ " simple constructor enum "
              ^ String.concat "," simple_constructors) ]
        ()
    in
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
    Proto_die.create_ignore ~parent:(Some variant_immediate_case)
      ~tag:Dwarf_tag.Member
      ~attribute_values:
        [ DAH.create_type ~proto_die:enum_die;
          DAH.create_bit_size (Int8.of_int_exn ((value_size * 8) - 1));
          DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
          DAH.create_data_bit_offset ~bit_offset:(Int8.of_int_exn 1) ]
      ()
  in
  let _variant_complex_constructors =
    let ptr_case_structure =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:value_size;
            DAH.create_ocaml_offset_record_from_pointer
              ~value:(Int64.of_int (-value_size));
            DAH.create_name
              ("variant_part " ^ name ^ " "
              ^ String.concat "," complex_constructors_names) ]
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
      let variant_pointer =
        Proto_die.create ~parent:(Some variant_part_immediate_or_pointer)
          ~tag:Dwarf_tag.Variant
          ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int 0)]
          ()
      in
      Proto_die.create_ignore ~parent:(Some variant_pointer)
        ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_type ~proto_die:ptr_case_pointer_to_structure;
            DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0)
          ]
        ()
    in
    let variant_part_pointer =
      Proto_die.create ~parent:(Some ptr_case_structure) ~attribute_values:[]
        ~tag:Dwarf_tag.Variant_part ()
    in
    let _enum_complex_constructor =
      let enum_die =
        Proto_die.create ~parent:(Some parent_proto_die)
          ~tag:Dwarf_tag.Enumeration_type
          ~attribute_values:
            [ DAH.create_byte_size_exn ~byte_size:1;
              DAH.create_name
                (name ^ " " ^ String.concat "," complex_constructors_names) ]
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
      (fun i (_, fields) ->
        let subvariant =
          Proto_die.create ~parent:(Some variant_part_pointer)
            ~tag:Dwarf_tag.Variant
            ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
            ()
        in
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

let create_immediate_or_block ~reference ~parent_proto_die ~name ~immediate_type
    ~pointer_type =
  let value_size = Arch.size_addr in
  let int_or_ptr_structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~attribute_values:
        [DAH.create_byte_size_exn ~byte_size:value_size; DAH.create_name name]
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
    ~name constructors =
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
  create_immediate_or_block ~reference ~name ~parent_proto_die
    ~immediate_type:simple_constructor_enum_die
    ~pointer_type:ptr_case_pointer_to_structure

let create_exception_die ~reference ~fallback_value_die ~parent_proto_die ~name
    =
  let exn_structure =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_name name ]
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

let create_tuple_die ~reference ~parent_proto_die ~name ~fields =
  let structure_type =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:(List.length fields * 8);
          DAH.create_name name ]
      ()
  in
  List.iteri
    (fun i field_die ->
      let member_attributes =
        [ DAH.create_type_from_reference ~proto_die_reference:field_die;
          DAH.create_data_member_location_offset
            ~byte_offset:(Int64.of_int (8 * i)) ]
      in
      Proto_die.create_ignore ~parent:(Some structure_type)
        ~tag:Dwarf_tag.Member ~attribute_values:member_attributes ())
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

let create_simd_vec_split_base_layout_die ~reference ~parent_proto_die ~name
    ~(split : S.Predef.simd_vec_split option) =
  let maybe_name = List.map DAH.create_name (Option.to_list name) in
  match split with
  | None ->
    Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Base_type
      ~attribute_values:
        ([ DAH.create_encoding ~encoding:Encoding_attribute.unsigned;
           DAH.create_byte_size_exn ~byte_size:16 ]
        @ maybe_name)
      ()
  | Some vec_split ->
    let structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:([DAH.create_byte_size_exn ~byte_size:16] @ maybe_name)
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
    (sort : base_layout) ~name ~parent_proto_die ~fallback_value_die =
  let byte_size = base_layout_to_byte_size sort in
  match sort with
  | Value ->
    create_typedef_die ~reference ~parent_proto_die ~name
      ~child_die:fallback_value_die
  | Float32 | Float64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.float
  | Void | Bits8 | Bits16 | Bits32 | Bits64 | Word ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ~name ~byte_size
      ~encoding:Encoding_attribute.signed
  | Vec128 | Vec256 | Vec512 ->
    create_simd_vec_split_base_layout_die ~reference ~parent_proto_die
      ~name:(Some name) ~split:simd_vec_split

module Shape_with_layout = struct
  include Identifiable.Make (struct
    type nonrec t = Layout.t S.ts

    let compare = Stdlib.compare
    (* CR sspies: Fix compare and equals on this type. Move the module to type
       shape once it is more cleaned up. *)

    let print = S.print_type_shape

    let hash = Hashtbl.hash

    let equal (x : t) y = x = y

    let output _oc _t = Misc.fatal_error "unimplemented"
  end)
end

module Cache = Shape_with_layout.Tbl

let cache = Cache.create 16

let rec type_shape_to_dwarf_die (type_shape : Layout.t S.ts) ~parent_proto_die
    ~fallback_value_die =
  match Cache.find_opt cache type_shape with
  | Some reference -> reference
  | None ->
    let reference = Proto_die.create_reference () in
    (* We add the combination of shape and layout early in case of recursive
       types, which can then look up their reference, before it is fully
       defined. That way [type myintlist = MyNil | MyCons of int * myintlist]
       will work correctly (as opposed to diverging). *)
    Cache.add cache type_shape reference;
    let type_name = Type_shape.type_name type_shape in
    let layout_name =
      Format.asprintf "%a" Layout.format (S.shape_layout type_shape)
    in
    let name = type_name ^ " @ " ^ layout_name in
    (match type_shape with
    | Ts_other type_layout | Ts_var (_, type_layout) -> (
      match type_layout with
      | Base b ->
        create_base_layout_type ~reference b ~name ~parent_proto_die
          ~fallback_value_die
      | Product _ ->
        Misc.fatal_errorf
          "only base layouts supported, but found unboxed product layout %s"
          layout_name)
    | Ts_unboxed_tuple _ ->
      Misc.fatal_errorf "unboxed tuples cannot have base layout %s" layout_name
    | Ts_tuple fields ->
      type_shape_to_dwarf_die_tuple ~reference ~parent_proto_die
        ~fallback_value_die ~name fields
    | Ts_predef (predef, args) ->
      type_shape_to_dwarf_die_predef ~reference ~name ~parent_proto_die
        ~fallback_value_die predef args
    | Ts_constr ((type_uid, type_path, type_layout), shapes) -> (
      match type_layout with
      | Base b ->
        type_shape_to_dwarf_die_type_constructor ~reference ~name
          ~parent_proto_die ~fallback_value_die ~type_uid type_path b shapes
      | Product _ ->
        Misc.fatal_errorf
          "only base layouts supported, but found product layout %s" layout_name
      )
    | Ts_variant fields ->
      type_shape_to_dwarf_die_poly_variant ~reference ~name ~parent_proto_die
        ~fallback_value_die ~constructors:fields
    | Ts_arrow (arg, ret) ->
      type_shape_to_dwarf_die_arrow ~reference ~name ~parent_proto_die
        ~fallback_value_die arg ret);
    reference

and type_shape_to_dwarf_die_tuple ~name ~reference ~parent_proto_die
    ~fallback_value_die fields =
  let fields =
    List.map
      (type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die)
      fields
  in
  create_tuple_die ~reference ~parent_proto_die ~name ~fields

and type_shape_to_dwarf_die_predef ~name ~reference ~parent_proto_die
    ~fallback_value_die (predef : S.Predef.t) args =
  match predef, args with
  | Array, [element_type_shape] ->
    let element_type_shape =
      S.shape_with_layout ~layout:(Base Value) element_type_shape
    in
    (* CR sspies: Check whether the elements of an array are always values and,
       if not, where that information is maintained.

       mshinwell: we need to handle unboxed arrays. See Cmm_helpers, but let's
       wait until after we change the representation of these not to use custom
       blocks. *)
    let child_die =
      type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
        element_type_shape
    in
    create_array_die ~reference ~parent_proto_die ~child_die ~name
  | Array, args ->
    Misc.fatal_errorf
      "[Array] shapes must be applied to exactly one type shape (found %d)"
      (List.length args)
  | Char, _ -> create_char_die ~reference ~parent_proto_die ~name
  | Unboxed b, _ ->
    let type_layout = S.Predef.unboxed_type_to_layout b in
    create_base_layout_type
      ~simd_vec_split:(unboxed_base_type_to_simd_vec_split b)
      ~reference type_layout ~name ~parent_proto_die ~fallback_value_die
  | Simd s, _ ->
    (* We represent these vectors as pointers of the form [struct {...} *],
       because their runtime representations are blocks with tag [Abstract_tag]
       (see [Cmm_helpers]). *)
    let base_ref = Proto_die.create_reference () in
    create_simd_vec_split_base_layout_die ~split:(Some s) ~reference:base_ref
      ~name:None ~parent_proto_die;
    Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Reference_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
          DAH.create_type_from_reference ~proto_die_reference:base_ref;
          DAH.create_name name ]
      ()
  | Exception, _ ->
    create_exception_die ~reference ~fallback_value_die ~parent_proto_die ~name
  | ( ( Bytes | Extension_constructor | Float | Float32 | Floatarray | Int
      | Int32 | Int64 | Lazy_t | Nativeint | String ),
      _ ) ->
    create_base_layout_type ~reference Value ~name ~parent_proto_die
      ~fallback_value_die

and type_shape_to_dwarf_die_type_constructor ~reference ~name ~parent_proto_die
    ~fallback_value_die ~type_uid _type_path (type_layout : base_layout) shapes
    =
  match
    (* CR sspies: Somewhat subtly, this case currently also handles [unit],
       [bool], [option], and [list], because they are not treated as predefined
       types and do have declarations. *)
    Type_shape.find_in_type_decls type_uid
  with
  | None ->
    create_base_layout_type ~reference type_layout ~name ~parent_proto_die
      ~fallback_value_die
  | Some type_decl_shape -> (
    let type_decl_shape = S.replace_tvar type_decl_shape shapes in
    match type_decl_shape.definition with
    | Tds_other ->
      create_base_layout_type ~reference type_layout ~name ~parent_proto_die
        ~fallback_value_die
    | Tds_alias alias_shape ->
      let alias_shape =
        S.shape_with_layout ~layout:(Base type_layout) alias_shape
      in
      let alias_die =
        type_shape_to_dwarf_die alias_shape ~parent_proto_die
          ~fallback_value_die
      in
      create_typedef_die ~reference ~parent_proto_die ~child_die:alias_die ~name
    | Tds_record { fields; kind = Record_boxed | Record_floats } ->
      let fields =
        List.map
          (fun (name, type_shape, type_layout) ->
            let type_shape' =
              S.shape_with_layout ~layout:type_layout type_shape
            in
            ( name,
              Arch.size_addr,
              (* All fields here are machine word width *)
              type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
                type_shape' ))
          fields
      in
      create_record_die ~reference ~parent_proto_die ~name ~fields
    | Tds_record { fields = _; kind = Record_unboxed_product } ->
      Misc.fatal_error
        "Unboxed records should not reach this stage. They are deconstructed \
         by unarization in earlier stages of the compiler."
    | Tds_record
        { fields = [(field_name, sh, Base base_layout)]; kind = Record_unboxed }
      ->
      let field_shape = S.shape_with_layout ~layout:(Base base_layout) sh in
      let field_die =
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
          field_shape
      in
      let field_size = base_layout_to_byte_size base_layout in
      create_attribute_unboxed_record_die ~reference ~parent_proto_die ~name
        ~field_die ~field_name ~field_size
      (* The two cases below are filtered out by the flattening of shapes in
         [flatten_shape]. *)
    | Tds_record { fields = [] | _ :: _ :: _; kind = Record_unboxed } ->
      assert false
    | Tds_record { fields = [(_, _, Product _)]; kind = Record_unboxed } ->
      assert false
    | Tds_record { fields; kind = Record_mixed mixed_block_shapes } ->
      let fields = List.map (fun (name, sh, ly) -> Some name, sh, ly) fields in
      let fields = flatten_fields_in_mixed_record ~mixed_block_shapes fields in
      let fields =
        List.map
          (fun (name, type_shape, base_layout) ->
            let type_shape' =
              S.shape_with_layout ~layout:(Base base_layout) type_shape
            in
            match name with
            | Some name ->
              ( name,
                base_layout_to_byte_size_in_mixed_block base_layout,
                type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
                  type_shape' )
            | _ -> assert false)
          fields
      in
      create_record_die ~reference ~parent_proto_die ~name ~fields
    | Tds_variant { simple_constructors; complex_constructors } -> (
      match complex_constructors with
      | [] ->
        create_simple_variant_die ~reference ~parent_proto_die ~name
          ~simple_constructors
      | _ :: _ ->
        (* We flatten the fields of the constructors first *)
        let complex_constructors =
          List.map
            (fun { S.name; kind = mixed_block_shapes; args } ->
              ( name,
                flatten_fields_in_mixed_record ~mixed_block_shapes
                  (List.map
                     (fun { S.field_name = name; field_value = sh, ly } ->
                       name, sh, ly)
                     args) ))
            complex_constructors
        in
        let complex_constructors =
          List.map
            (fun (constr_name, fields) ->
              ( constr_name,
                List.map
                  (fun (field_name, sh, ly) ->
                    let sh = S.shape_with_layout ~layout:(Layout.Base ly) sh in
                    ( field_name,
                      type_shape_to_dwarf_die ~parent_proto_die
                        ~fallback_value_die sh,
                      ly ))
                  fields ))
            complex_constructors
        in
        create_complex_variant_die ~reference ~parent_proto_die ~name
          ~simple_constructors ~complex_constructors)
    | Tds_variant_unboxed
        { name = constr_name; arg_name; arg_shape; arg_layout } ->
      let arg_shape = S.shape_with_layout ~layout:arg_layout arg_shape in
      let arg_die =
        type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die arg_shape
      in
      create_attribute_unboxed_variant_die ~reference ~parent_proto_die ~name
        ~constr_name ~arg_name ~arg_layout ~arg_die)

and type_shape_to_dwarf_die_arrow ~reference ~name ~parent_proto_die
    ~fallback_value_die _arg _ret =
  (* There is no need to inspect the argument and return value. *)
  create_typedef_die ~reference ~parent_proto_die ~name
    ~child_die:fallback_value_die

and type_shape_to_dwarf_die_poly_variant ~reference ~parent_proto_die
    ~fallback_value_die ~name ~constructors =
  let constructors_with_references =
    S.poly_variant_constructors_map
      (type_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die)
      constructors
  in
  create_type_shape_to_dwarf_die_poly_variant ~reference ~parent_proto_die ~name
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
   type is known, we produce [Known type_shape] for the fields. *)

type shape_or_unknown =
  | Known of Layout.t S.ts
  | Unknown of base_layout

let rec flatten_shape (type_shape : Layout.t S.ts) =
  let unknown_base_layouts layout =
    let base_sorts = flatten_to_base_sorts layout in
    List.map (fun base_sort -> Unknown base_sort) base_sorts
  in
  match type_shape with
  | Ts_var (_, Base _) -> [Known type_shape]
  | Ts_var (_, (Product _ as type_layout)) -> unknown_base_layouts type_layout
  | Ts_tuple _ ->
    [Known type_shape]
    (* tuples are word-width, as they are pointers to blocks *)
  | Ts_unboxed_tuple shapes -> List.concat_map flatten_shape shapes
  | Ts_predef _ -> [Known type_shape]
  | Ts_arrow _ -> [Known type_shape]
  | Ts_variant _ -> [Known type_shape]
  | Ts_other layout ->
    let base_layouts = flatten_to_base_sorts layout in
    List.map (fun layout -> Unknown layout) base_layouts
  | Ts_constr ((type_uid, _type_path, layout), shapes) -> (
    match Type_shape.find_in_type_decls type_uid with
    | None -> unknown_base_layouts layout
    | Some { definition = Tds_other; _ } -> unknown_base_layouts layout
    | Some type_decl_shape -> (
      let type_decl_shape = S.replace_tvar type_decl_shape shapes in
      match type_decl_shape.definition with
      | Tds_other ->
        unknown_base_layouts layout (* Cannot break up unknown type. *)
      | Tds_alias alias_shape ->
        let alias_shape = S.shape_with_layout ~layout alias_shape in
        flatten_shape alias_shape
        (* At first glance, this recursion could potentially diverge, for direct
           cycles between type aliases and the defintion of the type. However,
           it seems the compiler disallows direct cycles such as [type t = t]
           and the like. If this ever causes trouble or the behvior of the
           compiler changes with respect to recursive types, we can add a bound
           on the maximal recursion depth. *)
      | Tds_record
          { fields = _; kind = Record_boxed | Record_mixed _ | Record_floats }
        -> (
        match layout with
        | Base Value -> [Known type_shape]
        | _ -> Misc.fatal_error "record must have value layout")
      | Tds_record { fields = [(_, sh, ly)]; kind = Record_unboxed }
        when Layout.equal ly layout -> (
        match layout with
        | Product _ -> flatten_shape (S.shape_with_layout ~layout sh)
        (* for unboxed products of the form [{ field: ty } [@@unboxed]] where
           [ty] is of product sort, we simply look through the unboxed product.
           Otherwise, we will create an additional DWARF entry for it. *)
        | Base _ -> [Known type_shape])
      | Tds_record { fields = [_]; kind = Record_unboxed } ->
        Misc.fatal_error "unboxed record at different layout from its field"
      | Tds_record
          { fields = ([] | _ :: _ :: _) as fields; kind = Record_unboxed } ->
        Misc.fatal_errorf "unboxed record must have exactly one field, found %a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             Format.pp_print_string)
          (List.map (fun (name, _, _) -> name) fields)
      | Tds_record { fields; kind = Record_unboxed_product } -> (
        match layout with
        | Product prod_shapes when List.length prod_shapes = List.length fields
          ->
          let shapes =
            List.map
              (fun (_, sh, ly) -> S.shape_with_layout ~layout:ly sh)
              fields
          in
          List.concat_map flatten_shape shapes
        | Product _ -> Misc.fatal_error "unboxed record field mismatch"
        | Base _ -> Misc.fatal_error "unboxed record must have product layout")
      | Tds_variant _ -> (
        match layout with
        | Base Value -> [Known type_shape]
        | _ -> Misc.fatal_error "variant must have value layout")
      | Tds_variant_unboxed
          { name = _; arg_name = _; arg_layout; arg_shape = _ } ->
        if Layout.equal arg_layout layout
        then [Known type_shape]
        else
          Misc.fatal_error
            "unboxed variant must have same layout as its contents"))

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
  | Some { type_shape; type_layout } -> (
    let type_shape = S.shape_with_layout ~layout:type_layout type_shape in
    let type_shape =
      match unboxed_projection with
      | None -> Known type_shape
      | Some i ->
        let flattened = flatten_shape type_shape in
        let flattened_length = List.length flattened in
        if i < 0 || i >= flattened_length
        then
          Misc.fatal_errorf "unboxed projection index %d out of bounds 0...%d" i
            (flattened_length - 1);
        List.nth flattened i
    in
    match type_shape with
    | Known type_shape ->
      type_shape_to_dwarf_die type_shape ~parent_proto_die ~fallback_value_die
    | Unknown base_layout ->
      let reference = Proto_die.create_reference () in
      create_base_layout_type ~reference ~parent_proto_die
        ~name:("unknown @ " ^ Sort.to_string_base base_layout)
        ~fallback_value_die base_layout;
      reference)
