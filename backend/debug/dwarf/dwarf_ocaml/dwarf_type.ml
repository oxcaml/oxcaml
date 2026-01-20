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

[@@@warning "+26+27+32"]

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
module MB = Misc.Maybe_bounded
module RS = Runtime_shape
module RL = Runtime_shape.Runtime_layout

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

(* CR sspies: This is a very hacky way of doing an unboxed variant with just a
   single constructor. DWARF variants expect to have a discriminator. So what we
   do is pick the first bit of the contents of the unboxed variant as the
   discriminator and then we simply ouput the same DWARF information for both
   cases. *)
(* This function deals with variants that are annotated with [@@unboxed]. They
   are only allowed to have a single constructor. *)
let create_attribute_unboxed_variant_die ~reference ~parent_proto_die ?name
    ~constr_name ~arg_name ~arg_layout ~arg_die () =
  let width = RL.size arg_layout in
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
      (string * (string option * Proto_die.reference * RL.t) list) list) =
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
            let member_size = RL.size_in_memory ly in
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

let create_poly_variant_dwarf_die ~reference ~parent_proto_die ?name
    simple_constructors complex_constructors =
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

let unboxed_base_type_to_simd_vec_split (x : RS.unboxed) =
  match x with
  | Unboxed_simd s -> Some s
  | Unboxed_float | Unboxed_float32 | Unboxed_nativeint | Unboxed_int64
  | Unboxed_int32 | Unboxed_int16 | Unboxed_int8 ->
    None

type vec_split_properties =
  { encoding : Encoding_attribute.t;
    count : int;
    size : int
  }

let vec_split_to_properties (vec_split : RS.simd_vec_split) =
  let signed = Encoding_attribute.signed in
  let float = Encoding_attribute.float in
  match vec_split with
  | Int8x16 -> { encoding = signed; count = 16; size = 1 }
  | Int16x8 -> { encoding = signed; count = 8; size = 2 }
  | Int32x4 -> { encoding = signed; count = 4; size = 4 }
  | Int64x2 -> { encoding = signed; count = 2; size = 8 }
  | Float16x8 -> { encoding = float; count = 8; size = 2 }
  | Float32x4 -> { encoding = float; count = 4; size = 4 }
  | Float64x2 -> { encoding = float; count = 2; size = 8 }
  | Int8x32 -> { encoding = signed; count = 32; size = 1 }
  | Int16x16 -> { encoding = signed; count = 16; size = 2 }
  | Int32x8 -> { encoding = signed; count = 8; size = 4 }
  | Int64x4 -> { encoding = signed; count = 4; size = 8 }
  | Float16x16 -> { encoding = float; count = 16; size = 2 }
  | Float32x8 -> { encoding = float; count = 8; size = 4 }
  | Float64x4 -> { encoding = float; count = 4; size = 8 }
  | Int8x64 -> { encoding = signed; count = 64; size = 1 }
  | Int16x32 -> { encoding = signed; count = 32; size = 2 }
  | Int32x16 -> { encoding = signed; count = 16; size = 4 }
  | Int64x8 -> { encoding = signed; count = 8; size = 8 }
  | Float16x32 -> { encoding = float; count = 32; size = 2 }
  | Float32x16 -> { encoding = float; count = 16; size = 4 }
  | Float64x8 -> { encoding = float; count = 8; size = 8 }

let create_simd_vec_split_base_layout_die ~reference ~parent_proto_die ?name
    ~byte_size ~(split : RS.simd_vec_split option) () =
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

let create_runtime_layout_type ?(simd_vec_split = None) ~reference (sort : RL.t)
    ?name ~parent_proto_die ~fallback_value_die () =
  let byte_size = RL.size sort in
  match sort with
  | Value ->
    create_typedef_die ~reference ~parent_proto_die ?name fallback_value_die
  | Float32 | Float64 ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.float
  | Bits8 | Bits16 | Bits32 | Bits64 | Word | Untagged_immediate ->
    create_unboxed_base_layout_die ~reference ~parent_proto_die ?name ~byte_size
      Encoding_attribute.signed
  | Vec128 | Vec256 | Vec512 ->
    create_simd_vec_split_base_layout_die ~reference ~parent_proto_die ?name
      ~byte_size ~split:simd_vec_split ()

let create_packed_struct ~parent_proto_die dies_and_layouts =
  let packed_byte_size =
    List.fold_left
      (fun acc (_, layout) -> acc + RL.size_in_memory layout)
      0 dies_and_layouts
  in
  let die =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:[DAH.create_byte_size_exn ~byte_size:packed_byte_size]
      ()
  in
  let offset = ref 0 in
  List.iter
    (fun (reference, layout) ->
      let component_packed_byte_size = RL.size_in_memory layout in
      Proto_die.create_ignore ~parent:(Some die) ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_type_from_reference ~proto_die_reference:reference;
            DAH.create_byte_size_exn ~byte_size:(RL.size layout);
            DAH.create_data_member_location_offset
              ~byte_offset:(Int64.of_int !offset) ]
        ();
      offset := !offset + component_packed_byte_size)
    dies_and_layouts;
  Proto_die.reference die

let create_boxed_simd_type ?name ~reference ~parent_proto_die split =
  (* We represent these vectors as pointers of the form [struct {...} *]. Their
     runtime representation is non-scannable mixed blocks (see Cmm_helpers). *)
  let base_ref = Proto_die.create_reference () in
  let byte_size = RS.simd_vec_split_to_byte_size split in
  create_simd_vec_split_base_layout_die ~split:(Some split) ~reference:base_ref
    ~byte_size ~parent_proto_die ();
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Reference_type
    ~attribute_values:
      (attribute_list_with_optional_name name
         [ DAH.create_byte_size_exn ~byte_size:Arch.size_addr;
           DAH.create_type_from_reference ~proto_die_reference:base_ref ])
    ()

module Rec_env : sig
  type t

  include Hashtbl.HashedType with type t := t

  val empty : t

  val push : t -> Proto_die.reference -> t

  val get_opt :
    t -> de_bruijn_index:RS.DeBruijn_index.t -> Proto_die.reference option
end = struct
  (* We deduplicate the recursive environment for efficiency. Modifications to
     the environment are rare compared to equality checks and hashing, so we
     wrap each environment value with its precomputed hash. This enables O(1)
     equality checks when used as a key in [Dwarf_die_cache]. *)
  include Hash_consed.Dedup (struct
    type t = Proto_die.reference RS.DeBruijn_env.t

    let initial_size = 256

    let hash = RS.DeBruijn_env.hash Asm_targets.Asm_label.hash

    let equal = RS.DeBruijn_env.equal Asm_targets.Asm_label.equal
  end)

  let empty = create RS.DeBruijn_env.empty

  let push rec_env ref = create (RS.DeBruijn_env.push (value rec_env) ref)

  let get_opt rec_env ~de_bruijn_index =
    RS.DeBruijn_env.get_opt (value rec_env) ~de_bruijn_index
end

module Dwarf_die_cache : sig
  val find : inp:RS.t -> rec_env:Rec_env.t -> Proto_die.reference option

  val add : inp:RS.t -> rec_env:Rec_env.t -> outp:Proto_die.reference -> unit
end = struct
  type t =
    { runtime_shape : RS.t;
      rec_env : Rec_env.t
    }

  module Cache = Hashtbl.Make (struct
    type nonrec t = t

    let equal ({ runtime_shape = s1; rec_env = r1 } : t)
        ({ runtime_shape = s2; rec_env = r2 } : t) =
      RS.equal s1 s2 && Rec_env.equal r1 r2

    let hash { runtime_shape; rec_env } =
      Hashtbl.hash (RS.hash runtime_shape, Rec_env.hash rec_env)
  end)

  let cache = Cache.create 100

  let find ~inp ~rec_env = Cache.find_opt cache { runtime_shape = inp; rec_env }

  let add ~inp ~rec_env ~outp =
    Cache.add cache { runtime_shape = inp; rec_env } outp
end

(* CR sspies: We have to be careful here, because LLDB currently disambiguates
   type dies based on the type name; however, we can easily have types with the
   same name and different definitions. The function
   [runtime_shape_to_dwarf_die] only works for types without names, and types
   with names are handled in [runtime_shape_to_dwarf_die_with_aliased_name]
   below. *)
let rec runtime_shape_to_dwarf_die (t : RS.t) ~parent_proto_die
    ~fallback_value_die ~rec_env =
  match Dwarf_die_cache.find ~inp:t ~rec_env with
  | Some reference -> reference
  | None ->
    let reference = Proto_die.create_reference () in
    Dwarf_die_cache.add ~inp:t ~rec_env ~outp:reference;
    let name = None in
    (* Instead of omitting the name argument below, we fix it to be [None] here
       such that it is easier to change this code if in the future we want to
       change how the names of types are handled. *)
    runtime_shape_to_dwarf_die_memo t ?name ~reference ~parent_proto_die
      ~fallback_value_die ~rec_env;
    reference

and runtime_shape_to_dwarf_die_memo ~reference ?name (t : RS.t)
    ~parent_proto_die ~fallback_value_die ~rec_env : unit =
  let err ~fallback f =
    if !Clflags.dwarf_pedantic
    then f Misc.fatal_errorf
    else
      create_runtime_layout_type ~reference fallback ?name ~parent_proto_die
        ~fallback_value_die ()
  in
  let die sh =
    runtime_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die ~rec_env sh
  in
  let die_with_extended_env sh new_ref =
    runtime_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die
      ~rec_env:(Rec_env.push rec_env new_ref)
      sh
  in
  match t.desc with
  | Unknown type_layout ->
    create_runtime_layout_type ~reference type_layout ?name ~parent_proto_die
      ~fallback_value_die ()
  | Predef p ->
    predef_to_dwarf_die ~reference ?name p ~parent_proto_die ~fallback_value_die
      ~rec_env
  | Tuple { args; kind = Tuple_boxed } ->
    (* CR sspies: In the future, tuples have to be handled like mixed
       records. *)
    let fields = List.map die args in
    create_tuple_die ~reference ~parent_proto_die ?name fields
  | Func ->
    create_typedef_die ~reference ~parent_proto_die ?name fallback_value_die
  | Record { fields; kind = Record_mixed } ->
    let fields =
      List.map
        (fun ({ field_type; label } : string RS.mixed_block_field) ->
          let element = RS.runtime_layout field_type in
          (* CR sspies: Investigate in the future whether we should use the
             actual sizes here/in addition to produce more accurate DWARF. *)
          label, RL.size_in_memory element, die field_type)
        fields
    in
    create_record_die ~reference ~parent_proto_die ?name ~fields ()
  | Record
      { fields = [{ field_type; label }];
        kind = Record_attribute_unboxed layout
      } ->
    let field_name = label in
    let field_die = die field_type in
    let field_size = RL.size layout in
    create_attribute_unboxed_record_die ~reference ~parent_proto_die ?name
      ~field_name ~field_size ~field_die ()
  | Record
      { fields = ([] | _ :: _ :: _) as fields;
        kind = Record_attribute_unboxed layout
      } ->
    (* This case should be unreachable due to the Complex_shape smart
       constructors that only allow creating @@unboxed records with one
       field. *)
    err ~fallback:layout (fun f ->
        let fields = List.map (fun { RS.field_type; _ } -> field_type) fields in
        f "[@@unboxed] record should have exactly one field, but has: %a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space RS.print)
          fields)
  | Variant { constructors; kind = Variant_polymorphic } ->
    let simple_constructors, complex_constructors =
      List.partition_map
        (fun (constr : RS.constructor) ->
          let constr_name = RS.constructor_name constr in
          let args = RS.constructor_args constr in
          match args with
          | [] -> Left constr_name
          | _ :: _ ->
            let arg_dies =
              List.map (fun { RS.field_type; _ } -> die field_type) args
            in
            Right (constr_name, arg_dies))
        constructors
    in
    create_poly_variant_dwarf_die ~reference ~parent_proto_die ?name
      simple_constructors complex_constructors
  | Variant { constructors; kind = Variant_boxed } -> (
    let simple_constructors, complex_constructors =
      List.partition_map
        (fun (constr : RS.constructor) ->
          let constr_name = RS.constructor_name constr in
          let args = RS.constructor_args constr in
          match args with
          | [] -> Left constr_name
          | _ :: _ -> Right (constr_name, args))
        constructors
    in
    match complex_constructors with
    | [] ->
      create_simple_variant_die ~reference ~parent_proto_die ?name
        simple_constructors
    | _ :: _ ->
      let complex_constructors =
        List.map
          (fun (constr_name, args) ->
            let args =
              List.map
                (fun { RS.label; field_type } ->
                  label, die field_type, RS.runtime_layout field_type)
                args
            in
            constr_name, args)
          complex_constructors
      in
      create_complex_variant_die ~reference ~parent_proto_die ?name
        simple_constructors complex_constructors)
  | Variant { constructors = [constr]; kind = Variant_attribute_unboxed layout }
    -> (
    let constr_name = RS.constructor_name constr in
    let args = RS.constructor_args constr in
    match args with
    | [{ RS.label; field_type; _ }] ->
      let arg_die = die field_type in
      let arg_name = label in
      create_attribute_unboxed_variant_die ~reference ~parent_proto_die ?name
        ~constr_name ~arg_name ~arg_layout:layout ~arg_die ()
    | _ ->
      err ~fallback:layout (fun f ->
          f
            "Variant with [@@unboxed] attribute must have exactly one \
             argument:@ %a"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space RS.print)
            (List.map (fun { RS.field_type; _ } -> field_type) args)))
  | Variant
      { constructors = ([] | _ :: _ :: _) as constructors;
        kind = Variant_attribute_unboxed layout
      } ->
    err ~fallback:layout (fun f ->
        (* This case should be unreachable due to the Complex_shape smart
           constructors that only allow creating @@unboxed variants with one
           constructor. *)
        let constructor_names = List.map RS.constructor_name constructors in
        f
          "Variant with [@@unboxed] attribute must have exactly one \
           constructor:@ %a"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             Format.pp_print_string)
          constructor_names)
  | Rec_var (de_bruijn_index, layout) -> (
    match Rec_env.get_opt rec_env ~de_bruijn_index with
    | Some reference' ->
      create_typedef_die ~reference ~parent_proto_die ?name reference'
    | None ->
      err ~fallback:layout (fun f ->
          f
            "Recursive variable environment lookup failed: rec_env returned \
             None for de Bruijn index %a"
            RS.DeBruijn_index.print de_bruijn_index))
  | Mu sh ->
    (* CR sspies: We are creating two typedefs for recursive types. One should
       be enough. *)
    let reference' = die_with_extended_env sh reference in
    create_typedef_die ~reference ~parent_proto_die ?name reference'

and predef_to_dwarf_die ~reference ?name (t : RS.predef) ~parent_proto_die
    ~fallback_value_die ~rec_env =
  let die sh =
    runtime_shape_to_dwarf_die ~parent_proto_die ~fallback_value_die ~rec_env sh
  in
  match t with
  | Array (Regular s) ->
    let child_die = die s in
    create_array_die ~reference ~parent_proto_die ~child_die ?name ()
  | Array (Packed fields) ->
    let dies = List.map (fun t -> die t, RS.runtime_layout t) fields in
    let packed_die = create_packed_struct ~parent_proto_die dies in
    create_array_die ~reference ~parent_proto_die ~child_die:packed_die ?name ()
  | Char -> create_char_die ~reference ~parent_proto_die ?name ()
  | Unboxed b ->
    let type_layout = RS.runtime_layout_of_unboxed b in
    create_runtime_layout_type
      ~simd_vec_split:(unboxed_base_type_to_simd_vec_split b)
      ~reference type_layout ?name ~parent_proto_die ~fallback_value_die ()
  | Simd s -> create_boxed_simd_type ?name ~reference ~parent_proto_die s
  | Exception ->
    create_exception_die ~reference ~fallback_value_die ~parent_proto_die ?name
      ()
  | Bytes | Extension_constructor | Float | Float32 | Floatarray | Int | Int8
  | Int16 | Int32 | Int64 | Lazy_t _ | Nativeint | String ->
    create_runtime_layout_type ~reference Value ?name ~parent_proto_die
      ~fallback_value_die ()
(* CR sspies: Create a separate block for lazy values. We now have type
   information for them. *)

(** This second cache is for named type shapes. Every type name should be
    associated with at most one DWARF die, so this cache maps type names to type
    shapes and DWARF dies. *)
let name_cache = String.Tbl.create 16

module With_cms_reduce = Shape_reduce.Make (struct
  let fuel () = MB.of_option !Clflags.gdwarf_config_shape_reduce_fuel

  let fuel_for_compilation_units () =
    MB.of_option !Clflags.gdwarf_config_max_cms_files_per_variable
  (* Every variable gets to look up at most N compilation units. *)

  let max_shape_reduce_steps_per_variable () =
    MB.of_option !Clflags.gdwarf_config_max_shape_reduce_steps_per_variable

  let max_compilation_unit_depth () =
    MB.of_option !Clflags.gdwarf_config_shape_reduce_depth

  let projection_rules_for_merlin_enabled = false

  let cms_file_cache = String.Tbl.create 264

  let cms_files_read_counter = ref 0
  (* Track the number of .cms files read during compilation. *)
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
      let max_cms_files =
        MB.of_option !Clflags.gdwarf_config_max_cms_files_per_unit
      in
      if MB.is_out_of_bounds !cms_files_read_counter max_cms_files
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
            incr cms_files_read_counter;
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
   [name_cache] cache. If we come along a type of the same name and runtime
   shape, then we simply use that reference. *)
let find_unused_type_name_or_cached (name : string) (runtime_shape : RS.t) :
    (Proto_die.reference, string) Either.t =
  let rec aux inc : _ Either.t =
    let name_suffix = if inc = 0 then "" else "/" ^ string_of_int inc in
    let name = name ^ name_suffix in
    match String.Tbl.find_opt name_cache name with
    | Some (runtime_shape', reference) ->
      if RS.equal runtime_shape runtime_shape'
      then Left reference
      else aux (inc + 1)
    | None -> Right name
  in
  aux 0

(* We represent all types as DIE entries of the form [typedef ... type_name;]
   and use caching for types that have the same name and shape. For name
   conflicts, we search for the next available suffix-numbered version of the
   name, [type_name/n]. *)
let runtime_shape_to_dwarf_die_with_aliased_name (type_name : string)
    (runtime_shape : RS.t) ~parent_proto_die ~fallback_value_die :
    Proto_die.reference =
  match find_unused_type_name_or_cached type_name runtime_shape with
  | Left reference -> reference
  | Right name ->
    let unnamed_die =
      runtime_shape_to_dwarf_die runtime_shape ~parent_proto_die
        ~fallback_value_die (* note that we do not pass the type name here *)
        ~rec_env:Rec_env.empty
    in
    let reference = Proto_die.create_reference () in
    let runtime_layout = RS.runtime_layout runtime_shape in
    let layout_name = RL.to_string runtime_layout in
    let full_name = name ^ " @ " ^ layout_name in
    String.Tbl.add name_cache name (runtime_shape, reference);
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
  | Some { type_shape; type_name; type_layout } -> (
    let err ~fallback f =
      if !Clflags.dwarf_pedantic then f Misc.fatal_errorf else fallback
    in
    let reduction_diagnostics = D.create type_name type_layout in
    let shape_reduce =
      With_cms_reduce.reduce
        ~diagnostics:(D.shape_reduce_diagnostics reduction_diagnostics)
        Env.empty
    in
    D.record_before_reduction reduction_diagnostics type_shape;
    let type_shape =
      Profile.record "shape_reduce"
        (fun () -> shape_reduce type_shape)
        ~accumulate:true ()
    in
    D.record_after_reduction reduction_diagnostics type_shape;
    let type_shape =
      Profile.record "unfold_and_evaluate"
        (fun () ->
          Type_shape.unfold_and_evaluate
            ~diagnostics:(D.shape_evaluation_diagnostics reduction_diagnostics)
            type_shape)
        ~accumulate:true ()
    in
    D.record_after_evaluation reduction_diagnostics type_shape;
    let complex_shape =
      Complex_shape.type_shape_to_complex_shape type_shape type_layout
    in
    let complex_shape_flattened =
      Complex_shape.flatten_complex_shape complex_shape
    in
    let runtime_shape =
      match complex_shape_flattened, unboxed_projection with
      | [Other runtime_shape], None -> Some runtime_shape
      | ([] | _ :: _ :: _ | [Void]), None ->
        err ~fallback:None (fun f ->
            f "Expected a single runtime shape, but got %a" Complex_shape.print
              complex_shape)
      | _, Some i -> (
        if i < 0 || i >= List.length complex_shape_flattened
        then
          err ~fallback:None (fun f ->
              f
                "Attempted to access field %d of a layout with %d fields, \
                 namely %a"
                i
                (List.length complex_shape_flattened)
                (Format.pp_print_list ~pp_sep:Format.pp_print_space
                   (Runtime_shape.print_or_void RS.print))
                complex_shape_flattened)
        else
          match List.nth complex_shape_flattened i with
          | Other runtime_shape -> Some runtime_shape
          | Void ->
            err ~fallback:None (fun f ->
                f
                  "Unarization projection %d is a void field without a runtime \
                   shape"
                  i))
    in
    match runtime_shape with
    | None ->
      fallback_value_die
      (* CR sspies: Another case where we need a fallback when we are not
         running in pedantic mode. *)
    | Some runtime_shape ->
      let type_name =
        match unboxed_projection with
        | None -> type_name
        | Some i -> type_name ^ "_unboxed" ^ string_of_int i
        (* CR sspies: In case of unboxed projections, we do not have the type
           names of the individual fields available. And obtaining them in
           general is not straightforward, since they could be hidden behind a
           type alias (e.g., [type prod = #{ a: int64#; b: float# }]). What we
           currently do is match the style of unarization variables by appending
           "_unboxed" for the projections to indicate that the type is not the
           same. *)
      in
      D.record_before_dwarf_generation reduction_diagnostics parent_proto_die;
      let reference =
        let reference =
          Profile.record "dwarf_produce_dies"
            (fun () ->
              runtime_shape_to_dwarf_die_with_aliased_name type_name
                runtime_shape ~parent_proto_die ~fallback_value_die)
            ~accumulate:true ()
        in
        if Debugging_the_compiler.enabled ()
        then (
          Format.eprintf "%a has become %a@." Uid.print var_uid
            Asm_targets.Asm_label.print reference;
          Debugging_the_compiler.print ~die:parent_proto_die);
        reference
      in
      D.record_after_dwarf_generation reduction_diagnostics parent_proto_die;
      D.append_diagnostics_to_dwarf_state state reduction_diagnostics;
      reference)
