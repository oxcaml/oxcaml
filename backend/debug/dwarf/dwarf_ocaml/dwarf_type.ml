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
[@@@ocaml.warning "+a-40-42"]

open! Dwarf_low
open! Dwarf_high
module Uid = Flambda2_identifiers.Flambda_debug_uid
module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state

module Type_shape_ident = struct
  include Identifiable.Make (struct
    type nonrec t =
      Type_shape.Type_shape.without_layout Type_shape.Type_shape.ts

    let compare = Stdlib.compare
    (* CR sspies: Fix compare and equals on this type. Move the module to type
       shape once it is more cleaned up. *)

    let print = Type_shape.Type_shape.print

    let hash = Hashtbl.hash

    let equal (x : t) y = x = y

    let output = Misc.output_of_print print
  end)
end
(* CR sspies: In subsequent PRs, we should enrich this module to also cache the
   type layout. For now, we completely ignore the type layout. *)

let cache = Type_shape_ident.Tbl.create 16

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
          DAH.create_byte_stride ~bytes:(Numbers.Int8.of_int_exn Arch.size_addr)
        ]
      ()
  in
  Proto_die.create_ignore ~parent:(Some array_die) ~tag:Dwarf_tag.Subrange_type
    ~attribute_values:
      [ (* Thankfully, all that lldb cares about is DW_AT_count. *)
        DAH.create_count_const 0L ]
    ();
  wrap_die_under_a_pointer ~proto_die:array_die ~reference ~parent_proto_die

let create_char_die ~reference ~parent_proto_die =
  (* As a char is an immediate value, we have to ignore the first bit.
     Unfortunately lldb supports bit offsets only on members of structs, so
     instead, we create a hacky enum containing all possible char values. *)
  let enum =
    Proto_die.create ~reference ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Enumeration_type
      ~attribute_values:
        [DAH.create_name "char"; DAH.create_byte_size_exn ~byte_size:8]
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

let create_typedef_die ~reference ~parent_proto_die ~child_die ~name =
  Proto_die.create_ignore ~reference ~parent:(Some parent_proto_die)
    ~tag:Dwarf_tag.Typedef
    ~attribute_values:
      [ DAH.create_name name;
        DAH.create_type_from_reference ~proto_die_reference:child_die ]
    ()

let create_record_die ~reference ~parent_proto_die ~name ~fields =
  let structure =
    Proto_die.create ~parent:(Some parent_proto_die)
      ~tag:Dwarf_tag.Structure_type
      ~attribute_values:
        [ DAH.create_byte_size_exn ~byte_size:(8 * List.length fields);
          DAH.create_name name ]
      ()
  in
  List.iteri
    (fun i (field_name, field_die) ->
      Proto_die.create_ignore ~parent:(Some structure) ~tag:Dwarf_tag.Member
        ~attribute_values:
          [ DAH.create_name field_name;
            DAH.create_type_from_reference ~proto_die_reference:field_die;
            DAH.create_data_member_location_offset
              ~byte_offset:(Int64.of_int (8 * i)) ]
        ())
    fields;
  wrap_die_under_a_pointer ~proto_die:structure ~reference ~parent_proto_die

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

let create_complex_variant_die ~reference ~parent_proto_die ~name
    ~simple_constructors
    ~(complex_constructors :
       Proto_die.reference Type_shape.Type_decl_shape.complex_constructor list)
    =
  let complex_constructors_names =
    List.map
      (fun { Type_shape.Type_decl_shape.name; kind = _; args = _ } -> name)
      complex_constructors
  in
  let variant_part_immediate_or_pointer =
    let int_or_ptr_structure =
      Proto_die.create ~reference ~parent:(Some parent_proto_die)
        ~attribute_values:
          [DAH.create_byte_size_exn ~byte_size:8; DAH.create_name name]
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
          [ DAH.create_byte_size_exn ~byte_size:8;
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
            DAH.create_bit_size (Numbers.Int8.of_int_exn 1);
            DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 0);
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
          [ DAH.create_byte_size_exn ~byte_size:8;
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
          DAH.create_bit_size (Numbers.Int8.of_int_exn 63);
          DAH.create_data_member_location_offset ~byte_offset:(Int64.of_int 0);
          DAH.create_data_bit_offset ~bit_offset:(Numbers.Int8.of_int_exn 1) ]
      ()
  in
  let _variant_complex_constructors =
    let ptr_case_structure =
      Proto_die.create ~parent:(Some parent_proto_die)
        ~tag:Dwarf_tag.Structure_type
        ~attribute_values:
          [ DAH.create_byte_size_exn ~byte_size:8;
            DAH.create_ocaml_offset_record_from_pointer
              ~value:(Int64.of_int (-8));
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
            [ DAH.create_byte_size_exn ~byte_size:8;
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
        (fun i { Type_shape.Type_decl_shape.name; kind = _; args = _ } ->
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
      (fun i { Type_shape.Type_decl_shape.name = _; kind = _; args } ->
        (* CR sspies: In a subsequent PR, we have to take the kind here into
           account. *)
        let subvariant =
          Proto_die.create ~parent:(Some variant_part_pointer)
            ~tag:Dwarf_tag.Variant
            ~attribute_values:[DAH.create_discr_value ~value:(Int64.of_int i)]
            ()
        in
        List.iteri
          (fun i
               { Type_shape.Type_decl_shape.field_name;
                 field_value = field_type
               } ->
            let member_die =
              Proto_die.create ~parent:(Some subvariant) ~tag:Dwarf_tag.Member
                ~attribute_values:
                  [ DAH.create_data_member_location_offset
                      ~byte_offset:(Int64.of_int (8 * (1 + i)));
                    DAH.create_byte_size_exn ~byte_size:8;
                    DAH.create_type_from_reference
                      ~proto_die_reference:field_type ]
                ()
            in
            match field_name with
            | Some name ->
              Proto_die.add_or_replace_attribute_value member_die
                (DAH.create_name name)
            | None -> ())
          args)
      complex_constructors
  in
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

(* CR sspies: The function [type_shape_to_die] currently ignores type layouts
   completely. As such, it will output wrong type dies for variables that use
   non-value layout. This will be fixed in subsequent PRs. The places are marked
   with a comment. *)
let rec type_shape_to_die
    (type_shape : Type_shape.Type_shape.without_layout Type_shape.Type_shape.ts)
    ~parent_proto_die ~fallback_die =
  match Type_shape_ident.Tbl.find_opt cache type_shape with
  | Some reference -> reference
  | None ->
    let reference = Proto_die.create_reference () in
    Type_shape_ident.Tbl.add cache type_shape reference;
    let name = Type_shape.type_name type_shape in
    let successfully_created =
      match type_shape with
      | Ts_other Layout_to_be_determined | Ts_var _ -> false
      | Ts_predef (Array, [element_type_shape]) ->
        let child_die =
          type_shape_to_die element_type_shape ~parent_proto_die ~fallback_die
        in
        create_array_die ~reference ~parent_proto_die ~child_die ~name;
        true
      | Ts_predef (Array, _) -> false
      | Ts_predef (Char, _) ->
        create_char_die ~reference ~parent_proto_die;
        true
      | Ts_predef
          ( ( Bytes | Extension_constructor | Float | Floatarray | Int | Int32
            | Int64 | Lazy_t | Nativeint | String | Float32 | Exception | Simd _
              ),
            _ ) ->
        create_typedef_die ~reference ~parent_proto_die ~name
          ~child_die:fallback_die;
        true
      | Ts_predef (Unboxed _, _) -> false (* CR sspies: Fix layout. *)
      | Ts_constr ((type_uid, _type_path, Layout_to_be_determined), shapes) -> (
        match(* CR sspies: Somewhat subtly, this case currently also handles
                [unit], [bool], [option], and [list], because they are not
                treated as predefined types, but they do have declarations. *)
             [@warning "-4"]
          Type_shape.find_in_type_decls type_uid
        with
        | None | Some { definition = Tds_other; _ } -> false
        | Some type_decl_shape -> (
          let type_decl_shape =
            Type_shape.Type_decl_shape.replace_tvar type_decl_shape shapes
          in
          match type_decl_shape.definition with
          | Tds_other -> false
          | Tds_alias alias_shape ->
            let alias_die =
              type_shape_to_die alias_shape ~parent_proto_die ~fallback_die
            in
            create_typedef_die ~reference ~parent_proto_die ~child_die:alias_die
              ~name;
            true
          | Tds_record { kind = _; fields } ->
            let fields =
              List.map
                (fun (name, type_shape, _) ->
                  (* CR sspies: Fix layout. *)
                  ( name,
                    type_shape_to_die ~parent_proto_die ~fallback_die type_shape
                  ))
                fields
            in
            create_record_die ~reference ~parent_proto_die ~name ~fields;
            true
          | Tds_variant { simple_constructors; complex_constructors } -> (
            match complex_constructors with
            | [] ->
              create_simple_variant_die ~reference ~parent_proto_die ~name
                ~simple_constructors;
              true
            | _ :: _ ->
              let complex_constructors =
                List.map
                  (Type_shape.Type_decl_shape.complex_constructor_map
                     (fun (sh, _) ->
                       (* CR sspies: Fix layout. *)
                       type_shape_to_die ~parent_proto_die ~fallback_die sh))
                  complex_constructors
              in
              create_complex_variant_die ~reference ~parent_proto_die ~name
                ~simple_constructors ~complex_constructors;
              true)
          | Tds_variant_unboxed _ -> false (* CR sspies: Fix layout. *)))
      | Ts_tuple fields ->
        let fields =
          List.map (type_shape_to_die ~parent_proto_die ~fallback_die) fields
        in
        create_tuple_die ~reference ~parent_proto_die ~name ~fields;
        true
      | Ts_variant _ | Ts_unboxed_tuple _ | Ts_arrow _ ->
        false (* CR sspies: Fix layout. *)
    in
    let reference = if successfully_created then reference else fallback_die in
    Type_shape_ident.Tbl.add (* replace *) cache type_shape reference;
    reference

let variable_to_die state (var_uid : Uid.t) ~parent_proto_die =
  let fallback_die = Proto_die.reference (DS.value_type_proto_die state) in
  match var_uid with
  | Uid var_uid -> (
    match Shape.Uid.Tbl.find_opt Type_shape.all_type_shapes var_uid with
    | None -> fallback_die
    | Some { type_shape; type_layout = _ } ->
      (* CR sspies: For now, we ignore the layout here. This has to be fixed in
         future versions. *)
      type_shape_to_die type_shape ~parent_proto_die ~fallback_die)
  | Proj { uid = _; unboxed_field = _ } ->
    (* CR mshinwell: support unboxed product projections in the shape eval
       language *)
    fallback_die
