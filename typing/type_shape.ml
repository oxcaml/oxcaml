module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

module Type_shape = struct
  module Predef = struct
    open Shape.Predef

    let simd_base_type_of_path = function
      | p when Path.same p Predef.path_int8x16 -> Some Int8x16
      | p when Path.same p Predef.path_int16x8 -> Some Int16x8
      | p when Path.same p Predef.path_int32x4 -> Some Int32x4
      | p when Path.same p Predef.path_int64x2 -> Some Int64x2
      | p when Path.same p Predef.path_float32x4 -> Some Float32x4
      | p when Path.same p Predef.path_float64x2 -> Some Float64x2
      | p when Path.same p Predef.path_int8x32 -> Some Int8x32
      | p when Path.same p Predef.path_int16x16 -> Some Int16x16
      | p when Path.same p Predef.path_int32x8 -> Some Int32x8
      | p when Path.same p Predef.path_int64x4 -> Some Int64x4
      | p when Path.same p Predef.path_float32x8 -> Some Float32x8
      | p when Path.same p Predef.path_float64x4 -> Some Float64x4
      | p when Path.same p Predef.path_int8x64 -> Some Int8x64
      | p when Path.same p Predef.path_int16x32 -> Some Int16x32
      | p when Path.same p Predef.path_int32x16 -> Some Int32x16
      | p when Path.same p Predef.path_int64x8 -> Some Int64x8
      | p when Path.same p Predef.path_float32x16 -> Some Float32x16
      | p when Path.same p Predef.path_float64x8 -> Some Float64x8
      | _ -> None

    let simd_vec_split_of_path = function
      | p when Path.same p Predef.path_unboxed_int8x16 -> Some Int8x16
      | p when Path.same p Predef.path_unboxed_int16x8 -> Some Int16x8
      | p when Path.same p Predef.path_unboxed_int32x4 -> Some Int32x4
      | p when Path.same p Predef.path_unboxed_int64x2 -> Some Int64x2
      | p when Path.same p Predef.path_unboxed_float32x4 -> Some Float32x4
      | p when Path.same p Predef.path_unboxed_float64x2 -> Some Float64x2
      | p when Path.same p Predef.path_unboxed_int8x32 -> Some Int8x32
      | p when Path.same p Predef.path_unboxed_int16x16 -> Some Int16x16
      | p when Path.same p Predef.path_unboxed_int32x8 -> Some Int32x8
      | p when Path.same p Predef.path_unboxed_int64x4 -> Some Int64x4
      | p when Path.same p Predef.path_unboxed_float32x8 -> Some Float32x8
      | p when Path.same p Predef.path_unboxed_float64x4 -> Some Float64x4
      | p when Path.same p Predef.path_unboxed_int8x64 -> Some Int8x64
      | p when Path.same p Predef.path_unboxed_int16x32 -> Some Int16x32
      | p when Path.same p Predef.path_unboxed_int32x16 -> Some Int32x16
      | p when Path.same p Predef.path_unboxed_int64x8 -> Some Int64x8
      | p when Path.same p Predef.path_unboxed_float32x16 -> Some Float32x16
      | p when Path.same p Predef.path_unboxed_float64x8 -> Some Float64x8
      | _ -> None

    let unboxed_of_path = function
      | p when Path.same p Predef.path_unboxed_float -> Some Unboxed_float
      | p when Path.same p Predef.path_unboxed_float32 -> Some Unboxed_float32
      | p when Path.same p Predef.path_unboxed_nativeint ->
        Some Unboxed_nativeint
      | p when Path.same p Predef.path_unboxed_int64 -> Some Unboxed_int64
      | p when Path.same p Predef.path_unboxed_int32 -> Some Unboxed_int32
      | p -> Option.map (fun s -> Unboxed_simd s) (simd_vec_split_of_path p)

    let of_path : Path.t -> t option = function
      | p when Path.same p Predef.path_array -> Some Array
      | p when Path.same p Predef.path_bytes -> Some Bytes
      | p when Path.same p Predef.path_char -> Some Char
      | p when Path.same p Predef.path_extension_constructor ->
        Some Extension_constructor
      | p when Path.same p Predef.path_float -> Some Float
      | p when Path.same p Predef.path_float32 -> Some Float32
      | p when Path.same p Predef.path_floatarray -> Some Floatarray
      | p when Path.same p Predef.path_int -> Some Int
      | p when Path.same p Predef.path_int32 -> Some Int32
      | p when Path.same p Predef.path_int64 -> Some Int64
      | p when Path.same p Predef.path_lazy_t -> Some Lazy_t
      | p when Path.same p Predef.path_nativeint -> Some Nativeint
      | p when Path.same p Predef.path_string -> Some String
      | p when Path.same p Predef.path_exn -> Some Exception
      | p -> (
        match simd_base_type_of_path p with
        | Some b -> Some (Simd b)
        | None -> (
          match unboxed_of_path p with
          | Some u -> Some (Unboxed u)
          | None -> None))
  end

  (* Similarly to [value_kind], we track a set of visited types to avoid cycles
     in the lookup and we, additionally, carry a maximal depth for the recursion.
     We allow a deeper bound than [value_kind]. *)
  (* CR sspies: Consider additionally adding a max size for the set of visited types.
     Also consider reverting to the original value kind depth limit (although 2
     seems low). *)
  let rec of_type_expr_go ~visited ~depth (expr : Types.type_expr) uid_of_path =
    let predef_of_path = Predef.of_path in
    let open Shape in
    let[@inline] cannot_proceed () =
      Numbers.Int.Set.mem (Types.get_id expr) visited || depth > 10
    in
    if cannot_proceed ()
    then Ts_other Layout_to_be_determined
    else
      let visited = Numbers.Int.Set.add (Types.get_id expr) visited in
      let depth = depth + 1 in
      let desc = Types.get_desc expr in
      let of_expr_list (exprs : Types.type_expr list) =
        List.map
          (fun expr -> of_type_expr_go ~depth ~visited expr uid_of_path)
          exprs
      in
      match desc with
      | Tconstr (path, constrs, _abbrev_memo) -> (
        match predef_of_path path with
        | Some predef -> Ts_predef (predef, of_expr_list constrs)
        | None -> (
          match uid_of_path path with
          | Some uid ->
            Shape.Ts_constr
              ((uid, path, Layout_to_be_determined), of_expr_list constrs)
          | None -> Ts_other Layout_to_be_determined))
      | Ttuple exprs -> Ts_tuple (of_expr_list (List.map snd exprs))
      | Tvar { name; _ } -> Ts_var (name, Layout_to_be_determined)
      | Tpoly (type_expr, _type_vars) ->
        (* CR sspies: At the moment, we simply ignore the polymorphic variables.
           This code used to only work for [_type_vars = []]. *)
        of_type_expr_go ~depth ~visited type_expr uid_of_path
      | Tunboxed_tuple exprs ->
        Shape.Ts_unboxed_tuple (of_expr_list (List.map snd exprs))
      | Tobject _ | Tnil | Tfield _ ->
        Shape.Ts_other Layout_to_be_determined
        (* Objects are currently not supported in the debugger. *)
      | Tlink _ | Tsubst _ ->
        Misc.fatal_error "linking and substitution should not reach this stage."
      | Tvariant rd ->
        let row_fields = Types.row_fields rd in
        let row_fields =
          List.concat_map
            (fun (name, desc) ->
              match Types.row_field_repr desc with
              | Types.Rpresent (Some ty) ->
                [ { pv_constr_name = name;
                    pv_constr_args =
                      [of_type_expr_go ~depth ~visited ty uid_of_path]
                  } ]
              | Types.Rpresent None ->
                [{ pv_constr_name = name; pv_constr_args = [] }]
              | Types.Rabsent -> [] (* we filter out absent constructors *)
              | Types.Reither (_, args, _) ->
                [{ pv_constr_name = name; pv_constr_args = of_expr_list args }])
            row_fields
        in
        Shape.Ts_variant row_fields
      | Tarrow (_, arg, ret, _) ->
        Shape.Ts_arrow
          ( of_type_expr_go ~depth ~visited arg uid_of_path,
            of_type_expr_go ~depth ~visited ret uid_of_path )
      | Tunivar { name; _ } -> Ts_var (name, Layout_to_be_determined)
      | Tof_kind _ -> Ts_other Layout_to_be_determined
      | Tpackage _ ->
        Shape.Ts_other
          Layout_to_be_determined (* CR sspies: Support first-class modules. *)

  let of_type_expr (expr : Types.type_expr) uid_of_path =
    of_type_expr_go ~visited:Numbers.Int.Set.empty ~depth:0 expr uid_of_path
end

module Type_decl_shape = struct
  let rec mixed_block_shape_to_layout =
    let open Jkind_types.Sort in
    function
    | Types.Value -> Layout.Base Value
    | Types.Float_boxed ->
      Layout.Base Float64
      (* [Float_boxed] records are unboxed in the variant at runtime,
         contrary to the name.*)
    | Types.Float64 -> Layout.Base Float64
    | Types.Float32 -> Layout.Base Float32
    | Types.Bits8 -> Layout.Base Bits8
    | Types.Bits16 -> Layout.Base Bits16
    | Types.Bits32 -> Layout.Base Bits32
    | Types.Untagged_immediate -> Layout.Base Untagged_immediate
    | Types.Bits64 -> Layout.Base Bits64
    | Types.Vec128 -> Layout.Base Vec128
    | Types.Vec256 -> Layout.Base Vec256
    | Types.Vec512 -> Layout.Base Vec512
    | Types.Word -> Layout.Base Word
    | Types.Void -> Layout.Base Void
    | Types.Product args ->
      Layout.Product
        (Array.to_list (Array.map mixed_block_shape_to_layout args))

  let of_complex_constructor name (cstr_args : Types.constructor_declaration)
      ((constructor_repr, _) : Types.constructor_representation * _) uid_of_path
      =
    let args =
      match cstr_args.cd_args with
      | Cstr_tuple list ->
        List.map
          (fun ({ ca_type = type_expr; ca_sort = type_layout; _ } :
                 Types.constructor_argument) ->
            { Shape.field_name = None;
              field_value =
                Type_shape.of_type_expr type_expr uid_of_path, type_layout
            })
          list
      | Cstr_record list ->
        List.map
          (fun (lbl : Types.label_declaration) ->
            { Shape.field_name = Some (Ident.name lbl.ld_id);
              field_value =
                Type_shape.of_type_expr lbl.ld_type uid_of_path, lbl.ld_sort
            })
          list
    in
    let constructor_repr =
      match constructor_repr with
      | Constructor_mixed shapes ->
        List.iter2
          (fun mix_shape { Shape.field_name = _; field_value = _, ly } ->
            let ly2 = mixed_block_shape_to_layout mix_shape in
            if not (Layout.equal ly ly2)
            then
              Misc.fatal_errorf
                "Type_shape: variant constructor with mismatched layout, has \
                 %a but expected %a"
                Layout.format ly Layout.format ly2)
          (Array.to_list shapes) args;
        Array.map mixed_block_shape_to_layout shapes
      | Constructor_uniform_value ->
        let lys =
          List.map
            (fun { Shape.field_name = _; field_value = _, ly } ->
              if not
                   (Layout.equal ly (Layout.Base Value)
                   || Layout.equal ly (Layout.Base Void))
              then
                Misc.fatal_errorf
                  "Type_shape: variant constructor with mismatched layout, has \
                   %a but expected value or void."
                  Layout.format ly
              else ly)
            args
        in
        Array.of_list lys
    in
    { Shape.name; kind = constructor_repr; args }

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    match cstr_args.cd_args with
    | Cstr_tuple [] -> true
    | Cstr_tuple (_ :: _)
    | Cstr_record _
    (* Records are not allowed to have an empty list of fields.*) ->
      false

  let record_of_labels ~uid_of_path kind labels =
    Shape.Tds_record
      { fields =
          List.map
            (fun (lbl : Types.label_declaration) ->
              ( Ident.name lbl.ld_id,
                Type_shape.of_type_expr lbl.ld_type uid_of_path,
                lbl.ld_sort ))
            labels;
        kind
      }

  let of_type_declaration path (type_declaration : Types.type_declaration)
      uid_of_path =
    let module Types_predef = Predef in
    let open Shape in
    let definition =
      match type_declaration.type_manifest with
      | Some type_expr ->
        Tds_alias (Type_shape.of_type_expr type_expr uid_of_path)
      | None -> (
        match type_declaration.type_kind with
        | Type_variant (cstr_list, Variant_boxed layouts, _unsafe_mode_crossing)
          ->
          let cstrs_with_layouts =
            List.combine cstr_list (Array.to_list layouts)
          in
          let simple_constructors, complex_constructors =
            List.partition_map
              (fun ((cstr, arg_layouts) : Types.constructor_declaration * _) ->
                let name = Ident.name cstr.cd_id in
                match is_empty_constructor_list cstr with
                | true -> Left name
                | false ->
                  Right
                    (of_complex_constructor name cstr arg_layouts uid_of_path))
              cstrs_with_layouts
          in
          Tds_variant { simple_constructors; complex_constructors }
        | Type_variant ([cstr], Variant_unboxed, _unsafe_mode_crossing)
          when not (is_empty_constructor_list cstr) ->
          let name = Ident.name cstr.cd_id in
          let field_name, type_expr, layout =
            match cstr.cd_args with
            | Cstr_tuple [ca] -> None, ca.ca_type, ca.ca_sort
            | Cstr_record [ld] ->
              Some (Ident.name ld.ld_id), ld.ld_type, ld.ld_sort
            | Cstr_tuple _ | Cstr_record _ ->
              Misc.fatal_error "Unboxed variant must have exactly one argument."
          in
          Tds_variant_unboxed
            { name;
              arg_name = field_name;
              arg_layout = layout;
              arg_shape = Type_shape.of_type_expr type_expr uid_of_path
            }
        | Type_variant ([_], Variant_unboxed, _unsafe_mode_crossing) ->
          Misc.fatal_error "Unboxed variant must have constructor arguments."
        | Type_variant (([] | _ :: _ :: _), Variant_unboxed, _) ->
          Misc.fatal_error "Unboxed variant must have exactly one constructor."
        | Type_variant
            (_, (Variant_extensible | Variant_with_null), _unsafe_mode_crossing)
          ->
          Tds_other (* CR sspies: These variants are not yet supported. *)
        | Type_record (lbl_list, record_repr, _unsafe_mode_crossing) -> (
          match record_repr with
          | Record_boxed _ ->
            record_of_labels ~uid_of_path Record_boxed lbl_list
          | Record_mixed fields ->
            record_of_labels ~uid_of_path
              (Record_mixed (Array.map mixed_block_shape_to_layout fields))
              lbl_list
          | Record_unboxed ->
            record_of_labels ~uid_of_path Record_unboxed lbl_list
          | Record_float | Record_ufloat ->
            let lbl_list =
              List.map
                (fun (lbl : Types.label_declaration) ->
                  { lbl with
                    ld_sort = Base Float64;
                    ld_type = Types_predef.type_unboxed_float
                  })
                  (* CR sspies: We are changing the type and the layout here.
                     Consider adding a name for the types of the fields instead
                     of replacing it with [float#]. *)
                lbl_list
            in
            record_of_labels ~uid_of_path Record_floats lbl_list
          | Record_inlined _ ->
            Misc.fatal_error "inlined records not allowed here"
            (* Inline records of this form should not occur as part of type
               declarations. They do not exist for top-level declarations, but
               they do exist temporarily such as inside of a match (e.g., [t] is
               an inline record in [match e with Foo t -> ...]). *))
        | Type_abstract _ -> Tds_other
        | Type_open -> Tds_other
        | Type_record_unboxed_product (lbl_list, _, _) ->
          record_of_labels ~uid_of_path Record_unboxed_product lbl_list)
    in
    let type_params =
      List.map
        (fun type_expr -> Type_shape.of_type_expr type_expr uid_of_path)
        type_declaration.type_params
    in
    { path; definition; type_params }
end

type shape_with_layout =
  { type_shape : Shape.without_layout Shape.ts;
    type_layout : Layout.t
  }

let (all_type_decls : Shape.tds Uid.Tbl.t) = Uid.Tbl.create 16

let (all_type_shapes : shape_with_layout Uid.Tbl.t) = Uid.Tbl.create 16

let add_to_type_decls path (type_decl : Types.type_declaration) uid_of_path =
  let type_decl_shape =
    Type_decl_shape.of_type_declaration path type_decl uid_of_path
  in
  Uid.Tbl.add all_type_decls type_decl.type_uid type_decl_shape

let add_to_type_shapes var_uid type_expr type_layout uid_of_path =
  let type_shape = Type_shape.of_type_expr type_expr uid_of_path in
  Uid.Tbl.add all_type_shapes var_uid { type_shape; type_layout }

let find_in_type_decls (type_uid : Uid.t) =
  Uid.Tbl.find_opt all_type_decls type_uid

let rec estimate_layout_from_type_shape (t : Shape.without_layout Shape.ts) :
    Layout.t option =
  match t with
  | Ts_predef (t, _) -> Some (Shape.Predef.to_layout t)
  | Ts_constr ((uid, _, _), _) ->
    find_in_type_decls uid
    |> Option.map estimate_layout_from_type_decl_shape
    |> Option.join
  | Ts_tuple _ -> Some (Layout.Base Value)
  | Ts_unboxed_tuple fields ->
    let field_layouts = List.map estimate_layout_from_type_shape fields in
    if List.for_all Option.is_some field_layouts
    then Some (Layout.Product (List.map Option.get field_layouts))
    else None
  | Ts_var _ -> None
  | Ts_arrow (_, _) -> Some (Layout.Base Value)
  | Ts_variant _ -> Some (Layout.Base Value)
  | Ts_other _ -> None

and estimate_layout_from_type_decl_shape (tds : Shape.tds) : Layout.t option =
  match tds.definition with
  | Tds_variant_unboxed _ -> Some (Layout.Base Value)
  | Tds_variant _ -> Some (Layout.Base Value)
  | Tds_record _ -> Some (Layout.Base Value)
  | Tds_alias t -> estimate_layout_from_type_shape t
  | Tds_other -> None

let tuple_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd
  | _ :: _ :: _ -> "(" ^ String.concat " * " strings ^ ")"

let unboxed_tuple_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd
  | _ :: _ :: _ -> "(" ^ String.concat " & " strings ^ ")"

let type_arg_list_to_string (strings : string list) =
  match strings with
  | [] -> ""
  | hd :: [] -> hd ^ " "
  | _ :: _ :: _ -> "(" ^ String.concat ", " strings ^ ") "

let rec type_name : 'a. 'a Shape.ts -> _ =
 fun type_shape ->
  match type_shape with
  | Ts_predef (predef, shapes) ->
    type_arg_list_to_string (List.map type_name shapes)
    ^ Shape.Predef.to_string predef
  | Ts_other _ -> "unknown"
  | Ts_tuple shapes -> tuple_to_string (List.map type_name shapes)
  | Ts_unboxed_tuple shapes ->
    unboxed_tuple_to_string (List.map type_name shapes)
  | Ts_var (name, _) -> "'" ^ Option.value name ~default:"?"
  | Ts_arrow (shape1, shape2) ->
    let arg_name = type_name shape1 in
    let ret_name = type_name shape2 in
    arg_name ^ " -> " ^ ret_name
  | Ts_variant fields ->
    let field_constructors =
      List.map
        (fun { Shape.pv_constr_name; pv_constr_args } ->
          let arg_types = List.map (fun sh -> type_name sh) pv_constr_args in
          let arg_type_string = String.concat " ∩ " arg_types in
          (* CR sspies: Currently, our LLDB fork removes ampersands, because
             it's elsewhere used for printing references. Would be great to fix
             this in the future. For now we use an intersection. *)
          let arg_type_string =
            if List.length pv_constr_args = 0
            then ""
            else " of " ^ arg_type_string
          in
          "`" ^ pv_constr_name ^ arg_type_string)
        fields
    in
    (* CR sspies: This type is imprecise. The polymorpic variant could
       potentially have fewer/more constructors. However, there is no need to
       fix this in this PR, because in a subsequent PR, this code will be
       replaced by simply remembering the names of types alongside their shape,
       making the type reconstruction here obsolete. *)
    Format.asprintf "[ %s ]" (String.concat " | " field_constructors)
  | Ts_constr ((type_uid, _type_path, _), shapes) -> (
    match[@warning "-fragile-match"] find_in_type_decls type_uid with
    | None -> "unknown"
    | Some { definition = Tds_other; _ } -> "unknown"
    | Some type_decl_shape ->
      (* We have found type instantiation shapes [shapes] and a typing
         declaration shape [type_decl_shape]. *)
      let type_decl_shape = Shape.replace_tvar type_decl_shape shapes in
      let args = type_arg_list_to_string (List.map type_name shapes) in
      let name = Path.name type_decl_shape.path in
      args ^ name)

let print_table_all_type_decls ppf =
  let entries = Uid.Tbl.to_list all_type_decls in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, v) ->
        ( Format.asprintf "%a" Uid.print k,
          Format.asprintf "%a" Shape.print_type_decl_shape v ))
      entries
  in
  let uids, decls = List.split entries in
  Misc.pp_table ppf ["UID", uids; "Type Declaration", decls]

let print_table_all_type_shapes ppf =
  let entries = Uid.Tbl.to_list all_type_shapes in
  let entries = List.sort (fun (a, _) (b, _) -> Uid.compare a b) entries in
  let entries =
    List.map
      (fun (k, { type_shape; type_layout }) ->
        ( Format.asprintf "%a" Uid.print k,
          ( Format.asprintf "%a" Shape.print_type_shape type_shape,
            Format.asprintf "%a" Layout.format type_layout ) ))
      entries
  in
  let uids, rest = List.split entries in
  let types, sorts = List.split rest in
  Misc.pp_table ppf ["UID", uids; "Type", types; "Sort", sorts]

(* Print debug uid tables when the command line flag [-ddebug-uids] is set. *)
let print_debug_uid_tables ppf =
  Format.fprintf ppf "\n";
  print_table_all_type_decls ppf;
  Format.fprintf ppf "\n";
  print_table_all_type_shapes ppf
