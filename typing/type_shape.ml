module Uid = Shape.Uid
module Layout = Jkind_types.Sort.Const

type base_layout = Jkind_types.Sort.base

module Type_shape = struct
  module Predef = struct
    type simd_vec_split =
      (* 128 bit *)
      | Int8x16
      | Int16x8
      | Int32x4
      | Int64x2
      | Float32x4
      | Float64x2
      (* 256 bit *)
      | Int8x32
      | Int16x16
      | Int32x8
      | Int64x4
      | Float32x8
      | Float64x4
      (* 512 bit *)
      | Int8x64
      | Int16x32
      | Int32x16
      | Int64x8
      | Float32x16
      | Float64x8

    type unboxed =
      | Unboxed_float
      | Unboxed_float32
      | Unboxed_nativeint
      | Unboxed_int64
      | Unboxed_int32
      | Unboxed_simd of simd_vec_split

    type t =
      | Array
      | Bytes
      | Char
      | Extension_constructor
      | Float
      | Float32
      | Floatarray
      | Int
      | Int32
      | Int64
      | Lazy_t
      | Nativeint
      | String
      | Simd of simd_vec_split
      | Exception
      | Unboxed of unboxed

    let simd_vec_split_to_string : simd_vec_split -> string = function
      | Int8x16 -> "int8x16"
      | Int16x8 -> "int16x8"
      | Int32x4 -> "int32x4"
      | Int64x2 -> "int64x2"
      | Float32x4 -> "float32x4"
      | Float64x2 -> "float64x2"
      | Int8x32 -> "int8x32"
      | Int16x16 -> "int16x16"
      | Int32x8 -> "int32x8"
      | Int64x4 -> "int64x4"
      | Float32x8 -> "float32x8"
      | Float64x4 -> "float64x4"
      | Int8x64 -> "int8x64"
      | Int16x32 -> "int16x32"
      | Int32x16 -> "int32x16"
      | Int64x8 -> "int64x8"
      | Float32x16 -> "float32x16"
      | Float64x8 -> "float64x8"

    (* name of the type without the hash *)
    let unboxed_to_string (u : unboxed) =
      match u with
      | Unboxed_float -> "float"
      | Unboxed_float32 -> "float32"
      | Unboxed_nativeint -> "nativeint"
      | Unboxed_int64 -> "int64"
      | Unboxed_int32 -> "int32"
      | Unboxed_simd s -> simd_vec_split_to_string s

    let to_string : t -> string = function
      | Array -> "array"
      | Bytes -> "bytes"
      | Char -> "char"
      | Extension_constructor -> "extension_constructor"
      | Float -> "float"
      | Float32 -> "float32"
      | Floatarray -> "floatarray"
      | Int -> "int"
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Lazy_t -> "lazy_t"
      | Nativeint -> "nativeint"
      | String -> "string"
      | Simd s -> simd_vec_split_to_string s
      | Exception -> "exn"
      | Unboxed u -> unboxed_to_string u ^ "#"

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

    let simd_vec_split_to_layout (b : simd_vec_split) : Jkind_types.Sort.base =
      match b with
      | Int8x16 -> Vec128
      | Int16x8 -> Vec128
      | Int32x4 -> Vec128
      | Int64x2 -> Vec128
      | Float32x4 -> Vec128
      | Float64x2 -> Vec128
      | Int8x32 -> Vec256
      | Int16x16 -> Vec256
      | Int32x8 -> Vec256
      | Int64x4 -> Vec256
      | Float32x8 -> Vec256
      | Float64x4 -> Vec256
      | Int8x64 -> Vec512
      | Int16x32 -> Vec512
      | Int32x16 -> Vec512
      | Int64x8 -> Vec512
      | Float32x16 -> Vec512
      | Float64x8 -> Vec512

    let unboxed_type_to_layout (b : unboxed) : Jkind_types.Sort.base =
      match b with
      | Unboxed_float -> Float64
      | Unboxed_float32 -> Float32
      | Unboxed_nativeint -> Word
      | Unboxed_int64 -> Bits64
      | Unboxed_int32 -> Bits32
      | Unboxed_simd s -> simd_vec_split_to_layout s

    let to_layout : t -> Layout.t = function
      | Array -> Base Value
      | Bytes -> Base Value
      | Char -> Base Value
      | Extension_constructor -> Base Value
      | Float -> Base Value
      | Float32 -> Base Value
      | Floatarray -> Base Value
      | Int -> Base Value
      | Int32 -> Base Value
      | Int64 -> Base Value
      | Lazy_t -> Base Value
      | Nativeint -> Base Value
      | String -> Base Value
      | Simd _ -> Base Value
      | Exception -> Base Value
      | Unboxed u -> Base (unboxed_type_to_layout u)
  end

  type without_layout = Layout_to_be_determined

  type 'a ts =
    | Ts_constr of (Uid.t * Path.t * 'a) * without_layout ts list
    | Ts_tuple of 'a ts list
    | Ts_unboxed_tuple of 'a ts list
    | Ts_var of string option * 'a
    | Ts_predef of Predef.t * without_layout ts list
    | Ts_arrow of without_layout ts * without_layout ts
    | Ts_variant of 'a ts poly_variant_constructors
    | Ts_other of 'a

  and 'a poly_variant_constructors = 'a poly_variant_constructor list

  and 'a poly_variant_constructor =
    { pv_constr_name : string;
      pv_constr_args : 'a list
    }

  let poly_variant_constructors_map f pvs =
    List.map
      (fun pv -> { pv with pv_constr_args = List.map f pv.pv_constr_args })
      pvs

  let rec shape_layout (sh : Layout.t ts) : Layout.t =
    match sh with
    | Ts_constr ((_, _, ly), _) -> ly
    | Ts_tuple _ -> Base Value
    | Ts_unboxed_tuple shapes -> Product (List.map shape_layout shapes)
    | Ts_var (_, ly) -> ly
    | Ts_predef (predef, _) -> Predef.to_layout predef
    | Ts_arrow _ -> Base Value
    | Ts_variant _ -> Base Value
    | Ts_other ly -> ly

  (* Similarly to [value_kind], we track a set of visited types to avoid cycles
     in the lookup and we, additionally, carry a maximal depth for the recursion.
     We allow a deeper bound than [value_kind]. *)
  (* CR sspies: Consider additionally adding a max size for the set of visited types.
     Also consider reverting to the original value kind depth limit (although 2
     seems low). *)
  let rec of_type_expr_go ~visited ~depth (expr : Types.type_expr) uid_of_path =
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
        match Predef.of_path path with
        | Some predef -> Ts_predef (predef, of_expr_list constrs)
        | None -> (
          match uid_of_path path with
          | Some uid ->
            Ts_constr
              ((uid, path, Layout_to_be_determined), of_expr_list constrs)
          | None -> Ts_other Layout_to_be_determined))
      | Ttuple exprs -> Ts_tuple (of_expr_list (List.map snd exprs))
      | Tvar { name; _ } -> Ts_var (name, Layout_to_be_determined)
      | Tpoly (type_expr, _type_vars) ->
        (* CR sspies: At the moment, we simply ignore the polymorphic variables.
           This code used to only work for [_type_vars = []]. *)
        of_type_expr_go ~depth ~visited type_expr uid_of_path
      | Tunboxed_tuple exprs ->
        Ts_unboxed_tuple (of_expr_list (List.map snd exprs))
      | Tobject _ | Tnil | Tfield _ ->
        Ts_other Layout_to_be_determined
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
        Ts_variant row_fields
      | Tarrow (_, arg, ret, _) ->
        Ts_arrow
          ( of_type_expr_go ~depth ~visited arg uid_of_path,
            of_type_expr_go ~depth ~visited ret uid_of_path )
      | Tunivar { name; _ } -> Ts_var (name, Layout_to_be_determined)
      | Tof_kind _ -> Ts_other Layout_to_be_determined
      | Tpackage _ ->
        Ts_other
          Layout_to_be_determined (* CR sspies: Support first-class modules. *)

  let of_type_expr (expr : Types.type_expr) uid_of_path =
    of_type_expr_go ~visited:Numbers.Int.Set.empty ~depth:0 expr uid_of_path

  let rec shape_with_layout ~(layout : Layout.t) (sh : without_layout ts) :
      Layout.t ts =
    match sh, layout with
    | Ts_constr ((uid, path, Layout_to_be_determined), shapes), _ ->
      Ts_constr ((uid, path, layout), shapes)
    | Ts_tuple shapes, Base Value ->
      let shapes_with_layout =
        List.map (shape_with_layout ~layout:(Layout.Base Value)) shapes
      in
      Ts_tuple shapes_with_layout
    | ( Ts_tuple _,
        ( Product _
        | Base
            ( Void | Bits32 | Bits64 | Float64 | Float32 | Word | Vec128
            | Vec256 | Vec512 ) ) ) ->
      Misc.fatal_errorf "tuple shape must have layout value, but has layout %a"
        Layout.format layout
    | Ts_unboxed_tuple shapes, Product lys
      when List.length shapes = List.length lys ->
      let shapes_and_layouts = List.combine shapes lys in
      let shapes_with_layout =
        List.map
          (fun (shape, layout) -> shape_with_layout ~layout shape)
          shapes_and_layouts
      in
      Ts_unboxed_tuple shapes_with_layout
    | Ts_unboxed_tuple shapes, Product lys ->
      Misc.fatal_errorf "unboxed tuple shape has %d shapes, but %d layouts"
        (List.length shapes) (List.length lys)
    | ( Ts_unboxed_tuple _,
        Base
          ( Void | Value | Float32 | Float64 | Word | Bits32 | Bits64 | Vec128
          | Vec256 | Vec512 ) ) ->
      Misc.fatal_errorf
        "unboxed tuple must have unboxed product layout, but has layout %a"
        Layout.format layout
    | Ts_var (name, Layout_to_be_determined), _ -> Ts_var (name, layout)
    | Ts_arrow (arg, ret), Base Value -> Ts_arrow (arg, ret)
    | Ts_arrow _, _ ->
      Misc.fatal_errorf "function type shape must have layout value"
    | Ts_predef (predef, shapes), _
      when Layout.equal (Predef.to_layout predef) layout ->
      Ts_predef (predef, shapes)
    | Ts_predef (predef, _), _ ->
      Misc.fatal_errorf
        "predef %s has layout %a, but is expected to have layout %a"
        (Predef.to_string predef) Layout.format (Predef.to_layout predef)
        Layout.format layout
    | Ts_variant fields, Base Value ->
      let fields =
        poly_variant_constructors_map
          (shape_with_layout ~layout:(Layout.Base Value))
          fields
      in
      Ts_variant fields
    | Ts_variant _, _ ->
      Misc.fatal_errorf "polymorphic variant must have layout value"
    | Ts_other Layout_to_be_determined, _ -> Ts_other layout

  let rec print : type a. Format.formatter -> a ts -> unit =
   (* CR sspies: We should figure out pretty printing for shapes. For now, this
      verbose non-boxed version should be fine. *)
   fun ppf -> function
    | Ts_predef (predef, shapes) ->
      Format.fprintf ppf "%s (%a)" (Predef.to_string predef)
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_constr ((uid, path, _), shapes) ->
      Format.fprintf ppf "Ts_constr uid=%a path=%a (%a)" Uid.print uid
        Path.print path
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_tuple shapes ->
      Format.fprintf ppf "Ts_tuple (%a)"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_unboxed_tuple shapes ->
      Format.fprintf ppf "Ts_unboxed_tuple (%a)"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           print)
        shapes
    | Ts_var (name, _) ->
      Format.fprintf ppf "Ts_var (%a)"
        (fun ppf opt -> Format.pp_print_option Format.pp_print_string ppf opt)
        name
    | Ts_arrow (arg, ret) ->
      Format.fprintf ppf "Ts_arrow (%a, %a)" print arg print ret
    | Ts_variant fields ->
      Format.fprintf ppf "Ts_variant (%a)"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           (fun ppf { pv_constr_name; pv_constr_args } ->
             Format.fprintf ppf "%s (%a)" pv_constr_name
               (Format.pp_print_list
                  ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
                  print)
               pv_constr_args))
        fields
    | Ts_other _ -> Format.fprintf ppf "Ts_other"

  (* CR sspies: This is a hacky "solution" to do type variable substitution in
     type expression shapes. In subsequent PRs, this code should be changed to
     use the shape mechanism instead. *)
  let rec replace_tvar t ~(pairs : (without_layout ts * without_layout ts) list)
      =
    match
      List.find_map
        (fun (from, to_) -> if t = from then Some to_ else None)
        pairs
    with
    | Some new_type -> new_type
    | None -> (
      match t with
      | Ts_constr (uid, shape_list) ->
        Ts_constr (uid, List.map (replace_tvar ~pairs) shape_list)
      | Ts_tuple shape_list ->
        Ts_tuple (List.map (replace_tvar ~pairs) shape_list)
      | Ts_unboxed_tuple shape_list ->
        Ts_unboxed_tuple (List.map (replace_tvar ~pairs) shape_list)
      | Ts_var (name, ly) -> Ts_var (name, ly)
      | Ts_predef (predef, shape_list) -> Ts_predef (predef, shape_list)
      | Ts_arrow (arg, ret) ->
        Ts_arrow (replace_tvar ~pairs arg, replace_tvar ~pairs ret)
      | Ts_variant fields ->
        let fields =
          poly_variant_constructors_map (replace_tvar ~pairs) fields
        in
        Ts_variant fields
      | Ts_other ly -> Ts_other ly)
end

module Type_decl_shape = struct
  type tds_desc =
    | Tds_variant of
        { simple_constructors : string list;
          (* CR sspies: Deduplicate these cases once type shapes have reached
              a more stable form. *)
          complex_constructors :
            (Type_shape.without_layout Type_shape.ts * Layout.t)
            complex_constructors
        }
    | Tds_variant_unboxed of
        { name : string;
          arg_name : string option;
          arg_shape : Type_shape.without_layout Type_shape.ts;
          arg_layout : Layout.t
        }
    | Tds_record of
        { fields :
            (string * Type_shape.without_layout Type_shape.ts * Layout.t) list;
          kind : record_kind
        }
    | Tds_alias of Type_shape.without_layout Type_shape.ts
    | Tds_other

  and record_kind =
    | Record_unboxed
    | Record_unboxed_product
    | Record_boxed
    | Record_mixed of mixed_product_shape
    | Record_floats

  and 'a complex_constructors = 'a complex_constructor list

  and 'a complex_constructor =
    { name : string;
      kind : constructor_representation;
      args : 'a complex_constructor_argument list
    }

  and 'a complex_constructor_argument =
    { field_name : string option;
      field_value : 'a
    }

  and constructor_representation = mixed_product_shape

  and mixed_product_shape = Layout.t array

  type tds =
    { path : Path.t;
      definition : tds_desc;
      type_params : Type_shape.without_layout Type_shape.ts list
    }

  let complex_constructor_map f { name; kind; args } =
    let args =
      List.map
        (fun { field_name; field_value } ->
          { field_name; field_value = f field_value })
        args
    in
    { name; kind; args }

  let complex_constructors_map f = List.map (complex_constructor_map f)

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
    | Types.Bits32 -> Layout.Base Bits32
    | Types.Bits64 -> Layout.Base Bits64
    | Types.Vec128 -> Layout.Base Vec128
    | Types.Vec256 -> Layout.Base Vec256
    | Types.Vec512 -> Layout.Base Vec512
    | Types.Word -> Layout.Base Word
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
            { field_name = None;
              field_value =
                Type_shape.of_type_expr type_expr uid_of_path, type_layout
            })
          list
      | Cstr_record list ->
        List.map
          (fun (lbl : Types.label_declaration) ->
            { field_name = Some (Ident.name lbl.ld_id);
              field_value =
                Type_shape.of_type_expr lbl.ld_type uid_of_path, lbl.ld_sort
            })
          list
    in
    let constructor_repr =
      match constructor_repr with
      | Constructor_mixed shapes ->
        List.iter2
          (fun mix_shape { field_name = _; field_value = _, ly } ->
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
            (fun { field_name = _; field_value = _, ly } ->
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
    { name; kind = constructor_repr; args }

  let is_empty_constructor_list (cstr_args : Types.constructor_declaration) =
    match cstr_args.cd_args with
    | Cstr_tuple [] -> true
    | Cstr_tuple (_ :: _)
    | Cstr_record _
    (* Records are not allowed to have an empty list of fields.*) ->
      false

  let record_of_labels ~uid_of_path kind labels =
    Tds_record
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
                    ld_type = Predef.type_unboxed_float
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

  let print_one_entry print_value ppf { field_name; field_value } =
    match field_name with
    | Some name ->
      Format.fprintf ppf "%a=%a" Format.pp_print_string name print_value
        field_value
    | None -> Format.fprintf ppf "%a" print_value field_value

  let print_sep_string sep ppf () = Format.pp_print_string ppf sep

  let print_complex_constructor print_value ppf { name; kind = _; args } =
    Format.fprintf ppf "(%a: %a)" Format.pp_print_string name
      (Format.pp_print_list ~pp_sep:(print_sep_string " * ")
         (print_one_entry print_value))
      args

  let print_only_shape ppf (shape, _) = Type_shape.print ppf shape

  let print_field ppf
      ((name, shape, _) : _ * Type_shape.without_layout Type_shape.ts * _) =
    Format.fprintf ppf "%a: %a" Format.pp_print_string name Type_shape.print
      shape

  let print_record_type = function
    | Record_boxed -> "_boxed"
    | Record_floats -> "_floats"
    | Record_mixed _ -> "_mixed"
    | Record_unboxed -> " [@@unboxed]"
    | Record_unboxed_product -> "_unboxed_product"

  let print_tds ppf = function
    (* CR sspies: We should figure out pretty printing for shapes. For now, this
       verbose non-boxed version should be fine. *)
    | Tds_variant { simple_constructors; complex_constructors } ->
      Format.fprintf ppf
        "Tds_variant simple_constructors=%a complex_constructors=%a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        simple_constructors
        (Format.pp_print_list ~pp_sep:(print_sep_string " | ")
           (print_complex_constructor print_only_shape))
        complex_constructors
    | Tds_record { fields; kind } ->
      Format.fprintf ppf "Tds_record%s { %a }" (print_record_type kind)
        (Format.pp_print_list ~pp_sep:(print_sep_string "; ") print_field)
        fields
    | Tds_variant_unboxed { name; arg_name; arg_shape; arg_layout } ->
      Format.fprintf ppf
        "Tds_variant_unboxed name=%s arg_name=%s arg_shape=%a arg_layout=%a"
        name
        (Option.value ~default:"None" arg_name)
        Type_shape.print arg_shape Layout.format arg_layout
    | Tds_alias type_shape ->
      Format.fprintf ppf "Tds_alias %a" Type_shape.print type_shape
    | Tds_other -> Format.fprintf ppf "Tds_other"

  let print ppf t =
    Format.fprintf ppf "path=%a, definition=(%a)" Path.print t.path print_tds
      t.definition

  (* CR sspies: This is a hacky "solution" to do type variable substitution in
     type declaration shapes. In subsequent PRs, this code should be changed to
     use the shape mechanism instead. *)
  let replace_tvar (t : tds)
      (shapes : Type_shape.without_layout Type_shape.ts list) =
    match List.length t.type_params == List.length shapes with
    | true ->
      let subst = List.combine t.type_params shapes in
      let replace_tvar (sh, ly) = Type_shape.replace_tvar ~pairs:subst sh, ly in
      let ret =
        { type_params = [];
          path = t.path;
          definition =
            (match t.definition with
            | Tds_variant { simple_constructors; complex_constructors } ->
              Tds_variant
                { simple_constructors;
                  complex_constructors =
                    complex_constructors_map replace_tvar complex_constructors
                }
            | Tds_variant_unboxed { name; arg_name; arg_shape; arg_layout } ->
              Tds_variant_unboxed
                { name;
                  arg_name;
                  arg_shape = Type_shape.replace_tvar ~pairs:subst arg_shape;
                  arg_layout
                }
            | Tds_record { fields; kind } ->
              Tds_record
                { fields =
                    List.map
                      (fun (name, sh, ly) ->
                        name, Type_shape.replace_tvar ~pairs:subst sh, ly)
                      fields;
                  kind
                }
            | Tds_alias type_shape ->
              Tds_alias (Type_shape.replace_tvar ~pairs:subst type_shape)
            | Tds_other -> Tds_other)
        }
      in
      ret
    | false -> { type_params = []; path = t.path; definition = Tds_other }
end

type shape_with_layout =
  { type_shape : Type_shape.without_layout Type_shape.ts;
    type_layout : Layout.t
  }

let (all_type_decls : Type_decl_shape.tds Uid.Tbl.t) = Uid.Tbl.create 16

let (all_type_shapes : shape_with_layout Uid.Tbl.t) = Uid.Tbl.create 16

let add_to_type_decls path (type_decl : Types.type_declaration) uid_of_path =
  let type_decl_shape =
    Type_decl_shape.of_type_declaration path type_decl uid_of_path
  in
  Uid.Tbl.add all_type_decls type_decl.type_uid type_decl_shape

let add_to_type_shapes var_uid type_expr type_layout uid_of_path =
  let type_shape = Type_shape.of_type_expr type_expr uid_of_path in
  Uid.Tbl.add all_type_shapes var_uid { type_shape; type_layout }

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

let find_in_type_decls (type_uid : Uid.t) =
  Uid.Tbl.find_opt all_type_decls type_uid

let rec type_name : 'a. 'a Type_shape.ts -> _ =
 fun type_shape ->
  match type_shape with
  | Ts_predef (predef, shapes) ->
    type_arg_list_to_string (List.map type_name shapes)
    ^ Type_shape.Predef.to_string predef
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
        (fun { Type_shape.pv_constr_name; pv_constr_args } ->
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
      let type_decl_shape =
        Type_decl_shape.replace_tvar type_decl_shape shapes
      in
      let args = type_arg_list_to_string (List.map type_name shapes) in
      let name = Path.name type_decl_shape.path in
      args ^ name)
