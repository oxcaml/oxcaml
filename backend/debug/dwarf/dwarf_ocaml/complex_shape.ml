(******************************************************************************
 *                                  OxCaml                                    *
 *                        Simon Spies, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025--2026 Jane Street Group LLC                             *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module RS = Runtime_shape
module S = Shape
module Sort = Jkind_types.Sort
module Layout = Sort.Const

type t =
  { desc : desc;
    layout : Layout.t;
    hash : int
  }

and desc =
  | Runtime of RS.t
  | Void
  | Unboxed_product of
      { kind : unboxed_product_kind;
        components : t list
      }

and unboxed_product_kind =
  | Unboxed_record of string list
  | Unboxed_tuple

let to_layout { layout; _ } = layout

(* Hashing *)
let hash_runtime = 0

let hash_void = 1

let hash_unboxed_product = 2

let hash_unboxed_product_kind : unboxed_product_kind -> int = function
  | Unboxed_record names -> Hashtbl.hash (0, List.map Hashtbl.hash names)
  | Unboxed_tuple -> 1

let hash { hash; _ } = hash

(* Smart constructors *)

let runtime t =
  let desc = Runtime t in
  let layout =
    Layout.Base (RS.Runtime_layout.to_base_layout (RS.runtime_layout t))
  in
  { desc; layout; hash = Hashtbl.hash (hash_runtime, RS.hash t) }

let unboxed_tuple args =
  let desc = Unboxed_product { components = args; kind = Unboxed_tuple } in
  { desc;
    layout = Layout.Product (List.map to_layout args);
    hash =
      Hashtbl.hash
        ( hash_unboxed_product,
          hash_unboxed_product_kind Unboxed_tuple,
          List.map hash args )
  }

let record_unboxed args =
  let field_names, components = List.split args in
  let desc =
    Unboxed_product { components; kind = Unboxed_record field_names }
  in
  { desc;
    layout = Layout.Product (List.map to_layout components);
    hash =
      Hashtbl.hash
        ( hash_unboxed_product,
          hash_unboxed_product_kind (Unboxed_record field_names),
          List.map hash components )
  }

let void =
  let desc = Void in
  { desc; layout = Layout.Base Sort.Void; hash = Hashtbl.hash hash_void }

let rec print fmt { desc } =
  match desc with
  | Runtime s -> RS.print fmt s
  | Void -> Format.fprintf fmt "void"
  | Unboxed_product { components; kind } -> (
    match kind with
    | Unboxed_record field_names ->
      Format.fprintf fmt "#{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (shape, name) ->
             Format.fprintf fmt "%s: %a" name print shape))
        (List.combine components field_names)
    | Unboxed_tuple ->
      Format.fprintf fmt "#(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ")
           print)
        components)

let equal_unboxed_product_kind kind1 kind2 =
  match kind1, kind2 with
  | Unboxed_record names1, Unboxed_record names2 ->
    List.equal String.equal names1 names2
  | Unboxed_tuple, Unboxed_tuple -> true
  | (Unboxed_record _ | Unboxed_tuple), _ -> false

let rec equal { desc = desc1 } { desc = desc2 } =
  match desc1, desc2 with
  | Runtime s1, Runtime s2 -> RS.equal s1 s2
  | Void, Void -> true
  | ( Unboxed_product { kind = kind1; components = comps1 },
      Unboxed_product { kind = kind2; components = comps2 } ) ->
    equal_unboxed_product_kind kind1 kind2 && List.equal equal comps1 comps2
  | (Runtime _ | Void | Unboxed_product _), _ -> false

let runtime_shape (cs : t) =
  match cs.desc with Runtime s -> Some s | Void | Unboxed_product _ -> None

let rec flatten_complex_shape (cs : t) : RS.t RS.Or_void.t list =
  match cs.desc with
  | Void -> [RS.Or_void.Void]
  | Runtime s -> [RS.Or_void.Other s]
  | Unboxed_product { components; kind = _ } ->
    List.concat_map flatten_complex_shape components

(* Shape Translation *)

let translate_simd_vec_split : S.Predef.simd_vec_split -> RS.simd_vec_split =
  function
  | Int8x16 -> Int8x16
  | Int16x8 -> Int16x8
  | Int32x4 -> Int32x4
  | Int64x2 -> Int64x2
  | Float16x8 -> Float16x8
  | Float32x4 -> Float32x4
  | Float64x2 -> Float64x2
  | Int8x32 -> Int8x32
  | Int16x16 -> Int16x16
  | Int32x8 -> Int32x8
  | Int64x4 -> Int64x4
  | Float16x16 -> Float16x16
  | Float32x8 -> Float32x8
  | Float64x4 -> Float64x4
  | Int8x64 -> Int8x64
  | Int16x32 -> Int16x32
  | Int32x16 -> Int32x16
  | Int64x8 -> Int64x8
  | Float16x32 -> Float16x32
  | Float32x16 -> Float32x16
  | Float64x8 -> Float64x8

let translate_unboxed : S.Predef.unboxed -> RS.unboxed = function
  | Unboxed_float -> Unboxed_float
  | Unboxed_float32 -> Unboxed_float32
  | Unboxed_nativeint -> Unboxed_nativeint
  | Unboxed_int64 -> Unboxed_int64
  | Unboxed_int32 -> Unboxed_int32
  | Unboxed_int16 -> Unboxed_int16
  | Unboxed_int8 -> Unboxed_int8
  | Unboxed_simd svs -> Unboxed_simd (translate_simd_vec_split svs)

exception Layout_missing

let err_exn f =
  if !Clflags.dwarf_pedantic then f Misc.fatal_errorf else raise Layout_missing
(* There are various error cases that we want to detect in pedantic mode, but
   where we want to recover gracefully otherwise. *)

let force_runtime_shape_exn cs =
  match runtime_shape cs with Some s -> s | None -> raise Layout_missing

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
    | Float32 -> Float32
    | Bits8 -> Bits8
    | Bits16 -> Bits16
    | Bits32 -> Bits32
    | Bits64 -> Bits64
    | Vec128 -> Vec128
    | Vec256 -> Vec256
    | Vec512 -> Vec512
    | Word -> Word
    | Untagged_immediate -> Untagged_immediate
    | Void -> Product [||])
  | Product lys -> Product (Array.of_list (List.map layout_to_types_layout lys))

let to_runtime_layout (e : _ Mixed_block_shape.Singleton_mixed_block_element.t)
    : RS.Runtime_layout.t =
  match e with
  | Value _ -> Value
  | Float_boxed _ -> Float64 (* unboxed in the actual block *)
  | Float64 -> Float64
  | Float32 -> Float32
  | Bits8 -> Bits8
  | Bits16 -> Bits16
  | Bits32 -> Bits32
  | Bits64 -> Bits64
  | Vec128 -> Vec128
  | Vec256 -> Vec256
  | Vec512 -> Vec512
  | Word -> Word
  | Untagged_immediate -> Untagged_immediate

(* In a mixed block, we want to add names for record fields but not for tuple
   fields. To treat them uniformly during unarization, we accumulate the record
   labels and tuple indices and then let the caller decide whether they want to
   use the resulting access name or not. *)
let projection_component_name (i : int) (field_name : string option) : string =
  match field_name with Some n -> n | None -> string_of_int (i + 1)

(* Raises if we hit a void field unintentionally. *)
let rec lay_out_into_mixed_block_exn
    ~(source_level_fields : (string option * t) list) =
  let layouts =
    List.map (fun (bf_name, bf_type) -> to_layout bf_type) source_level_fields
  in
  let source_level_fields_array = Array.of_list source_level_fields in
  let mixed_block_shapes = List.map layout_to_types_layout layouts in
  let reordering =
    Mixed_block_shape.of_mixed_block_elements
      ~print_locality:(fun _ _ -> ())
      (Lambda.transl_mixed_product_shape (Array.of_list mixed_block_shapes))
  in
  let fields =
    Array.init (Mixed_block_shape.new_block_length reordering) (fun i ->
        let old_path = Mixed_block_shape.new_index_to_old_path reordering i in
        if Int.equal (List.length old_path) 0
        then
          err_exn (fun f ->
              let pp =
                Format.pp_print_list ~pp_sep:Format.pp_print_space print
              in
              let source_level_fields =
                List.map (fun (_, bf_type) -> bf_type) source_level_fields
              in
              f "layout_mixed_block: empty path encountered when laying out %a"
                pp source_level_fields);
        let projected_shape, label_components =
          project_field_given_path_exn source_level_fields_array [] old_path
        in
        (* After projection, we should not have a void field. The new indices
           should only point to non-void fields of the record. *)
        let shape =
          match runtime_shape projected_shape with
          | Some s -> s
          | None ->
            err_exn (fun f ->
                f
                  "Found complex shape %a when projecting runtime shapes from \
                   mixed block."
                  print projected_shape)
        in
        let label = String.concat ".#" label_components in
        RS.mixed_block_field ~field_type:shape ~label)
  in
  Array.to_list fields

and project_field_given_path_exn (fields : (string option * t) array)
    (name_acc : string list) path =
  match path with
  | [] ->
    err_exn (fun f ->
        f "project_field_given_path_exn: argument cannot be empty.")
    (* This case cannot happen recursively. [lay_out_into_mixed_block_exn]
       currently ensures that it is ruled out with a more informative error. *)
  | i :: _ when i < 0 || i >= Array.length fields ->
    err_exn (fun f ->
        f
          "project_field_given_path_exn: invalid path, index %d out of bounds \
           0..<%d. Fields are: %a and accumulated name is %a."
          i (Array.length fields)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (name_opt, cs) ->
               match name_opt with
               | None -> print fmt cs
               | Some name -> Format.fprintf fmt "%s: %a" name print cs))
          (Array.to_list fields)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
             Format.pp_print_string)
          (List.rev name_acc))
  | [i] ->
    let bf_name, bf_type = Array.get fields i in
    bf_type, List.rev (projection_component_name i bf_name :: name_acc)
  | i :: subpath ->
    let bf_name, bf_type = Array.get fields i in
    let inner_fields = flatten_product_layout_exn bf_type in
    let name_acc = projection_component_name i bf_name :: name_acc in
    project_field_given_path_exn (Array.of_list inner_fields) name_acc subpath

and flatten_product_layout_exn (cs : t) =
  match cs.desc with
  | Void ->
    err_exn (fun f -> f "flatten_product_layout_exn: cannot project from Void.")
    (* All runtime shapes are register size and cannot be projected from. *)
  | Runtime t ->
    err_exn (fun f ->
        f
          "flatten_product_layout_exn: found unexpected runtime shape when \
           projecting: %a"
          RS.print t)
  | Unboxed_product { components; kind = Unboxed_record field_names } ->
    let fields = List.combine field_names components in
    List.map (fun (field_name, arg) -> Some field_name, arg) fields
  | Unboxed_product { components; kind = Unboxed_tuple } ->
    List.map (fun arg -> None, arg) components

module Shape_cache = struct
  type key =
    { type_shape : Shape.t;
      type_layout : Layout.t
    }

  (* CR sspies: This caching will need performance improvements along with the
     other caches in the future. *)
  module Cache = Hashtbl.Make (struct
    type t = key

    let equal ({ type_shape = x1; type_layout = y1 } : t)
        ({ type_shape = x2; type_layout = y2 } : t) =
      Shape.equal x1 x2 && Layout.equal y1 y2

    let hash { type_shape; type_layout } =
      Hashtbl.hash (type_shape.hash, type_layout)
    (* CR sspies: Add a hash function to Layout.t *)
  end)

  type nonrec t = t Cache.t

  let create initial_size = Cache.create initial_size

  let find_in_cache cache type_shape type_layout ~rec_env =
    if S.DeBruijn_env.is_empty rec_env
    then Cache.find_opt cache { type_shape; type_layout }
    else None

  let add_to_cache cache type_shape type_layout value ~rec_env =
    (* [rec_env] being empty means that the shape is closed. *)
    if S.DeBruijn_env.is_empty rec_env
    then Cache.add cache { type_shape; type_layout } value
end

(** Lays out the elements in the shape sequentially while erasing all voids.
    This can be used for turning a complex shape into a sequence of runtime
    shapes for arrays. *)
let lay_out_sequentially (cs : t) : RS.t list =
  (* CR sspies: Maybe bring back the names here for records? *)
  flatten_complex_shape cs |> RS.Or_void.erase_void

let rec layout_to_unknown_shape (ly : Layout.t) : t =
  match ly with
  | Product lys ->
    let layouts = List.map layout_to_unknown_shape lys in
    unboxed_tuple layouts
  | Base b -> (
    match RS.Runtime_layout.of_base_layout b with
    | Other runtime_layout -> runtime (RS.unknown runtime_layout)
    | Void -> void)

let rec type_shape_to_complex_shape_exn ~cache ~rec_env (type_shape : Shape.t)
    (type_layout : Layout.t option) : t =
  let unknown_shape_exn =
    match type_layout with
    | Some type_layout -> fun () -> layout_to_unknown_shape type_layout
    | None -> fun () -> raise Layout_missing
  in
  let err_or_unknown_exn f =
    if !Clflags.dwarf_pedantic
    then f Misc.fatal_errorf
    else unknown_shape_exn ()
  in
  match type_shape.desc, type_layout with
  | Leaf, _ -> unknown_shape_exn ()
  | Unknown_type, _ -> unknown_shape_exn ()
  | Tuple args, (None | Some (Base Value)) -> (
    let args =
      List.map
        (fun sh ->
          type_shape_to_complex_shape ~cache ~rec_env sh (Layout.Base Value))
        (* CR sspies: In the future, we cannot assume that these are always
           values. This means we may need to use
           [type_shape_to_complex_shape_exn] and catch the exception on the
           outside. *)
        args
    in
    try
      let args = List.map force_runtime_shape_exn args in
      (* CR sspies: In the future, we will need to use the mixed block logic
         here. *)
      runtime (RS.tuple args)
    with Layout_missing -> runtime (RS.unknown Value))
  | Tuple _, Some type_layout ->
    err_or_unknown_exn (fun f ->
        f "tuple must have value layout, but got: %a" Layout.format type_layout)
  | At_layout (shape, ly), None ->
    type_shape_to_complex_shape ~cache ~rec_env shape ly
  | At_layout (shape, ly1), Some ly2 when Layout.equal ly1 ly2 ->
    type_shape_to_complex_shape ~cache ~rec_env shape ly1
  | At_layout (shape, layout), Some type_layout ->
    err_or_unknown_exn (fun f ->
        f
          "shape at layout does not match expected layout, expected %a, got \
           %a; shape: %a"
          Layout.format type_layout Layout.format layout Shape.print shape)
  | Predef (pre, args), _ -> (
    try
      let predef_shape =
        predef_to_complex_shape_exn ~cache ~rec_env pre ~args
      in
      runtime (RS.predef predef_shape)
    with Layout_missing -> (
      let layout = Shape.Predef.to_base_layout pre in
      let runtime_layout = RS.Runtime_layout.of_base_layout layout in
      match runtime_layout with
      | Void -> void
      | Other runtime_layout -> runtime (RS.unknown runtime_layout)))
  | Mu sh, type_layout ->
    (* We currently do not support unboxed recursive types (e.g., recursively
       defined mixed records). Those will fall back to default for printing the
       layout by forcing the runtime shape below. *)
    (* CR sspies: We should guess the layout from the recursive body [sh]
       instead of just using the current layout. *)
    let rec_env = Shape.DeBruijn_env.push rec_env type_layout in
    type_shape_to_complex_shape_exn ~cache ~rec_env sh type_layout
    |> force_runtime_shape_exn |> RS.mu |> runtime
  | Rec_var i, layout -> (
    match Shape.DeBruijn_env.get_opt rec_env ~de_bruijn_index:i, layout with
    | Some (Some (Layout.Base base as ly1)), ly2_opt
    (* We combine the [None] and [Some] layout cases with the guard: *)
      when Option.value ~default:true (Option.map (Layout.equal ly1) ly2_opt)
      -> (
      match RS.Runtime_layout.of_base_layout base with
      | Void -> void
      | Other runtime_layout -> runtime (RS.rec_var i runtime_layout))
    | Some (Some ly1), Some ly2 when not (Layout.equal ly1 ly2) ->
      err_or_unknown_exn (fun f ->
          f "Recursive variable has wrong layout. Expected %a, got %a"
            Layout.format ly2 Layout.format ly1)
      (* In all cases below, if there are two layouts, they are the same. *)
    | (Some (Some ly1), (None | Some _)) as _ly2_opt ->
      (*= In this case, either:
          - [ly1] is a product and [_ly2_opt] is None
          - [ly1] is a product and [_ly2_opt] is equal to Some [ly1].

          If [ly1] is a base layout, we would have taken the first branch (if
          equal to the second layout) or the second branch (if not equal to the
          second layout). *)
      layout_to_unknown_shape ly1
    | (Some None | None), Some (Layout.Base base) -> (
      match RS.Runtime_layout.of_base_layout base with
      | Void -> void
      | Other runtime_layout -> runtime (RS.rec_var i runtime_layout))
    | (Some None | None), Some (Layout.Product _ as ly) ->
      layout_to_unknown_shape ly
    | (Some None | None), None -> raise Layout_missing)
  | Alias sh, type_layout ->
    type_shape_to_complex_shape_exn ~cache ~rec_env sh type_layout
  | Arrow, (None | Some (Base Value)) -> runtime RS.func
  | Arrow, Some type_layout ->
    err_or_unknown_exn (fun f ->
        f "function must have value layout, but got: %a" Layout.format
          type_layout)
  | Unboxed_tuple shapes, None ->
    unboxed_tuple
      (List.map
         (fun sh -> type_shape_to_complex_shape_exn ~cache ~rec_env sh None)
         shapes)
  | Unboxed_tuple fields, Some (Product lys)
    when Int.equal (List.length lys) (List.length fields) ->
    unboxed_tuple
      (List.map2 (type_shape_to_complex_shape ~cache ~rec_env) fields lys)
  | Unboxed_tuple _, Some (Base b) ->
    err_or_unknown_exn (fun f ->
        f "unboxed tuple must have product layout, but got: %a" Layout.format
          (Layout.Base b))
  | Unboxed_tuple fields, Some (Product _ as type_layout) ->
    err_or_unknown_exn (fun f ->
        f
          "unboxed tuple layout length mismatch: expected a product layout of \
           length %d, but got: %a"
          (List.length fields) Layout.format type_layout)
  | Variant constructors, (None | Some (Base Value)) -> (
    try
      let constructors =
        List.map
          (fun { S.name; kind = _; args; constr_uid = _ } ->
            (* We first compute complex shapes for the fields. Then we unarize
               using the mixed block helpers defined above. *)
            let is_tuple_constructor =
              List.for_all
                (fun { S.field_name; _ } -> Option.is_none field_name)
                args
            in
            let source_level_fields =
              List.map
                (fun { S.field_name;
                       S.field_uid = _;
                       S.field_value = arg, layout
                     } ->
                  ( field_name,
                    type_shape_to_complex_shape ~cache ~rec_env arg layout ))
                args
            in
            let constr_args =
              lay_out_into_mixed_block_exn ~source_level_fields
            in
            if is_tuple_constructor
            then
              (* We clear the field names for tuple constructors. *)
              RS.constructor_with_tuple_arg ~name
                ~args:
                  (List.map
                     (RS.map_mixed_block_field_label (fun _ -> ()))
                     constr_args)
            else RS.constructor_with_record_arg ~name ~args:constr_args)
          constructors
      in
      runtime (RS.variant constructors)
    with Layout_missing ->
      runtime (RS.unknown Value)
      (* should only be raised by [lay_out_into_mixed_block_exn] *))
  | Variant _, Some ((Product _ | Base _) as ly) ->
    err_or_unknown_exn (fun f ->
        f "variant must have value layout, but got: %a" Layout.format ly)
  | Poly_variant constructors, (None | Some (Base Value)) -> (
    try
      let constructors =
        List.map
          (fun { S.pv_constr_name; pv_constr_args } ->
            let source_level_fields =
              List.map
                (fun arg ->
                  ( None,
                    type_shape_to_complex_shape ~cache ~rec_env arg
                      (Layout.Base Value) ))
                pv_constr_args
            in
            (* Currently, all arguments here are values, but sooner or later
               this is going to change. Worth adding the extra laying out
               already. *)
            let constr_args =
              lay_out_into_mixed_block_exn ~source_level_fields
            in
            RS.constructor_with_tuple_arg ~name:pv_constr_name
              ~args:
                (List.map
                   (RS.map_mixed_block_field_label (fun _ -> ()))
                   constr_args))
          constructors
      in
      runtime (RS.polymorphic_variant constructors)
    with Layout_missing ->
      runtime (RS.unknown Value)
      (* should only be raised by [lay_out_into_mixed_block_exn] *))
  | Poly_variant _, Some ((Product _ | Base _) as ly) ->
    err_or_unknown_exn (fun f ->
        f "polymorphic variant must have value layout, but got: %a"
          Layout.format ly)
  | Record { fields; kind = Record_unboxed_product }, None ->
    record_unboxed
      (List.map
         (fun (field_name, _, field_value, field_layout) ->
           ( field_name,
             type_shape_to_complex_shape ~cache ~rec_env field_value
               field_layout ))
         fields)
  | Record { fields; kind = Record_unboxed_product }, Some (Product lys)
    when List.equal Layout.equal (List.map (fun (_, _, _, ly) -> ly) fields) lys
    ->
    record_unboxed
      (List.map
         (fun (field_name, _, field_value, field_layout) ->
           ( field_name,
             type_shape_to_complex_shape ~cache ~rec_env field_value
               field_layout ))
         fields)
  | ( Record { fields; kind = Record_unboxed_product },
      Some ((Base _ | Product _) as ly) ) ->
    let layout_from_shapes =
      Layout.Product (List.map (fun (_, _, _, ly) -> ly) fields)
    in
    err_or_unknown_exn (fun f ->
        f "unboxed record expected to be of layout %a, but got: %a"
          Layout.format layout_from_shapes Layout.format ly)
  | Record { fields; kind = Record_boxed }, (None | Some (Base Value))
  | Record { fields; kind = Record_floats }, (None | Some (Base Value))
  | Record { fields; kind = Record_mixed _ }, (None | Some (Base Value)) -> (
    try
      let source_level_fields =
        List.map
          (fun (field_name, _, field_type, field_layout) ->
            let shape =
              type_shape_to_complex_shape ~cache ~rec_env field_type
                field_layout
            in
            Some field_name, shape)
          fields
      in
      let fields = lay_out_into_mixed_block_exn ~source_level_fields in
      runtime (RS.record_mixed fields)
    with Layout_missing ->
      runtime (RS.unknown Value)
      (* should only be raised by [lay_out_into_mixed_block_exn] at the
         moment *)
      (* CR sspies: In the future, if the inner layouts become options, we need
         to use [type_shape_to_complex_shape_exn] here and catch the
         exception. *)
    )
  | ( Record { fields; kind = Record_boxed | Record_mixed _ | Record_floats },
      Some ((Product _ | Base _) as ly) ) ->
    err_or_unknown_exn (fun f ->
        f "record must have value layout, but got: %a" Layout.format ly)
  | ( Record
        { fields = [(field_name, _, field_type, field_layout)];
          kind = Record_unboxed
        },
      ly2 )
    when Option.value ~default:true (Option.map (Layout.equal field_layout) ly2)
    -> (
    let shape =
      type_shape_to_complex_shape ~cache ~rec_env field_type field_layout
    in
    match lay_out_sequentially shape with
    | [shape] ->
      runtime
        (RS.record_attribute_unboxed
           ~contents:(RS.mixed_block_field ~field_type:shape ~label:field_name))
    | _ -> shape
    (* We drop [@@unboxed] records/variants when they contain actual product
       types in the debugger. *))
  | ( Record
        { fields = [(field_name, _, field_type, field_layout)];
          kind = Record_unboxed
        },
      None ) ->
    assert false (* ruled out by the [Option.value ~default:true] above. *)
  | ( Record
        { fields = [(field_name, _, field_type, field_layout)];
          kind = Record_unboxed
        },
      Some ly ) ->
    err_or_unknown_exn (fun f ->
        f
          "record with [@@unboxed] attribute with expected layout %a, but \
           field has layout %a"
          Layout.format ly Layout.format field_layout)
  | Record { fields = ([] | _ :: _ :: _) as fields; kind = Record_unboxed }, _
    ->
    err_or_unknown_exn (fun f ->
        f
          "record with [@@unboxed] attribute must have exactly one field, but \
           has: { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt (name, _, shape, _) ->
               Format.fprintf fmt "%s: %a;" name S.print shape))
          fields)
  | ( Variant_unboxed
        { name; variant_uid = _; arg_name; arg_uid = _; arg_shape; arg_layout },
      ly2 )
    when Option.value ~default:true (Option.map (Layout.equal arg_layout) ly2)
    -> (
    let shape =
      type_shape_to_complex_shape ~cache ~rec_env arg_shape arg_layout
    in
    match lay_out_sequentially shape with
    | [shape] ->
      runtime
        (RS.variant_attribute_unboxed ~constructor_name:name
           ~constructor_arg:
             (RS.mixed_block_field ~field_type:shape ~label:arg_name))
    | _ -> shape
    (* We drop [@@unboxed] records/variants when they contain actual product
       types in the debugger. *))
  | ( Variant_unboxed
        { name; variant_uid = _; arg_name; arg_uid = _; arg_shape; arg_layout },
      None ) ->
    assert false (* ruled out by the [Option.value ~default:true] above. *)
  | ( Variant_unboxed
        { name; variant_uid = _; arg_name; arg_uid = _; arg_shape; arg_layout },
      Some ly ) ->
    err_or_unknown_exn (fun f ->
        f
          "variant with [@@unboxed] attribute with expected layout %a, but \
           field has layout %a"
          Layout.format ly Layout.format arg_layout)
  | ( ( Var _ | Error _ | Proj _ | Abs _ | Comp_unit _ | Struct _ | Mutrec _
      | Constr _ | App _ | Proj_decl _ ),
      _ ) ->
    unknown_shape_exn ()

and predef_to_complex_shape_exn ~cache ~rec_env (predef : S.Predef.t) ~args :
    RS.predef =
  match predef, args with
  | Array, [elem_shape] -> (
    let elem_shape =
      type_shape_to_complex_shape_exn ~cache ~rec_env elem_shape None
    in
    let children = lay_out_sequentially elem_shape in
    match children with
    | [] -> err_exn (fun f -> f "array cannot contain only void elements")
    | [child] -> Array (Regular child)
    | children -> Array (Packed children)
    (* case for an unboxed product inside *))
  | Bytes, [] -> Bytes
  | Char, [] -> Char
  | Extension_constructor, [] -> Extension_constructor
  | Float, [] -> Float
  | Float32, [] -> Float32
  | Floatarray, [] -> Floatarray
  | Int, [] -> Int
  | Int8, [] -> Int8
  | Int16, [] -> Int16
  | Int32, [] -> Int32
  | Int64, [] -> Int64
  | Lazy_t, [elem_shape] ->
    let elem_shape =
      type_shape_to_complex_shape_exn ~cache ~rec_env elem_shape
        (Some (Layout.Base Value))
    in
    let elem_shape =
      match runtime_shape elem_shape with
      | Some s -> s
      | None -> RS.unknown Value
    in
    Lazy_t elem_shape
  | Nativeint, [] -> Nativeint
  | String, [] -> String
  | Simd vec_split, [] -> Simd (translate_simd_vec_split vec_split)
  | Exception, [] -> Exception
  | Unboxed unb, [] -> Unboxed (translate_unboxed unb)
  | ( ( Bytes | Char | Extension_constructor | Float | Float32 | Floatarray
      | Int | Int8 | Int16 | Int32 | Int64 | Nativeint | String | Simd _
      | Exception | Unboxed _ ),
      arg :: args ) ->
    err_exn (fun f ->
        f "predefined type %a does not take arguments, but got arguments %a"
          S.Predef.print predef
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             S.print)
          (arg :: args))
  | Array, ([] | _ :: _ :: _) ->
    err_exn (fun f ->
        f
          "predefined type array requires exactly one argument, but got %d \
           arguments: %a"
          (List.length args)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             S.print)
          args)
  | Lazy_t, ([] | _ :: _ :: _) ->
    err_exn (fun f ->
        f
          "predefined type lazy_t requires exactly one argument, but got %d \
           arguments: %a"
          (List.length args)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             S.print)
          args)

and type_shape_to_complex_shape ~cache ~rec_env type_shape type_layout : t =
  match Shape_cache.find_in_cache cache type_shape type_layout ~rec_env with
  | Some shape -> shape
  | None ->
    let shape =
      try
        type_shape_to_complex_shape_exn ~cache ~rec_env type_shape
          (Some type_layout)
      with Layout_missing -> layout_to_unknown_shape type_layout
    in
    Shape_cache.add_to_cache cache type_shape type_layout shape ~rec_env;
    shape

let type_shape_to_complex_shape ~cache evaluated_shape type_layout =
  let type_shape = Type_shape.Evaluated_shape.shape evaluated_shape in
  type_shape_to_complex_shape ~cache ~rec_env:Shape.DeBruijn_env.empty
    type_shape type_layout
