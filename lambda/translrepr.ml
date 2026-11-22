(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(* Translation of type-level representations to Lambda *)

open Lambda

let transl_instantiated_shape env loc sorts_and_types kind =
  let consts =
    Array.map
      (fun (sort, _ty) -> Jkind.Sort.default_for_transl_and_get sort)
      sorts_and_types
  in
  let all_scannable = Array.for_all Jkind.Sort.Const.is_scannable consts in
  let shape =
    if all_scannable
    then `Not_mixed
    else
      (* Build each field's shape from its defaulted sort, then refine it
         against the field's type. This costs a [value_kind] per scannable
         leaf (plus decomposing unboxed products), rather than a [type_jkind]
         per field as when going via the type's layout, but is at least as
         precise: refinement reads separability off the jkind for type
         variables, and also recurses into unboxed products. Using
         [Typeopt.layout] instead would skip the product work, but would lose
         immediacy for values inside products. *)
      let shape =
        Array.map2
          (fun sort (_sort, ty) ->
            Typeopt.layout_of_sort loc sort
            |> Lambda.mixed_block_element_of_layout
            |> Typeopt.refine_mixed_block_element env loc ty)
          consts sorts_and_types
      in
      (* Shapes containing splices are checked after static evaluation *)
      if not (Lambda.mixed_block_shape_has_splices shape)
      then Typeopt.assert_mixed_product_support_for_lambda_shape loc kind shape;
      `Mixed shape
  in
  shape, consts

let transl_instantiated_constructor env loc sorts_and_types kind :
    Lambda.constructor_representation =
  match transl_instantiated_shape env loc sorts_and_types kind with
  | `Not_mixed, _ -> Constructor_uniform_value
  | `Mixed shape, _ -> Constructor_mixed shape

let transl_constructor_representation env loc
    (shape : Types.constructor_representation) :
    Lambda.constructor_representation =
  match shape with
  | Constructor_uniform_value -> Constructor_uniform_value
  | Constructor_mixed shape ->
    Constructor_mixed (Lambda.transl_mixed_product_shape shape)
  | Constructor_immediate_all_void -> Constructor_immediate_all_void
  | Constructor_variable sorts_and_types ->
    transl_instantiated_constructor env loc sorts_and_types Cstr_tuple
  | Constructor_undetermined ->
    Misc.fatal_error
      "Translrepr.transl_constructor_representation: representation was not \
       instantiated"

let transl_variant_representation :
    Types.variant_representation -> Lambda.variant_representation = function
  | Variant_unboxed -> Variant_unboxed
  | Variant_boxed _ -> Variant_boxed
  | Variant_extensible -> Variant_extensible
  | Variant_with_null -> Variant_with_null

let transl_record_representation_and_sorts env loc
    (repres : Types.record_representation) :
    Lambda.record_representation
    * variable_sorts:Jkind.Sort.Const.t array option =
  match repres with
  | Record_variable sorts_and_types ->
    let shape, consts =
      transl_instantiated_shape env loc sorts_and_types Record
    in
    let repres : Lambda.record_representation =
      match shape with
      | `Not_mixed -> Record_boxed
      | `Mixed shape -> Record_mixed shape
    in
    repres, ~variable_sorts:(Some consts)
  | Record_inlined (tag, Constructor_variable sorts_and_types, vrep) ->
    let shape, consts =
      transl_instantiated_shape env loc sorts_and_types Cstr_record
    in
    let shape : Lambda.constructor_representation =
      match shape with
      | `Not_mixed -> Constructor_uniform_value
      | `Mixed shape -> Constructor_mixed shape
    in
    ( Record_inlined (tag, shape, transl_variant_representation vrep),
      ~variable_sorts:(Some consts) )
  | Record_undetermined | Record_inlined (_, Constructor_undetermined, _) ->
    Misc.fatal_error
      "Translrepr.transl_record_representation: representation was not \
       instantiated"
  | Record_dummy _ ->
    Misc.fatal_error
      "Translrepr.transl_record_representation: dummy representation"
  | Record_inlined (tag, shape, vrep) ->
    ( Record_inlined
        ( tag,
          transl_constructor_representation env loc shape,
          transl_variant_representation vrep ),
      ~variable_sorts:None )
  | Record_unboxed -> Record_unboxed, ~variable_sorts:None
  | Record_boxed -> Record_boxed, ~variable_sorts:None
  | Record_float -> Record_float, ~variable_sorts:None
  | Record_ufloat -> Record_ufloat, ~variable_sorts:None
  | Record_mixed shape ->
    Record_mixed (Lambda.transl_mixed_product_shape shape), ~variable_sorts:None

let transl_record_representation env loc repres =
  let repres, ~variable_sorts:_ =
    transl_record_representation_and_sorts env loc repres
  in
  repres

let label_sort_for_representation (label : Data_types.label_description)
    (repres : Lambda.record_representation) ~record_sort ~variable_sorts =
  match repres with
  | Record_unboxed | Record_inlined (_, _, Variant_unboxed) -> record_sort
  | Record_boxed | Record_float | Record_ufloat | Record_mixed _
  | Record_inlined (_, (Constructor_uniform_value | Constructor_mixed _), _) ->
    begin match variable_sorts with
    | Some sorts -> sorts.(label.lbl_pos)
    | None ->
      begin match label.lbl_sort with
      | Some sort -> sort
      | None ->
        Misc.fatal_errorf
          "no sort for label %s despite finalized representation" label.lbl_name
      end
    end
  | Record_inlined (_, Constructor_immediate_all_void, _) ->
    Misc.fatal_error
      "label_sort_for_representation: unexpected immediate representation"

module Sum = struct
  module Repr = Jkind_types.Sum_repr

  let layouts loc (repr : Repr.t) =
    List.map
      (fun base ->
        Typeopt.layout_of_sort
          (Debuginfo.Scoped_location.to_location loc)
          (Jkind.Sort.Const.base base))
      repr.fields

  let zero : Jkind_types.Sort.base -> lambda = function
    | Scannable -> lambda_unit
    | Float64 -> Lconst (Const_base (Const_unboxed_float "0."))
    | Float32 -> Lconst (Const_base (Const_unboxed_float32 "0."))
    | Word -> Lconst (Const_base (Const_unboxed_nativeint Targetint.zero))
    | Bits32 -> Lconst (Const_base (Const_unboxed_int32 0l))
    | Bits64 -> Lconst (Const_base (Const_unboxed_int64 0L))
    | Bits8 -> Lconst (Const_base (Const_untagged_int8 Numbers.Int8.zero))
    | Bits16 -> Lconst (Const_base (Const_untagged_int16 Numbers.Int16.zero))
    | Untagged_immediate -> Lconst (Const_base (Const_untagged_int 0))
    | Void | Vec128 | Vec256 | Vec512 | Mask ->
      Misc.fatal_error "Translrepr.Sum.zero: invalid carrier field"

  let convert loc (src : Jkind_types.Sort.base) (dst : Jkind_types.Sort.base)
      arg =
    let integral (base : Jkind_types.Sort.base) =
      let open Scalar in
      match base with
      | Bits8 -> integral (Naked (Taggable Int8))
      | Bits16 -> integral (Naked (Taggable Int16))
      | Untagged_immediate -> integral (Naked (Taggable Int))
      | Bits32 -> integral (Naked (Boxable (Int32 Any_locality_mode)))
      | Bits64 -> integral (Naked (Boxable (Int64 Any_locality_mode)))
      | Word -> integral (Naked (Boxable (Nativeint Any_locality_mode)))
      | _ -> Misc.fatal_error "Translrepr.Sum.convert: expected integer"
    in
    let cast_bits src dst arg ~name ~native_name =
      let repr base =
        Primitive.Prim_global, Same_as_ocaml_repr (Jkind.Sort.Const.base base)
      in
      Lprim
        ( Pccall
            (Primitive.make ~name ~native_name ~alloc:false ~effects:No_effects
               ~coeffects:No_coeffects ~c_builtin:true
               ~native_repr_args:[repr src]
               ~native_repr_res:(repr dst) ~is_layout_poly:false),
          [arg],
          loc )
    in
    let int_bits (base : Jkind_types.Sort.base) : Jkind_types.Sort.base =
      match base with
      | Float32 -> Jkind_types.Sort.Bits32
      | Float64 -> Bits64
      | _ -> base
    in
    if Jkind_types.Sort.equal_base src dst
    then arg
    else
      let bits =
        match src with
        | Float64 ->
          cast_bits src Bits64 arg ~name:"caml_int64_bits_of_float"
            ~native_name:"caml_int64_bits_of_float_unboxed"
        | Float32 ->
          cast_bits src Bits32 arg ~name:"caml_float32_to_bits_bytecode"
            ~native_name:"caml_float32_to_bits"
        | _ -> arg
      in
      let bits =
        if Jkind_types.Sort.equal_base (int_bits src) (int_bits dst)
        then bits
        else
          static_cast
            ~src:(integral (int_bits src))
            ~dst:(integral (int_bits dst))
            bits ~loc
      in
      match dst with
      | Float64 ->
        cast_bits Bits64 dst bits ~name:"caml_int64_float_of_bits"
          ~native_name:"caml_int64_float_of_bits_unboxed"
      | Float32 ->
        cast_bits Bits32 dst bits ~name:"caml_float32_of_bits_bytecode"
          ~native_name:"caml_float32_of_bits"
      | _ -> bits

  let tag_size : Jkind_types.Sort.base -> _ = function
    | Bits32 -> Scalar.integral (Naked (Boxable (Int32 Any_locality_mode)))
    | Scannable -> Scalar.integral (Value (Taggable Int))
    | _ -> Misc.fatal_error "Translrepr.Sum: invalid tag field"

  let cast_tag loc src dst arg =
    if Jkind_types.Sort.equal_base src dst
    then arg
    else static_cast ~src:(tag_size src) ~dst:(tag_size dst) arg ~loc

  let shift_tag loc src dst offset arg =
    if offset = 0
    then cast_tag loc src dst arg
    else
      let index = cast_tag loc src Scannable arg in
      cast_tag loc Scannable dst (add int index (tagged_immediate offset) ~loc)

  let tag_field loc (repr : Repr.t) arg =
    match repr.fields with
    | [_] -> arg
    | _ -> Lprim (Punboxed_product_field (0, layouts loc repr), [arg], loc)

  let tag loc (repr : Repr.t) arg =
    cast_tag loc (List.hd repr.fields) Scannable (tag_field loc repr arg)

  let inject loc (repr : Repr.t) index args =
    let arm = repr.arms.(index) in
    let bases = Array.of_list repr.fields in
    let fields = Array.map zero bases in
    fields.(0)
      <- (match repr.fields with
         | Bits32 :: _ ->
           Lconst
             (Const_base (Const_unboxed_int32 (Int32.of_int arm.first_leaf)))
         | _ -> tagged_immediate arm.first_leaf);
    let rec fill (projection : Repr.projection) layout arg =
      match projection, layout with
      | Void, _ -> ()
      | Nested_tag base, _ ->
        fields.(0) <- shift_tag loc base bases.(0) arm.first_leaf arg
      | Field (i, base), _ -> fields.(i) <- convert loc base bases.(i) arg
      | Product projections, Punboxed_product layouts ->
        List.iteri
          (fun i (projection, layout) ->
            fill projection layout
              (Lprim (Punboxed_product_field (i, layouts), [arg], loc)))
          (List.combine projections layouts)
      | Product _, _ ->
        Misc.fatal_error "Translrepr.Sum.inject: expected product"
    in
    (* Evaluate each argument once in the same right-to-left order as a
       regular constructor, including arguments that contribute no fields. *)
    let rec build = function
      | [] when Array.length fields = 1 -> fields.(0)
      | [] ->
        Lprim
          (Pmake_unboxed_product (layouts loc repr), Array.to_list fields, loc)
      | (projection, (layout, arg)) :: rest ->
        let id = Ident.create_local "payload" in
        fill projection layout (Lvar id);
        Llet (Strict, layout, id, debug_uid_none, arg, build rest)
    in
    build (List.rev (List.combine arm.arguments args))

  let project loc (repr : Repr.t) index arg layouts_of_args =
    let arm = repr.arms.(index) in
    let fields = layouts loc repr in
    let rec read (projection : Repr.projection) layout =
      match projection, layout with
      | Void, _ -> Lprim (Punbox_unit, [lambda_unit], loc)
      | Nested_tag base, _ ->
        shift_tag loc (List.hd repr.fields) base (-arm.first_leaf)
          (tag_field loc repr arg)
      | Field (i, base), _ ->
        convert loc (List.nth repr.fields i) base
          (Lprim (Punboxed_product_field (i, fields), [arg], loc))
      | Product projections, Punboxed_product layouts ->
        Lprim
          ( Pmake_unboxed_product layouts,
            List.map2 read projections layouts,
            loc )
      | Product _, _ ->
        Misc.fatal_error "Translrepr.Sum.project: expected product"
    in
    List.map2 read arm.arguments layouts_of_args
end

module Boxed_sum = struct
  module Repr = Jkind_types.Sum_repr

  type direction =
    | Box of locality_mode
    | Unbox

  let translate loc direction leaves argument =
    let field_sorts (sort : Jkind.Sort.Const.t) =
      if Jkind.Sort.Const.all_void sort
      then []
      else match sort with Product fields -> fields | _ -> [sort]
    in
    let arms = List.map field_sorts leaves in
    let repr =
      match Repr.make arms with
      | Some repr -> repr
      | None -> Misc.fatal_error "Boxed_sum: unrepresentable sum"
    in
    let location = Debuginfo.Scoped_location.to_location loc in
    let carrier_layout =
      Typeopt.layout_of_sort location (Repr.carrier leaves)
    in
    let arg_layout, result_layout =
      match direction with
      | Box _ -> carrier_layout, layout_any_value
      | Unbox -> layout_any_value, carrier_layout
    in
    name_lambda Strict argument arg_layout (fun id ->
        let arg = Lvar id in
        let next_const = ref 0 and next_block = ref 0 in
        let cases =
          List.mapi
            (fun index fields ->
              let constant = List.is_empty fields in
              let next = if constant then next_const else next_block in
              let tag = !next in
              incr next;
              if (not constant) && tag > Config.max_tag
              then
                Location.raise_errorf ~loc:location
                  "A boxed sum cannot have more than %d non-constant \
                   alternatives."
                  (Config.max_tag + 1);
              let layouts = List.map (Typeopt.layout_of_sort location) fields in
              let shape () =
                let shape =
                  Array.of_list (List.map mixed_block_element_of_layout layouts)
                in
                Typeopt.assert_mixed_product_support_for_lambda_shape location
                  Cstr_tuple shape;
                shape
              in
              let uniform = List.for_all Jkind.Sort.Const.is_scannable fields in
              match direction with
              | Box mode ->
                let action =
                  if constant
                  then tagged_immediate tag
                  else
                    let shape =
                      if uniform then All_value else Shape (shape ())
                    in
                    let args = Sum.project loc repr index arg layouts in
                    Lprim (Pmakeblock (tag, Mutable, shape, mode), args, loc)
                in
                `Const (index, action)
              | Unbox ->
                let field =
                  if uniform
                  then fun pos -> Pfield (pos, Pointer, Reads_vary)
                  else
                    let shape = shape () in
                    fun pos -> Pmixedfield ([pos], shape, Reads_vary)
                in
                let args =
                  List.mapi
                    (fun pos layout -> layout, Lprim (field pos, [arg], loc))
                    layouts
                in
                let case = tag, Sum.inject loc repr index args in
                if constant then `Const case else `Block case)
            arms
        in
        let consts, blocks =
          List.partition_map
            (function
              | `Const case -> Either.Left case
              | `Block case -> Either.Right case)
            cases
        in
        let scrutinee =
          match direction with Box _ -> Sum.tag loc repr arg | Unbox -> arg
        in
        Lswitch
          ( scrutinee,
            { sw_numconsts = List.length consts;
              sw_consts = consts;
              sw_numblocks = List.length blocks;
              sw_blocks = blocks;
              sw_failaction = None
            },
            loc,
            result_layout ))
end
