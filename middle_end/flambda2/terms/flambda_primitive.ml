(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: Remove uses of polymorphic comparison *)

module K = Flambda_kind

type classification_for_printing =
  | Constructive
  | Destructive
  | Neither

module Lazy_block_tag = struct
  type t = Lambda.lazy_block_tag =
    | Lazy_tag
    | Forward_tag

  let print ppf t =
    match t with
    | Lazy_tag -> Format.fprintf ppf "Lazy_block"
    | Forward_tag -> Format.fprintf ppf "Forward_block"

  let compare t1 t2 =
    match t1, t2 with
    | Lazy_tag, Lazy_tag | Forward_tag, Forward_tag -> 0
    | Lazy_tag, Forward_tag -> -1
    | Forward_tag, Lazy_tag -> 1

  let to_tag t =
    match t with Lazy_tag -> Tag.lazy_tag | Forward_tag -> Tag.forward_tag
end

module Block_kind = struct
  type t =
    | Values of Tag.Scannable.t * K.With_subkind.t list
    | Naked_floats
    | Mixed of Tag.Scannable.t * Flambda_kind.Mixed_block_shape.t

  let to_shape t : _ * K.Block_shape.t =
    match t with
    | Values (tag, _) -> Tag.Scannable.to_tag tag, Scannable Value_only
    | Naked_floats -> Tag.double_array_tag, Float_record
    | Mixed (tag, fields) ->
      Tag.Scannable.to_tag tag, Scannable (Mixed_record fields)

  let [@ocamlformat "disable"] print ppf t =
   match t with
   | Values (tag, shape) ->
     Format.fprintf ppf
       "@[<hov 1>(Values@ \
         @[<hov 1>(tag %a)@]@ \
         @[<hov 1>(shape@ @[<hov 1>(%a)@])@])@]"
       Tag.Scannable.print tag
       (Format.pp_print_list ~pp_sep:Format.pp_print_space
      K.With_subkind.print) shape
   | Naked_floats ->
     Format.pp_print_string ppf "Naked_floats"
   | Mixed (tag, shape) ->
     Format.fprintf ppf
       "@[<hov 1>(Mixed@ \
         @[<hov 1>(tag %a)@]@ \
         @[<hov 1>(shape@ @[<hov 1>(%a)@])@])@]"
       Tag.Scannable.print tag
       (Format.pp_print_list ~pp_sep:Format.pp_print_space
          K.print) (Array.to_list (K.Mixed_block_shape.field_kinds shape))

  let compare t1 t2 =
    match t1, t2 with
    | Values (tag1, shape1), Values (tag2, shape2) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else Misc.Stdlib.List.compare K.With_subkind.compare shape1 shape2
    | Naked_floats, Naked_floats -> 0
    | Mixed (tag1, shape1), Mixed (tag2, shape2) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0 then c else K.Mixed_block_shape.compare shape1 shape2
    | Values _, _ -> -1
    | _, Values _ -> 1
    | Naked_floats, _ -> -1
    | _, Naked_floats -> 1
end

module Init_or_assign = struct
  type t =
    | Initialization
    | Assignment of Alloc_mode.For_assignments.t

  let [@ocamlformat "disable"] print ppf t =
    let fprintf = Format.fprintf in
    match t with
    | Initialization -> fprintf ppf "Init"
    | Assignment Heap -> fprintf ppf "Assign Heap"
    | Assignment Local -> fprintf ppf "Assign Local"

  let compare = Stdlib.compare

  let to_lambda t : Lambda.initialization_or_assignment =
    match t with
    | Initialization -> Heap_initialization
    | Assignment mode -> Assignment (Alloc_mode.For_assignments.to_lambda mode)
end

module Array_kind = struct
  type t =
    | Immediates
    | Values
    | Naked_floats
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
    | Naked_vec128s
    | Naked_vec256s
    | Naked_vec512s
    | Unboxed_product of t list

  let rec print ppf t =
    match t with
    | Immediates -> Format.pp_print_string ppf "Immediates"
    | Naked_floats -> Format.pp_print_string ppf "Naked_floats"
    | Naked_float32s -> Format.pp_print_string ppf "Naked_float32s"
    | Values -> Format.pp_print_string ppf "Values"
    | Naked_int32s -> Format.pp_print_string ppf "Naked_int32s"
    | Naked_int64s -> Format.pp_print_string ppf "Naked_int64s"
    | Naked_nativeints -> Format.pp_print_string ppf "Naked_nativeints"
    | Naked_vec128s -> Format.pp_print_string ppf "Naked_vec128s"
    | Naked_vec256s -> Format.pp_print_string ppf "Naked_vec256s"
    | Naked_vec512s -> Format.pp_print_string ppf "Naked_vec512s"
    | Unboxed_product fields ->
      Format.fprintf ppf "@[<hov 1>(Unboxed_product@ @[<hov 1>(%a)@])@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
        fields

  let compare = Stdlib.compare

  let rec element_kinds t =
    match t with
    | Immediates -> [K.With_subkind.tagged_immediate]
    | Values -> [K.With_subkind.any_value]
    | Naked_floats -> [K.With_subkind.naked_float]
    | Naked_float32s -> [K.With_subkind.naked_float32]
    | Naked_int32s -> [K.With_subkind.naked_int32]
    | Naked_int64s -> [K.With_subkind.naked_int64]
    | Naked_nativeints -> [K.With_subkind.naked_nativeint]
    | Naked_vec128s -> [K.With_subkind.naked_vec128]
    | Naked_vec256s -> [K.With_subkind.naked_vec256]
    | Naked_vec512s -> [K.With_subkind.naked_vec512]
    | Unboxed_product kinds -> List.concat_map element_kinds kinds

  let element_kinds_for_primitive t =
    element_kinds t |> List.map K.With_subkind.kind

  let must_be_gc_scannable t =
    let kinds = element_kinds t in
    if not (List.exists K.With_subkind.must_be_gc_scannable kinds)
    then false
    else if List.for_all K.With_subkind.may_be_gc_scannable kinds
    then true
    else
      Misc.fatal_errorf
        "Unboxed product array kind contains both elements that must be \
         scannable and that cannot be scanned:@ %a"
        print t

  let rec width_in_scalars t =
    match t with
    | Immediates | Values | Naked_floats | Naked_float32s | Naked_int32s
    | Naked_int64s | Naked_nativeints | Naked_vec128s | Naked_vec256s
    | Naked_vec512s ->
      1
    | Unboxed_product kinds ->
      List.fold_left
        (fun width array_kind -> width + width_in_scalars array_kind)
        0 kinds
end

module Array_load_kind = struct
  type t =
    | Immediates
    | Values
    | Naked_floats
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
    | Naked_vec128s
    | Naked_vec256s
    | Naked_vec512s

  let print ppf t =
    match t with
    | Immediates -> Format.pp_print_string ppf "Immediates"
    | Values -> Format.pp_print_string ppf "Values"
    | Naked_floats -> Format.fprintf ppf "Naked_floats"
    | Naked_float32s -> Format.pp_print_string ppf "Naked_float32s"
    | Naked_int32s -> Format.pp_print_string ppf "Naked_int32s"
    | Naked_int64s -> Format.pp_print_string ppf "Naked_int64s"
    | Naked_nativeints -> Format.pp_print_string ppf "Naked_nativeints"
    | Naked_vec128s -> Format.pp_print_string ppf "Naked_vec128s"
    | Naked_vec256s -> Format.pp_print_string ppf "Naked_vec256s"
    | Naked_vec512s -> Format.pp_print_string ppf "Naked_vec512s"

  let compare = Stdlib.compare

  let kind_of_loaded_value t =
    match t with
    | Immediates -> Flambda_kind.With_subkind.tagged_immediate
    | Values -> Flambda_kind.With_subkind.any_value
    | Naked_floats -> Flambda_kind.With_subkind.naked_float
    | Naked_float32s -> Flambda_kind.With_subkind.naked_float32
    | Naked_int32s -> Flambda_kind.With_subkind.naked_int32
    | Naked_int64s -> Flambda_kind.With_subkind.naked_int64
    | Naked_nativeints -> Flambda_kind.With_subkind.naked_nativeint
    | Naked_vec128s -> Flambda_kind.With_subkind.naked_vec128
    | Naked_vec256s -> Flambda_kind.With_subkind.naked_vec256
    | Naked_vec512s -> Flambda_kind.With_subkind.naked_vec512
end

module Array_set_kind = struct
  type t =
    | Immediates
    | Values of Init_or_assign.t
    | Naked_floats
    | Naked_float32s
    | Naked_int32s
    | Naked_int64s
    | Naked_nativeints
    | Naked_vec128s
    | Naked_vec256s
    | Naked_vec512s

  let print ppf t =
    match t with
    | Immediates -> Format.pp_print_string ppf "Immediates"
    | Values init_or_assign ->
      Format.fprintf ppf "@[<hov 1>(Values %a)@]" Init_or_assign.print
        init_or_assign
    | Naked_floats -> Format.fprintf ppf "Naked_floats"
    | Naked_float32s -> Format.pp_print_string ppf "Naked_float32s"
    | Naked_int32s -> Format.pp_print_string ppf "Naked_int32s"
    | Naked_int64s -> Format.pp_print_string ppf "Naked_int64s"
    | Naked_nativeints -> Format.pp_print_string ppf "Naked_nativeints"
    | Naked_vec128s -> Format.pp_print_string ppf "Naked_vec128s"
    | Naked_vec256s -> Format.pp_print_string ppf "Naked_vec256s"
    | Naked_vec512s -> Format.pp_print_string ppf "Naked_vec512s"

  let compare = Stdlib.compare

  let kind_of_new_value t =
    match t with
    | Immediates -> Flambda_kind.With_subkind.tagged_immediate
    | Values _ -> Flambda_kind.With_subkind.any_value
    | Naked_floats -> Flambda_kind.With_subkind.naked_float
    | Naked_float32s -> Flambda_kind.With_subkind.naked_float32
    | Naked_int32s -> Flambda_kind.With_subkind.naked_int32
    | Naked_int64s -> Flambda_kind.With_subkind.naked_int64
    | Naked_nativeints -> Flambda_kind.With_subkind.naked_nativeint
    | Naked_vec128s -> Flambda_kind.With_subkind.naked_vec128
    | Naked_vec256s -> Flambda_kind.With_subkind.naked_vec256
    | Naked_vec512s -> Flambda_kind.With_subkind.naked_vec512
end

module Array_kind_for_length = struct
  type t =
    | Array_kind of Array_kind.t
    | Float_array_opt_dynamic

  let compare t1 t2 =
    match t1, t2 with
    | Array_kind a1, Array_kind a2 -> Array_kind.compare a1 a2
    | Float_array_opt_dynamic, Float_array_opt_dynamic -> 0
    | Array_kind _, _ -> -1
    | _, Array_kind _ -> 1

  let print ppf t =
    match t with
    | Array_kind a -> Array_kind.print ppf a
    | Float_array_opt_dynamic ->
      Format.pp_print_string ppf "Float_array_opt_dynamic"

  let width_in_scalars t =
    match t with
    | Float_array_opt_dynamic -> 1
    | Array_kind array_kind -> Array_kind.width_in_scalars array_kind
end

module Duplicate_block_kind = struct
  type t =
    | Values of
        { tag : Tag.Scannable.t;
          length : Targetint_31_63.t
        }
    | Naked_floats of { length : Targetint_31_63.t }
    | Mixed

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Values { tag; length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Block_of_values \
          @[<hov 1>(tag@ %a)@]@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        Tag.Scannable.print tag
        Targetint_31_63.print length
    | Naked_floats { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Block_of_naked_floats@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        Targetint_31_63.print length
    | Mixed ->
      Format.fprintf ppf
        "@[<hov 1>(Mixed)@]"

  let compare t1 t2 =
    match t1, t2 with
    | ( Values { tag = tag1; length = length1 },
        Values { tag = tag2; length = length2 } ) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0 then c else Targetint_31_63.compare length1 length2
    | Naked_floats { length = length1 }, Naked_floats { length = length2 } ->
      Targetint_31_63.compare length1 length2
    | Mixed, Mixed -> 0
    | Naked_floats _, Mixed -> -1
    | Mixed, Naked_floats _ -> 1
    | Values _, _ -> -1
    | _, Values _ -> 1
end

module Duplicate_array_kind = struct
  type t =
    | Immediates
    | Values
    | Naked_floats of { length : Targetint_31_63.t option }
    | Naked_float32s of { length : Targetint_31_63.t option }
    | Naked_int32s of { length : Targetint_31_63.t option }
    | Naked_int64s of { length : Targetint_31_63.t option }
    | Naked_nativeints of { length : Targetint_31_63.t option }
    | Naked_vec128s of { length : Targetint_31_63.t option }
    | Naked_vec256s of { length : Targetint_31_63.t option }
    | Naked_vec512s of { length : Targetint_31_63.t option }

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Immediates -> Format.pp_print_string ppf "Immediates"
    | Values -> Format.pp_print_string ppf "Values"
    | Naked_floats { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_floats@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length
    | Naked_float32s { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_float32s@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length
    | Naked_int32s { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_int32s@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length
    | Naked_int64s { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_int64s@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length
    | Naked_nativeints { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_nativeints@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length
    | Naked_vec128s { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_vec128s@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length
    | Naked_vec256s { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_vec256s@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length
    | Naked_vec512s { length; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_vec512s@ \
          @[<hov 1>(length@ %a)@]\
          )@]"
        (Misc.Stdlib.Option.print Targetint_31_63.print) length

  let compare t1 t2 =
    match t1, t2 with
    | Immediates, Immediates | Values, Values -> 0
    | Naked_floats { length = length1 }, Naked_floats { length = length2 } ->
      Option.compare Targetint_31_63.compare length1 length2
    | Naked_float32s { length = length1 }, Naked_float32s { length = length2 }
      ->
      Option.compare Targetint_31_63.compare length1 length2
    | Naked_int32s { length = length1 }, Naked_int32s { length = length2 } ->
      Option.compare Targetint_31_63.compare length1 length2
    | Naked_int64s { length = length1 }, Naked_int64s { length = length2 } ->
      Option.compare Targetint_31_63.compare length1 length2
    | ( Naked_nativeints { length = length1 },
        Naked_nativeints { length = length2 } ) ->
      Option.compare Targetint_31_63.compare length1 length2
    | Naked_vec128s { length = length1 }, Naked_vec128s { length = length2 } ->
      Option.compare Targetint_31_63.compare length1 length2
    | Naked_vec256s { length = length1 }, Naked_vec256s { length = length2 } ->
      Option.compare Targetint_31_63.compare length1 length2
    | Naked_vec512s { length = length1 }, Naked_vec512s { length = length2 } ->
      Option.compare Targetint_31_63.compare length1 length2
    | Immediates, _ -> -1
    | _, Immediates -> 1
    | Values, _ -> -1
    | _, Values -> 1
    | Naked_floats _, _ -> -1
    | _, Naked_floats _ -> 1
    | Naked_float32s _, _ -> -1
    | _, Naked_float32s _ -> 1
    | Naked_int32s _, _ -> -1
    | _, Naked_int32s _ -> 1
    | Naked_int64s _, _ -> -1
    | _, Naked_int64s _ -> 1
    | Naked_vec128s _, _ -> -1
    | _, Naked_vec128s _ -> 1
    | Naked_vec256s _, _ -> -1
    | _, Naked_vec256s _ -> 1
    | Naked_vec512s _, _ -> -1
    | _, Naked_vec512s _ -> 1
end

module Block_access_field_kind = struct
  type t =
    | Any_value
    | Immediate

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Any_value -> Format.pp_print_string ppf "Any_value"
    | Immediate -> Format.pp_print_string ppf "Immediate"

  let compare = Stdlib.compare
end

module Mixed_block_access_field_kind = struct
  type t =
    | Value_prefix of Block_access_field_kind.t
    | Flat_suffix of K.Flat_suffix_element.t

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Value_prefix field_kind ->
        Format.fprintf ppf
          "@[<hov 1>(Value_prefix@ \
           @[<hov 1>(field_kind@ %a)@]\
           )@]"
          Block_access_field_kind.print field_kind
    | Flat_suffix flat_element ->
        Format.fprintf ppf
          "@[<hov 1>(Flat_suffix \
           @[<hov 1>(flat_element@ %a)@]\
           )@]"
          K.Flat_suffix_element.print flat_element

  let compare t1 t2 =
    match t1, t2 with
    | Value_prefix field_kind1, Value_prefix field_kind2 ->
      Block_access_field_kind.compare field_kind1 field_kind2
    | Flat_suffix element_kind1, Flat_suffix element_kind2 ->
      K.Flat_suffix_element.compare element_kind1 element_kind2
    | Value_prefix _, Flat_suffix _ -> -1
    | Flat_suffix _, Value_prefix _ -> 1

  let to_element_kind = function
    | Value_prefix _ -> K.value
    | Flat_suffix kind -> K.Flat_suffix_element.kind kind
end

module Block_access_kind = struct
  type t =
    | Values of
        { tag : Tag.Scannable.t Or_unknown.t;
          size : Targetint_31_63.t Or_unknown.t;
          field_kind : Block_access_field_kind.t
        }
    | Naked_floats of { size : Targetint_31_63.t Or_unknown.t }
    | Mixed of
        { tag : Tag.Scannable.t Or_unknown.t;
          size : Targetint_31_63.t Or_unknown.t;
          field_kind : Mixed_block_access_field_kind.t;
          shape : Flambda_kind.Mixed_block_shape.t
        }

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Values { tag; size; field_kind; } ->
      Format.fprintf ppf
        "@[<hov 1>(Values@ \
          @[<hov 1>(tag@ %a)@]@ \
          @[<hov 1>(size@ %a)@]@ \
          @[<hov 1>(field_kind@ %a)@]\
          )@]"
        (Or_unknown.print Tag.Scannable.print) tag
        (Or_unknown.print Targetint_31_63.print) size
        Block_access_field_kind.print field_kind
    | Naked_floats { size; } ->
      Format.fprintf ppf
        "@[<hov 1>(Naked_floats@ \
          @[<hov 1>(size@ %a)@]\
          )@]"
        (Or_unknown.print Targetint_31_63.print) size
    | Mixed { tag; size; field_kind; shape = _ } ->
      Format.fprintf ppf
        "@[<hov 1>(Mixed@ \
          @[<hov 1>(tag@ %a)@]@ \
          @[<hov 1>(size@ %a)@]@ \
          @[<hov 1>(field_kind@ %a)@]\
          )@]"
        (Or_unknown.print Tag.Scannable.print) tag
        (Or_unknown.print Targetint_31_63.print) size
        Mixed_block_access_field_kind.print field_kind

  let element_kind_for_load t =
    match t with
    | Values _ -> K.value
    | Naked_floats _ -> K.naked_float
    | Mixed { field_kind; _ } ->
      Mixed_block_access_field_kind.to_element_kind field_kind

  let element_subkind_for_load t =
    match t with
    | Values { field_kind = Any_value; _ }
    | Mixed { field_kind = Value_prefix Any_value; _ } ->
      K.With_subkind.any_value
    | Values { field_kind = Immediate; _ }
    | Mixed { field_kind = Value_prefix Immediate; _ } ->
      K.With_subkind.tagged_immediate
    | Naked_floats _ -> K.With_subkind.naked_float
    | Mixed { field_kind = Flat_suffix field_kind; _ } ->
      K.Flat_suffix_element.to_kind_with_subkind field_kind

  let to_block_shape t : K.Block_shape.t =
    match t with
    | Values _ -> Scannable Value_only
    | Naked_floats _ -> Float_record
    | Mixed { shape; _ } -> Scannable (Mixed_record shape)

  let element_kind_for_set = element_kind_for_load

  let compare t1 t2 =
    match t1, t2 with
    | ( Values { tag = tag1; size = size1; field_kind = field_kind1 },
        Values { tag = tag2; size = size2; field_kind = field_kind2 } ) ->
      let c = Or_unknown.compare Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else
        let c = Or_unknown.compare Targetint_31_63.compare size1 size2 in
        if c <> 0
        then c
        else Block_access_field_kind.compare field_kind1 field_kind2
    | Naked_floats { size = size1 }, Naked_floats { size = size2 } ->
      Or_unknown.compare Targetint_31_63.compare size1 size2
    | ( Mixed
          { tag = tag1; size = size1; field_kind = field_kind1; shape = shape1 },
        Mixed
          { tag = tag2; size = size2; field_kind = field_kind2; shape = shape2 }
      ) ->
      let c = Or_unknown.compare Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else
        let c = Or_unknown.compare Targetint_31_63.compare size1 size2 in
        if c <> 0
        then c
        else
          let c =
            Mixed_block_access_field_kind.compare field_kind1 field_kind2
          in
          if c <> 0
          then c
          else Flambda_kind.Mixed_block_shape.compare shape1 shape2
    | Naked_floats _, Mixed _ -> -1
    | Mixed _, Naked_floats _ -> 1
    | Values _, _ -> -1
    | _, Values _ -> 1
end

type string_or_bytes =
  | String
  | Bytes

type array_like_operation =
  | Reading
  | Writing

let effects_of_operation operation =
  match operation with
  | Reading -> Effects.No_effects
  | Writing -> Effects.Arbitrary_effects

let reading_from_a_block mutable_or_immutable =
  let effects = effects_of_operation Reading in
  let coeffects =
    match (mutable_or_immutable : Mutability.t) with
    | Immutable | Immutable_unique -> Coeffects.No_coeffects
    | Mutable -> Coeffects.Has_coeffects
  in
  effects, coeffects, Placement.Strict

let reading_from_an_array (array_kind : Array_kind.t)
    (mutable_or_immutable : Mutability.t) =
  let effects : Effects.t =
    match array_kind with
    | Immediates | Values | Naked_floats | Naked_float32s | Naked_int32s
    | Naked_int64s | Naked_nativeints | Naked_vec128s | Naked_vec256s
    | Naked_vec512s | Unboxed_product _ ->
      No_effects
  in
  let coeffects =
    match mutable_or_immutable with
    | Immutable | Immutable_unique -> Coeffects.No_coeffects
    | Mutable -> Coeffects.Has_coeffects
  in
  effects, coeffects, Placement.Strict

let reading_from_a_string_or_bigstring mutable_or_immutable =
  reading_from_a_block mutable_or_immutable

let writing_to_a_block =
  let effects = effects_of_operation Writing in
  effects, Coeffects.No_coeffects, Placement.Strict

let writing_to_an_array = writing_to_a_block

let writing_to_bytes_or_bigstring = writing_to_a_block

let bigarray_kind = K.value

let bigstring_kind = K.value

let block_kind = K.value

let array_kind = K.value

let string_or_bytes_kind = K.value

let array_index_kind = K.value

let string_or_bigstring_index_kind = K.naked_immediate

let bytes_or_bigstring_index_kind = K.naked_immediate

type 'signed_or_unsigned comparison =
  | Eq
  | Neq
  | Lt of 'signed_or_unsigned
  | Gt of 'signed_or_unsigned
  | Le of 'signed_or_unsigned
  | Ge of 'signed_or_unsigned

type 'signed_or_unsigned comparison_behaviour =
  | Yielding_bool of 'signed_or_unsigned comparison
  | Yielding_int_like_compare_functions of 'signed_or_unsigned

let print_comparison print_signed_or_unsigned ppf c =
  let fprintf = Format.fprintf in
  match c with
  | Neq -> fprintf ppf "<>"
  | Eq -> fprintf ppf "="
  | Lt signed_or_unsigned ->
    fprintf ppf "<%a" print_signed_or_unsigned signed_or_unsigned
  | Le signed_or_unsigned ->
    fprintf ppf "<=%a" print_signed_or_unsigned signed_or_unsigned
  | Gt signed_or_unsigned ->
    fprintf ppf ">%a" print_signed_or_unsigned signed_or_unsigned
  | Ge signed_or_unsigned ->
    fprintf ppf ">=%a" print_signed_or_unsigned signed_or_unsigned

let print_comparison_and_behaviour print_signed_or_unsigned ppf behaviour =
  match behaviour with
  | Yielding_bool comparison ->
    print_comparison print_signed_or_unsigned ppf comparison
  | Yielding_int_like_compare_functions signed_or_unsigned ->
    Format.fprintf ppf "<compare%a>" print_signed_or_unsigned signed_or_unsigned

type signed_or_unsigned =
  | Signed
  | Unsigned

let print_signed_or_unsigned ppf signed_or_unsigned =
  match signed_or_unsigned with
  | Signed -> Format.fprintf ppf ""
  | Unsigned -> Format.fprintf ppf "u"

type equality_comparison =
  | Eq
  | Neq

let print_equality_comparison ppf op =
  match op with
  | Eq -> Format.pp_print_string ppf "Eq"
  | Neq -> Format.pp_print_string ppf "Neq"

module Bigarray_kind = struct
  type t =
    | Float16
    | Float32
    | Float32_t
    | Float64
    | Sint8
    | Uint8
    | Sint16
    | Uint16
    | Int32
    | Int64
    | Int_width_int
    | Targetint_width_int
    | Complex32
    | Complex64

  let element_kind t =
    match t with
    | Float16 | Float32 | Float64 -> K.naked_float
    | Float32_t -> K.naked_float32
    | Sint8 | Uint8 | Sint16 | Uint16 -> K.naked_immediate
    | Int32 -> K.naked_int32
    | Int64 -> K.naked_int64
    | Int_width_int -> K.naked_immediate
    | Targetint_width_int -> K.naked_nativeint
    | Complex32 | Complex64 ->
      (* See [copy_two_doubles] in bigarray_stubs.c. *)
      K.value

  let print ppf t =
    let fprintf = Format.fprintf in
    match t with
    | Float16 -> fprintf ppf "Float16"
    | Float32 -> fprintf ppf "Float32"
    | Float32_t -> fprintf ppf "Float32_t"
    | Float64 -> fprintf ppf "Float64"
    | Sint8 -> fprintf ppf "Sint8"
    | Uint8 -> fprintf ppf "Uint8"
    | Sint16 -> fprintf ppf "Sint16"
    | Uint16 -> fprintf ppf "Uint16"
    | Int32 -> fprintf ppf "Int32"
    | Int64 -> fprintf ppf "Int64"
    | Int_width_int -> fprintf ppf "Int_width_int"
    | Targetint_width_int -> fprintf ppf "Targetint_width_int"
    | Complex32 -> fprintf ppf "Complex32"
    | Complex64 -> fprintf ppf "Complex64"

  let from_lambda (kind : Lambda.bigarray_kind) =
    match kind with
    | Pbigarray_unknown -> None
    | Pbigarray_float16 -> Some Float16
    | Pbigarray_float32 -> Some Float32
    | Pbigarray_float32_t -> Some Float32_t
    | Pbigarray_float64 -> Some Float64
    | Pbigarray_sint8 -> Some Sint8
    | Pbigarray_uint8 -> Some Uint8
    | Pbigarray_sint16 -> Some Sint16
    | Pbigarray_uint16 -> Some Uint16
    | Pbigarray_int32 -> Some Int32
    | Pbigarray_int64 -> Some Int64
    | Pbigarray_caml_int -> Some Int_width_int
    | Pbigarray_native_int -> Some Targetint_width_int
    | Pbigarray_complex32 -> Some Complex32
    | Pbigarray_complex64 -> Some Complex64

  let to_lambda t : Lambda.bigarray_kind =
    match t with
    | Float16 -> Pbigarray_float16
    | Float32 -> Pbigarray_float32
    | Float32_t -> Pbigarray_float32_t
    | Float64 -> Pbigarray_float64
    | Sint8 -> Pbigarray_sint8
    | Uint8 -> Pbigarray_uint8
    | Sint16 -> Pbigarray_sint16
    | Uint16 -> Pbigarray_uint16
    | Int32 -> Pbigarray_int32
    | Int64 -> Pbigarray_int64
    | Int_width_int -> Pbigarray_caml_int
    | Targetint_width_int -> Pbigarray_native_int
    | Complex32 -> Pbigarray_complex32
    | Complex64 -> Pbigarray_complex64
end

module Bigarray_layout = struct
  type t =
    | C
    | Fortran

  let print ppf t =
    let fprintf = Format.fprintf in
    match t with C -> fprintf ppf "C" | Fortran -> fprintf ppf "Fortran"

  let from_lambda (layout : Lambda.bigarray_layout) =
    match layout with
    | Pbigarray_unknown_layout -> None
    | Pbigarray_c_layout -> Some C
    | Pbigarray_fortran_layout -> Some Fortran
end

let reading_from_a_bigarray kind =
  match (kind : Bigarray_kind.t) with
  | Complex32 | Complex64 ->
    ( Effects.Only_generative_effects Immutable,
      Coeffects.Has_coeffects,
      Placement.Strict )
  | Float16 | Float32 | Float32_t | Float64 | Sint8 | Uint8 | Sint16 | Uint16
  | Int32 | Int64 | Int_width_int | Targetint_width_int ->
    Effects.No_effects, Coeffects.Has_coeffects, Placement.Strict

(* The bound checks are taken care of outside the array primitive (using an
   explicit test and switch in the flambda code, see
   lambda_to_flambda_primitives.ml). *)
let writing_to_a_bigarray kind =
  match (kind : Bigarray_kind.t) with
  | Float16 | Float32 | Float32_t | Float64 | Sint8 | Uint8 | Sint16 | Uint16
  | Int32 | Int64 | Int_width_int | Targetint_width_int | Complex32
  | Complex64
    (* Technically, the write of a complex generates read of fields from the
       given complex, but since those reads are immutable, there is no
       observable coeffect. *) ->
    Effects.Arbitrary_effects, Coeffects.No_coeffects, Placement.Strict

let bigarray_index_kind = K.value

type string_like_value =
  | String
  | Bytes
  | Bigstring

let print_string_like_value ppf s =
  match s with
  | String -> Format.pp_print_string ppf "string"
  | Bytes -> Format.pp_print_string ppf "bytes"
  | Bigstring -> Format.pp_print_string ppf "bigstring"

type bytes_like_value =
  | Bytes
  | Bigstring

let print_bytes_like_value ppf b =
  match b with
  | Bytes -> Format.pp_print_string ppf "bytes"
  | Bigstring -> Format.pp_print_string ppf "bigstring"

type string_accessor_width =
  | Eight
  | Sixteen
  | Thirty_two
  | Single
  | Sixty_four
  | One_twenty_eight of { aligned : bool }
  | Two_fifty_six of { aligned : bool }
  | Five_twelve of { aligned : bool }

let print_string_accessor_width ppf w =
  let fprintf = Format.fprintf in
  match w with
  | Eight -> fprintf ppf "8"
  | Sixteen -> fprintf ppf "16"
  | Thirty_two -> fprintf ppf "32"
  | Single -> fprintf ppf "f32"
  | Sixty_four -> fprintf ppf "64"
  | One_twenty_eight { aligned = false } -> fprintf ppf "128u"
  | One_twenty_eight { aligned = true } -> fprintf ppf "128a"
  | Two_fifty_six { aligned = false } -> fprintf ppf "256u"
  | Two_fifty_six { aligned = true } -> fprintf ppf "256a"
  | Five_twelve { aligned = false } -> fprintf ppf "512u"
  | Five_twelve { aligned = true } -> fprintf ppf "512a"

let byte_width_of_string_accessor_width width =
  match width with
  | Eight -> 1
  | Sixteen -> 2
  | Thirty_two -> 4
  | Single -> 4
  | Sixty_four -> 8
  | One_twenty_eight _ -> 16
  | Two_fifty_six _ -> 32
  | Five_twelve _ -> 64

let kind_of_string_accessor_width width =
  match width with
  | Eight | Sixteen -> K.value
  | Thirty_two -> K.naked_int32
  | Single -> K.naked_float32
  | Sixty_four -> K.naked_int64
  | One_twenty_eight _ -> K.naked_vec128
  | Two_fifty_six _ -> K.naked_vec256
  | Five_twelve _ -> K.naked_vec512

type float_bitwidth =
  | Float32
  | Float64

type num_dimensions = int

let print_num_dimensions ppf d = Format.fprintf ppf "%d" d

type unary_int_arith_op = Swap_byte_endianness

let print_unary_int_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with Swap_byte_endianness -> fprintf ppf "bswap"

type unary_float_arith_op =
  | Abs
  | Neg

let print_unary_float_arith_op ppf width op =
  let fprintf = Format.fprintf in
  match width, op with
  | Float64, Abs -> fprintf ppf "abs"
  | Float64, Neg -> fprintf ppf "~-"
  | Float32, Abs -> fprintf ppf "Float32.abs"
  | Float32, Neg -> fprintf ppf "Float32.~-"

type arg_kinds =
  | Variadic_mixed of K.Mixed_block_shape.t
  | Variadic_all_of_kind of K.t
  | Variadic_zero_or_one of K.t
  | Variadic_unboxed_product of Flambda_kind.t list

type result_kind =
  | Singleton of K.t
  | Unit

type nullary_primitive =
  | Invalid of K.t
  | Optimised_out of K.t
  | Probe_is_enabled of { name : string }
  | Enter_inlined_apply of { dbg : Inlined_debuginfo.t }
  | Dls_get
  | Poll
  | Cpu_relax

let nullary_primitive_eligible_for_cse = function
  | Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _
  | Dls_get | Poll | Cpu_relax ->
    false

let compare_nullary_primitive p1 p2 =
  match p1, p2 with
  | Invalid k1, Invalid k2 -> K.compare k1 k2
  | Optimised_out k1, Optimised_out k2 -> K.compare k1 k2
  | Probe_is_enabled { name = name1 }, Probe_is_enabled { name = name2 } ->
    String.compare name1 name2
  | Enter_inlined_apply { dbg = dbg1 }, Enter_inlined_apply { dbg = dbg2 } ->
    Inlined_debuginfo.compare dbg1 dbg2
  | Dls_get, Dls_get -> 0
  | Poll, Poll -> 0
  | Cpu_relax, Cpu_relax -> 0
  | ( Invalid _,
      ( Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _ | Dls_get
      | Poll | Cpu_relax ) ) ->
    -1
  | ( Optimised_out _,
      (Probe_is_enabled _ | Enter_inlined_apply _ | Dls_get | Poll | Cpu_relax)
    ) ->
    -1
  | Optimised_out _, Invalid _ -> 1
  | Probe_is_enabled _, (Enter_inlined_apply _ | Dls_get | Poll | Cpu_relax) ->
    -1
  | Probe_is_enabled _, (Invalid _ | Optimised_out _) -> 1
  | Enter_inlined_apply _, (Invalid _ | Optimised_out _ | Probe_is_enabled _) ->
    1
  | Enter_inlined_apply _, (Dls_get | Poll | Cpu_relax) -> -1
  | ( Dls_get,
      (Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _)
    ) ->
    1
  | Dls_get, (Poll | Cpu_relax) -> -1
  | ( (Poll | Cpu_relax),
      ( Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _
      | Dls_get | Cpu_relax ) ) ->
    1
  | Cpu_relax, Poll -> -1

let equal_nullary_primitive p1 p2 = compare_nullary_primitive p1 p2 = 0

let print_nullary_primitive ppf p =
  match p with
  | Invalid _ ->
    Format.fprintf ppf "%tInvalid%t" Flambda_colours.invalid_keyword
      Flambda_colours.pop
  | Optimised_out _ ->
    Format.fprintf ppf "%tOptimised_out%t" Flambda_colours.elide
      Flambda_colours.pop
  | Probe_is_enabled { name } ->
    Format.fprintf ppf "@[<hov 1>(Probe_is_enabled@ %s)@]" name
  | Enter_inlined_apply { dbg } ->
    Format.fprintf ppf "@[<hov 1>(Enter_inlined_apply@ %a)@]"
      Inlined_debuginfo.print dbg
  | Dls_get -> Format.pp_print_string ppf "Dls_get"
  | Poll -> Format.pp_print_string ppf "Poll"
  | Cpu_relax -> Format.pp_print_string ppf "Cpu_relax"

let result_kind_of_nullary_primitive p : result_kind =
  match p with
  | Invalid k -> Singleton k
  | Optimised_out k -> Singleton k
  | Probe_is_enabled _ -> Singleton K.naked_immediate
  | Enter_inlined_apply _ -> Unit
  | Dls_get -> Singleton K.value
  | Poll | Cpu_relax -> Unit

let coeffects_of_mode : Alloc_mode.For_allocations.t -> Coeffects.t = function
  | Local _ -> Coeffects.Has_coeffects
  | Heap -> Coeffects.No_coeffects

let effects_and_coeffects_of_nullary_primitive p : Effects_and_coeffects.t =
  match p with
  | Invalid _ -> Arbitrary_effects, Has_coeffects, Strict
  | Optimised_out _ -> No_effects, No_coeffects, Strict
  | Probe_is_enabled _ ->
    (* This doesn't really have effects, but we want to make sure it never gets
       moved around. *)
    Arbitrary_effects, Has_coeffects, Strict
  | Enter_inlined_apply _ ->
    (* This doesn't really have effects, but without effects, these primitives
       get deleted during lambda_to_flambda. *)
    Arbitrary_effects, Has_coeffects, Strict
  | Dls_get -> No_effects, Has_coeffects, Strict
  | Poll | Cpu_relax -> Arbitrary_effects, Has_coeffects, Strict

let nullary_classify_for_printing p =
  match p with
  | Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _
  | Dls_get | Poll | Cpu_relax ->
    Neither

module Reinterpret_64_bit_word = struct
  type t =
    | Tagged_int63_as_unboxed_int64
    | Unboxed_int64_as_tagged_int63
    | Unboxed_int64_as_unboxed_float64
    | Unboxed_float64_as_unboxed_int64

  let compare = Stdlib.compare

  let print ppf t =
    match t with
    | Tagged_int63_as_unboxed_int64 ->
      Format.pp_print_string ppf "Tagged_int63_as_unboxed_int64"
    | Unboxed_int64_as_tagged_int63 ->
      Format.pp_print_string ppf "Unboxed_int64_as_tagged_int63"
    | Unboxed_int64_as_unboxed_float64 ->
      Format.pp_print_string ppf "Unboxed_int64_as_unboxed_float64"
    | Unboxed_float64_as_unboxed_int64 ->
      Format.pp_print_string ppf "Unboxed_float64_as_unboxed_int64"
end

type unary_primitive =
  | Block_load of
      { kind : Block_access_kind.t;
        mut : Mutability.t;
        field : Targetint_31_63.t
      }
  | Duplicate_block of { kind : Duplicate_block_kind.t }
  | Duplicate_array of
      { kind : Duplicate_array_kind.t;
        source_mutability : Mutability.t;
        destination_mutability : Mutability.t
      }
  | Is_int of { variant_only : bool }
  | Is_null
  | Get_tag
  | Array_length of Array_kind_for_length.t
  | Bigarray_length of { dimension : int }
  | String_length of string_or_bytes
  | Int_as_pointer of Alloc_mode.For_allocations.t
  | Opaque_identity of
      { middle_end_only : bool;
        kind : K.t
      }
  | Int_arith of Flambda_kind.Standard_int.t * unary_int_arith_op
  | Float_arith of float_bitwidth * unary_float_arith_op
  | Num_conv of
      { src : Flambda_kind.Standard_int_or_float.t;
        dst : Flambda_kind.Standard_int_or_float.t
      }
  | Boolean_not
  | Reinterpret_64_bit_word of Reinterpret_64_bit_word.t
  | Unbox_number of Flambda_kind.Boxable_number.t
  | Box_number of Flambda_kind.Boxable_number.t * Alloc_mode.For_allocations.t
  | Untag_immediate
  | Tag_immediate
  | Project_function_slot of
      { move_from : Function_slot.t;
        move_to : Function_slot.t
      }
  | Project_value_slot of
      { project_from : Function_slot.t;
        value_slot : Value_slot.t
      }
  | Is_boxed_float
  | Is_flat_float_array
  | End_region of { ghost : bool }
  | End_try_region of { ghost : bool }
  | Obj_dup
  | Get_header
  | Peek of Flambda_kind.Standard_int_or_float.t
  | Make_lazy of Lazy_block_tag.t

(* Here and below, operations that are genuine projections shouldn't be eligible
   for CSE, since we deal with projections through types. *)
let unary_primitive_eligible_for_cse p ~arg =
  match p with
  | Block_load _ -> false
  | Duplicate_array _ -> false
  | Duplicate_block { kind = _ } -> false
  | Is_int _ | Is_null | Get_tag | Get_header -> true
  | Array_length _ -> true
  | Bigarray_length _ -> false
  | String_length _ -> true
  | Int_as_pointer m -> ( match m with Heap -> true | Local _ -> false)
  | Opaque_identity _ -> false
  | Int_arith _ -> true
  | Float_arith _ ->
    (* See comment in effects_and_coeffects *)
    Flambda_features.float_const_prop ()
  | Num_conv _ | Boolean_not | Reinterpret_64_bit_word _ -> true
  | Unbox_number _ | Untag_immediate -> false
  | Box_number (_, Local _) ->
    (* For the moment we don't CSE any local allocations. *)
    (* CR mshinwell: relax this in the future? *)
    false
  | Box_number (_, Heap) | Tag_immediate ->
    (* Boxing or tagging of constants will yield values that can be lifted and
       if needs be deduplicated -- so there's no point in adding CSE variables
       to hold them. *)
    Simple.is_var arg
  | Project_function_slot _ | Project_value_slot _ -> false
  | Is_boxed_float | Is_flat_float_array -> true
  | End_region _ | End_try_region _ | Obj_dup | Peek _ | Make_lazy _ -> false

let compare_unary_primitive p1 p2 =
  let unary_primitive_numbering p =
    match p with
    | Block_load _ -> 0
    | Duplicate_array _ -> 1
    | Duplicate_block _ -> 2
    | Is_int _ -> 3
    | Get_tag -> 4
    | Array_length _ -> 5
    | Bigarray_length _ -> 6
    | String_length _ -> 7
    | Int_as_pointer _ -> 8
    | Opaque_identity _ -> 9
    | Int_arith _ -> 10
    | Float_arith _ -> 11
    | Num_conv _ -> 12
    | Boolean_not -> 13
    | Reinterpret_64_bit_word _ -> 14
    | Unbox_number _ -> 15
    | Box_number _ -> 16
    | Untag_immediate -> 17
    | Tag_immediate -> 18
    | Project_function_slot _ -> 19
    | Project_value_slot _ -> 20
    | Is_boxed_float -> 21
    | Is_flat_float_array -> 22
    | End_region _ -> 23
    | End_try_region _ -> 24
    | Obj_dup -> 25
    | Get_header -> 26
    | Is_null -> 27
    | Peek _ -> 28
    | Make_lazy _ -> 29
  in
  match p1, p2 with
  | ( Block_load { kind = kind1; mut = mut1; field = field1 },
      Block_load { kind = kind2; mut = mut2; field = field2 } ) ->
    let c = Block_access_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Mutability.compare mut1 mut2 in
      if c <> 0 then c else Targetint_31_63.compare field1 field2
  | ( Duplicate_array
        { kind = kind1;
          source_mutability = source_mutability1;
          destination_mutability = destination_mutability1
        },
      Duplicate_array
        { kind = kind2;
          source_mutability = source_mutability2;
          destination_mutability = destination_mutability2
        } ) ->
    let c = Duplicate_array_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare source_mutability1 source_mutability2 in
      if c <> 0
      then c
      else Stdlib.compare destination_mutability1 destination_mutability2
  | Duplicate_block { kind = kind1 }, Duplicate_block { kind = kind2 } ->
    Duplicate_block_kind.compare kind1 kind2
  | ( Is_int { variant_only = variant_only1 },
      Is_int { variant_only = variant_only2 } ) ->
    Bool.compare variant_only1 variant_only2
  | Get_tag, Get_tag -> 0
  | String_length kind1, String_length kind2 -> Stdlib.compare kind1 kind2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Num_conv { src = src1; dst = dst1 }, Num_conv { src = src2; dst = dst2 } ->
    let c = K.Standard_int_or_float.compare src1 src2 in
    if c <> 0 then c else K.Standard_int_or_float.compare dst1 dst2
  | Float_arith (width1, op1), Float_arith (width2, op2) ->
    let c = Stdlib.compare width1 width2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Array_length ak1, Array_length ak2 -> Array_kind_for_length.compare ak1 ak2
  | Bigarray_length { dimension = dim1 }, Bigarray_length { dimension = dim2 }
    ->
    Stdlib.compare dim1 dim2
  | Reinterpret_64_bit_word reinterpret1, Reinterpret_64_bit_word reinterpret2
    ->
    Reinterpret_64_bit_word.compare reinterpret1 reinterpret2
  | Unbox_number kind1, Unbox_number kind2 ->
    K.Boxable_number.compare kind1 kind2
  | Box_number (kind1, alloc_mode1), Box_number (kind2, alloc_mode2) ->
    let c = K.Boxable_number.compare kind1 kind2 in
    if c <> 0
    then c
    else Alloc_mode.For_allocations.compare alloc_mode1 alloc_mode2
  | Untag_immediate, Untag_immediate -> 0
  | Tag_immediate, Tag_immediate -> 0
  | ( Project_function_slot { move_from = move_from1; move_to = move_to1 },
      Project_function_slot { move_from = move_from2; move_to = move_to2 } ) ->
    let c = Function_slot.compare move_from1 move_from2 in
    if c <> 0 then c else Function_slot.compare move_to1 move_to2
  | ( Project_value_slot
        { project_from = function_slot1; value_slot = value_slot1 },
      Project_value_slot
        { project_from = function_slot2; value_slot = value_slot2 } ) ->
    let c = Function_slot.compare function_slot1 function_slot2 in
    if c <> 0 then c else Value_slot.compare value_slot1 value_slot2
  | ( Opaque_identity { middle_end_only = middle_end_only1; kind = kind1 },
      Opaque_identity { middle_end_only = middle_end_only2; kind = kind2 } ) ->
    let c = Bool.compare middle_end_only1 middle_end_only2 in
    if c <> 0 then c else K.compare kind1 kind2
  | Int_as_pointer alloc_mode1, Int_as_pointer alloc_mode2 ->
    Alloc_mode.For_allocations.compare alloc_mode1 alloc_mode2
  | End_region { ghost = ghost1 }, End_region { ghost = ghost2 } ->
    Bool.compare ghost1 ghost2
  | End_try_region { ghost = ghost1 }, End_try_region { ghost = ghost2 } ->
    Bool.compare ghost1 ghost2
  | Peek kind1, Peek kind2 ->
    Flambda_kind.Standard_int_or_float.compare kind1 kind2
  | Make_lazy lazy_tag1, Make_lazy lazy_tag2 ->
    Lazy_block_tag.compare lazy_tag1 lazy_tag2
  | ( ( Block_load _ | Duplicate_array _ | Duplicate_block _ | Is_int _
      | Is_null | Get_tag | String_length _ | Int_as_pointer _
      | Opaque_identity _ | Int_arith _ | Num_conv _ | Boolean_not
      | Reinterpret_64_bit_word _ | Float_arith _ | Array_length _
      | Bigarray_length _ | Unbox_number _ | Box_number _ | Untag_immediate
      | Tag_immediate | Project_function_slot _ | Project_value_slot _
      | Is_boxed_float | Is_flat_float_array | End_region _ | End_try_region _
      | Obj_dup | Get_header | Peek _ | Make_lazy _ ),
      _ ) ->
    Stdlib.compare (unary_primitive_numbering p1) (unary_primitive_numbering p2)

let equal_unary_primitive p1 p2 = compare_unary_primitive p1 p2 = 0

let print_unary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_load { kind; mut; field } ->
    fprintf ppf "@[(Block_load@ %a@ %a@ %a)@]" Block_access_kind.print kind
      Mutability.print mut Targetint_31_63.print field
  | Duplicate_block { kind } ->
    fprintf ppf "@[<hov 1>(Duplicate_block %a)@]" Duplicate_block_kind.print
      kind
  | Duplicate_array { kind; source_mutability; destination_mutability } ->
    fprintf ppf "@[<hov 1>(Duplicate_array %a (source %a) (dest %a))@]"
      Duplicate_array_kind.print kind Mutability.print source_mutability
      Mutability.print destination_mutability
  | Is_int { variant_only } ->
    if variant_only then fprintf ppf "Is_int" else fprintf ppf "Is_int_generic"
  | Is_null -> fprintf ppf "Is_null"
  | Get_tag -> fprintf ppf "Get_tag"
  | String_length _ -> fprintf ppf "String_length"
  | Int_as_pointer alloc_mode ->
    fprintf ppf "Int_as_pointer[%a]" Alloc_mode.For_allocations.print alloc_mode
  | Opaque_identity { middle_end_only; kind } ->
    fprintf ppf "@[(Opaque_identity@ (middle_end_only %b) (kind %a))@]"
      middle_end_only K.print kind
  | Int_arith (_k, o) -> print_unary_int_arith_op ppf o
  | Num_conv { src; dst } ->
    fprintf ppf "Num_conv_%a_to_%a"
      Flambda_kind.Standard_int_or_float.print_lowercase src
      Flambda_kind.Standard_int_or_float.print_lowercase dst
  | Boolean_not -> fprintf ppf "Boolean_not"
  | Reinterpret_64_bit_word reinterpret ->
    fprintf ppf "@[<hov 1>(Reinterpret_64_bit_word@ %a)@]"
      Reinterpret_64_bit_word.print reinterpret
  | Float_arith (width, op) -> print_unary_float_arith_op ppf width op
  | Array_length ak ->
    fprintf ppf "(Array_length %a)" Array_kind_for_length.print ak
  | Bigarray_length { dimension } ->
    fprintf ppf "Bigarray_length %a" print_num_dimensions dimension
  | Untag_immediate -> fprintf ppf "Untag_imm"
  | Unbox_number k ->
    fprintf ppf "Unbox_%a" K.Boxable_number.print_lowercase_short k
  | Tag_immediate -> fprintf ppf "Tag_imm"
  | Box_number (k, alloc_mode) ->
    fprintf ppf "Box_%a[%a]" K.Boxable_number.print_lowercase_short k
      Alloc_mode.For_allocations.print alloc_mode
  | Project_function_slot { move_from; move_to } ->
    Format.fprintf ppf "@[(Project_function_slot@ (%a \u{2192} %a))@]"
      Function_slot.print move_from Function_slot.print move_to
  | Project_value_slot { project_from; value_slot } ->
    Format.fprintf ppf "@[(Project_value_slot@ (%a@ %a))@]" Function_slot.print
      project_from Value_slot.print value_slot
  | Is_boxed_float -> fprintf ppf "Is_boxed_float"
  | Is_flat_float_array -> fprintf ppf "Is_flat_float_array"
  | End_region { ghost } ->
    Format.fprintf ppf "End_region%s" (if ghost then "_ghost" else "")
  | End_try_region { ghost } ->
    Format.fprintf ppf "End_try_region%s" (if ghost then "_ghost" else "")
  | Obj_dup -> Format.pp_print_string ppf "Obj_dup"
  | Get_header -> Format.pp_print_string ppf "Get_header"
  | Peek kind ->
    fprintf ppf "@[(Peek@ %a)@]"
      Flambda_kind.Standard_int_or_float.print_lowercase kind
  | Make_lazy lazy_tag ->
    fprintf ppf "@[<hov 1>(Make_lazy@ %a)@]" Lazy_block_tag.print lazy_tag

let arg_kind_of_unary_primitive p =
  match p with
  | Block_load _ -> block_kind
  | Duplicate_array _ | Duplicate_block _ -> K.value
  | Is_int _ -> K.value
  | Is_null -> K.value
  | Get_tag -> K.value
  | String_length _ -> K.value
  | Int_as_pointer _ -> K.value
  | Opaque_identity { middle_end_only = _; kind } -> kind
  | Int_arith (kind, _) -> K.Standard_int.to_kind kind
  | Num_conv { src; dst = _ } -> K.Standard_int_or_float.to_kind src
  | Boolean_not -> K.value
  | Reinterpret_64_bit_word reinterpret -> (
    match reinterpret with
    | Tagged_int63_as_unboxed_int64 -> K.value
    | Unboxed_int64_as_tagged_int63 -> K.naked_int64
    | Unboxed_int64_as_unboxed_float64 -> K.naked_int64
    | Unboxed_float64_as_unboxed_int64 -> K.naked_float)
  | Float_arith (Float64, _) -> K.naked_float
  | Float_arith (Float32, _) -> K.naked_float32
  | Array_length _ | Bigarray_length _ -> K.value
  | Unbox_number _ | Untag_immediate -> K.value
  | Box_number (kind, _) -> K.Boxable_number.unboxed_kind kind
  | Tag_immediate -> K.naked_immediate
  | Project_function_slot _ | Project_value_slot _ | Is_boxed_float
  | Is_flat_float_array ->
    K.value
  | End_region _ -> K.region
  | End_try_region _ -> K.region
  | Obj_dup -> K.value
  | Get_header -> K.value
  | Peek _ -> K.naked_nativeint
  | Make_lazy _ -> K.value

let result_kind_of_unary_primitive p : result_kind =
  match p with
  | Block_load { kind; _ } ->
    Singleton (Block_access_kind.element_kind_for_load kind)
  | Duplicate_array _ | Duplicate_block _ -> Singleton K.value
  | Is_int _ | Is_null | Get_tag -> Singleton K.naked_immediate
  | String_length _ -> Singleton K.naked_immediate
  | Int_as_pointer _ ->
    (* This primitive is *only* to be used when the resulting pointer points at
       something which is a valid OCaml value (even if outside of the heap). *)
    Singleton K.value
  | Opaque_identity { middle_end_only = _; kind } -> Singleton kind
  | Int_arith (kind, _) -> Singleton (K.Standard_int.to_kind kind)
  | Num_conv { src = _; dst } -> Singleton (K.Standard_int_or_float.to_kind dst)
  | Boolean_not -> Singleton K.value
  | Reinterpret_64_bit_word reinterpret -> (
    match reinterpret with
    | Tagged_int63_as_unboxed_int64 -> Singleton K.naked_int64
    | Unboxed_int64_as_tagged_int63 -> Singleton K.value
    | Unboxed_int64_as_unboxed_float64 -> Singleton K.naked_float
    | Unboxed_float64_as_unboxed_int64 -> Singleton K.naked_int64)
  | Float_arith (Float64, _) -> Singleton K.naked_float
  | Float_arith (Float32, _) -> Singleton K.naked_float32
  | Array_length _ -> Singleton K.value
  | Bigarray_length _ -> Singleton K.naked_immediate
  | Unbox_number kind -> Singleton (K.Boxable_number.unboxed_kind kind)
  | Untag_immediate -> Singleton K.naked_immediate
  | Box_number _ | Tag_immediate | Project_function_slot _ -> Singleton K.value
  | Project_value_slot { value_slot; _ } ->
    Singleton (Value_slot.kind value_slot)
  | Is_boxed_float | Is_flat_float_array -> Singleton K.naked_immediate
  | End_region _ -> Singleton K.value
  | End_try_region _ -> Singleton K.value
  | Obj_dup -> Singleton K.value
  | Get_header -> Singleton K.naked_nativeint
  | Peek kind -> Singleton (K.Standard_int_or_float.to_kind kind)
  | Make_lazy _ -> Singleton K.value

let effects_and_coeffects_of_unary_primitive p : Effects_and_coeffects.t =
  match p with
  | Block_load { mut; _ } -> reading_from_a_block mut
  | Duplicate_array { kind = _; source_mutability; destination_mutability; _ }
    -> (
    match source_mutability with
    | Immutable ->
      (* [Obj.truncate] has now been removed. *)
      Only_generative_effects destination_mutability, No_coeffects, Strict
    | Immutable_unique ->
      (* CR vlaviron: this should never occur, but it's hard to express it
         without duplicating the mutability type

         mshinwell: Adding a second mutability type seems like a good thing to
         avoid confusion in the future. It could maybe be a submodule of
         [Mutability]. *)
      Only_generative_effects destination_mutability, No_coeffects, Strict
    | Mutable ->
      Only_generative_effects destination_mutability, Has_coeffects, Strict)
  | Duplicate_block { kind = _ } ->
    (* We have to assume that the fields might be mutable. (This information
       isn't currently propagated from [Lambda].) *)
    Only_generative_effects Mutable, Has_coeffects, Strict
  | Is_int _ | Is_null -> No_effects, No_coeffects, Strict
  | Get_tag ->
    (* [Obj.truncate] has now been removed. *)
    No_effects, No_coeffects, Strict
  | String_length _ -> No_effects, No_coeffects, Strict
  | Int_as_pointer alloc_mode ->
    No_effects, coeffects_of_mode alloc_mode, Strict
  | Opaque_identity _ -> Arbitrary_effects, Has_coeffects, Strict
  | Int_arith (_, Swap_byte_endianness)
  | Num_conv _ | Boolean_not | Reinterpret_64_bit_word _ ->
    No_effects, No_coeffects, Strict
  | Float_arith (_width, (Abs | Neg)) ->
    (* Float operations are not really pure since they actually access the
       globally mutable rounding mode, which can be changed (but only from C
       code). The Flambda_features.float_const_prop tracks whether we are
       allowed to make optimizations assuming a non-changing rounding mode (i.e.
       'float_const_prop () = true' means that the rounding should not be
       changed by user code, and thus float optimizations are allowed).
       Therefore, when 'float_const_prop () = false', we add coeffects to float
       operations so that they cannot be moved through an effectful operation.
       (e.g. a call to a c stub that changes the rounding mode). See also the
       comment in binary_primitive_eligible_for_cse. *)
    if Flambda_features.float_const_prop ()
    then No_effects, No_coeffects, Strict
    else No_effects, Has_coeffects, Strict
  (* Since Obj.truncate has been deprecated, array_length should have no
     observable effect *)
  | Array_length _ -> No_effects, No_coeffects, Strict
  | Bigarray_length { dimension = _ } ->
    (* This is pretty much a direct access to a field of the bigarray, different
       from reading one of the values actually stored inside the array, hence
       [reading_from_a_block] (i.e. this has the same behaviour as a regular
       Block_load). *)
    reading_from_a_block Mutable
  | Unbox_number _ | Untag_immediate -> No_effects, No_coeffects, Strict
  | Tag_immediate -> No_effects, No_coeffects, Strict
  | Box_number (_, alloc_mode) ->
    (* Ensure boxing operations for numbers are inlined/substituted in to_cmm *)
    let placement : Placement.t =
      if Flambda_features.classic_mode ()
      then
        (* Local allocations have coeffects, to avoid them being moved past a
           begin/end region. Hence, it is not safe to force the allocation to be
           moved, so we cannot use the `Delay` mode for those. *)
        match alloc_mode with Heap -> Delay | Local _ -> Strict
      else Strict
    in
    Only_generative_effects Immutable, coeffects_of_mode alloc_mode, placement
  | Project_function_slot _ | Project_value_slot _ ->
    No_effects, No_coeffects, Delay
  | Is_boxed_float | Is_flat_float_array ->
    (* Tags on heap blocks are immutable. *)
    No_effects, No_coeffects, Strict
  | End_region _ | End_try_region _ ->
    (* These can't be [Only_generative_effects] or the primitives would get
       deleted without regard to prior uses of the region. Instead there are
       special cases in [Simplify_let_expr] and [Expr_builder] for this
       primitive. *)
    Arbitrary_effects, Has_coeffects, Strict
  | Obj_dup ->
    ( Only_generative_effects Mutable (* Mutable is conservative *),
      Has_coeffects,
      Strict )
  | Get_header -> No_effects, No_coeffects, Strict
  | Peek _ ->
    (* For the moment, prevent [Peek] from being moved. *)
    Arbitrary_effects, Has_coeffects, Strict
  | Make_lazy _ -> Only_generative_effects Mutable, No_coeffects, Strict

let unary_classify_for_printing p =
  match p with
  | Duplicate_array _ | Duplicate_block _ | Obj_dup -> Constructive
  | String_length _ | Get_tag -> Destructive
  | Is_int _ | Is_null | Opaque_identity _ | Int_arith _ | Num_conv _
  | Boolean_not | Reinterpret_64_bit_word _ | Float_arith _ ->
    Neither
  | Array_length _ | Bigarray_length _ | Unbox_number _ | Untag_immediate ->
    Destructive
  | Box_number _ | Tag_immediate | Int_as_pointer _ -> Constructive
  | Project_function_slot _ | Project_value_slot _ | Block_load _ -> Destructive
  | Is_boxed_float | Is_flat_float_array -> Neither
  | End_region _ | End_try_region _ -> Neither
  | Get_header -> Neither
  | Peek _ -> Neither
  | Make_lazy _ -> Constructive

let free_names_unary_primitive p =
  match p with
  | Box_number (_, alloc_mode) | Int_as_pointer alloc_mode ->
    Alloc_mode.For_allocations.free_names alloc_mode
  | Project_function_slot { move_from; move_to } ->
    Name_occurrences.add_function_slot_in_projection
      (Name_occurrences.add_function_slot_in_projection Name_occurrences.empty
         move_to Name_mode.normal)
      move_from Name_mode.normal
  | Project_value_slot { value_slot; project_from } ->
    Name_occurrences.add_function_slot_in_projection
      (Name_occurrences.add_value_slot_in_projection Name_occurrences.empty
         value_slot Name_mode.normal)
      project_from Name_mode.normal
  | Block_load _ | Duplicate_array _ | Duplicate_block _ | Is_int _ | Is_null
  | Get_tag | String_length _ | Opaque_identity _ | Int_arith _ | Num_conv _
  | Boolean_not | Reinterpret_64_bit_word _ | Float_arith _ | Array_length _
  | Bigarray_length _ | Unbox_number _ | Untag_immediate | Tag_immediate
  | Is_boxed_float | Is_flat_float_array | End_region _ | End_try_region _
  | Obj_dup | Get_header
  | Peek (_ : Flambda_kind.Standard_int_or_float.t)
  | Make_lazy _ ->
    Name_occurrences.empty

let apply_renaming_unary_primitive p renaming =
  match p with
  | Box_number (kind, alloc_mode) ->
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode' then p else Box_number (kind, alloc_mode')
  | Int_as_pointer alloc_mode ->
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode' then p else Int_as_pointer alloc_mode'
  | Block_load _ | Duplicate_array _ | Duplicate_block _ | Is_int _ | Is_null
  | Get_tag | String_length _ | Opaque_identity _ | Int_arith _ | Num_conv _
  | Boolean_not | Reinterpret_64_bit_word _ | Float_arith _ | Array_length _
  | Bigarray_length _ | Unbox_number _ | Untag_immediate | Tag_immediate
  | Is_boxed_float | Is_flat_float_array | End_region _ | End_try_region _
  | Project_function_slot _ | Project_value_slot _ | Obj_dup | Get_header
  | Peek (_ : Flambda_kind.Standard_int_or_float.t)
  | Make_lazy _ ->
    p

let ids_for_export_unary_primitive p =
  match p with
  | Box_number (_, alloc_mode) | Int_as_pointer alloc_mode ->
    Alloc_mode.For_allocations.ids_for_export alloc_mode
  | Block_load _ | Duplicate_array _ | Duplicate_block _ | Is_int _ | Is_null
  | Get_tag | String_length _ | Opaque_identity _ | Int_arith _ | Num_conv _
  | Boolean_not | Reinterpret_64_bit_word _ | Float_arith _ | Array_length _
  | Bigarray_length _ | Unbox_number _ | Untag_immediate | Tag_immediate
  | Is_boxed_float | Is_flat_float_array | End_region _ | End_try_region _
  | Project_function_slot _ | Project_value_slot _ | Obj_dup | Get_header
  | Peek (_ : Flambda_kind.Standard_int_or_float.t)
  | Make_lazy _ ->
    Ids_for_export.empty

type binary_int_arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor

let print_binary_int_arith_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | Mul -> fprintf ppf "*"
  | Div -> fprintf ppf "/"
  | Mod -> fprintf ppf "mod"
  | And -> fprintf ppf "and"
  | Or -> fprintf ppf "or"
  | Xor -> fprintf ppf "xor"

type int_shift_op =
  | Lsl
  | Lsr
  | Asr

let print_int_shift_op ppf o =
  let fprintf = Format.fprintf in
  match o with
  | Lsl -> fprintf ppf "lsl"
  | Lsr -> fprintf ppf "lsr"
  | Asr -> fprintf ppf "asr"

type binary_float_arith_op =
  | Add
  | Sub
  | Mul
  | Div

let print_binary_float_arith_op ppf width op =
  let fprintf = Format.fprintf in
  match width, op with
  | Float64, Add -> fprintf ppf "+."
  | Float64, Sub -> fprintf ppf "-."
  | Float64, Mul -> fprintf ppf "*."
  | Float64, Div -> fprintf ppf "/."
  | Float32, Add -> fprintf ppf "Float32.+."
  | Float32, Sub -> fprintf ppf "Float32.-."
  | Float32, Mul -> fprintf ppf "Float32.*."
  | Float32, Div -> fprintf ppf "Float32./."

type binary_primitive =
  | Block_set of
      { kind : Block_access_kind.t;
        init : Init_or_assign.t;
        field : Targetint_31_63.t
      }
  | Array_load of Array_kind.t * Array_load_kind.t * Mutability.t
  | String_or_bigstring_load of string_like_value * string_accessor_width
  | Bigarray_load of num_dimensions * Bigarray_kind.t * Bigarray_layout.t
  | Phys_equal of equality_comparison
  | Int_arith of Flambda_kind.Standard_int.t * binary_int_arith_op
  | Int_shift of Flambda_kind.Standard_int.t * int_shift_op
  | Int_comp of
      Flambda_kind.Standard_int.t * signed_or_unsigned comparison_behaviour
  | Float_arith of float_bitwidth * binary_float_arith_op
  | Float_comp of float_bitwidth * unit comparison_behaviour
  | Bigarray_get_alignment of int
  | Atomic_load_field of Block_access_field_kind.t
  | Poke of Flambda_kind.Standard_int_or_float.t

let binary_primitive_eligible_for_cse p =
  match p with
  | Array_load _ | Block_set _ -> false
  | String_or_bigstring_load _ -> false (* CR mshinwell: review *)
  | Bigarray_load _ -> false
  | Bigarray_get_alignment _ -> true
  | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _ -> true
  | Float_arith _ | Float_comp _ ->
    (* We believe that under the IEEE standard it is correct to CSE
       floating-point comparison operations. However we aren't completely sure
       what the situation is with regard to 80-bit precision floating-point
       support on Intel processors (and indeed whether we make use of that). As
       such, we don't CSE these comparisons unless we would also CSE
       floating-point arithmetic operations. See also the comment in
       effects_and_coeffects of unary primitives. *)
    Flambda_features.float_const_prop ()
  | Atomic_load_field (Any_value | Immediate) | Poke _ -> false

let compare_binary_primitive p1 p2 =
  let binary_primitive_numbering p =
    match p with
    | Block_set _ -> 0
    | Array_load _ -> 1
    | String_or_bigstring_load _ -> 2
    | Bigarray_load _ -> 3
    | Phys_equal _ -> 4
    | Int_arith _ -> 5
    | Int_shift _ -> 6
    | Int_comp _ -> 7
    | Float_arith _ -> 8
    | Float_comp _ -> 9
    | Bigarray_get_alignment _ -> 10
    | Atomic_load_field _ -> 11
    | Poke _ -> 12
  in
  match p1, p2 with
  | ( Block_set { kind = kind1; init = init1; field = field1 },
      Block_set { kind = kind2; init = init2; field = field2 } ) ->
    let c = Block_access_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Init_or_assign.compare init1 init2 in
      if c <> 0 then c else Targetint_31_63.compare field1 field2
  | Array_load (kind1, load_kind1, mut1), Array_load (kind2, load_kind2, mut2)
    ->
    let c = Array_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Array_load_kind.compare load_kind1 load_kind2 in
      if c <> 0 then c else Mutability.compare mut1 mut2
  | ( String_or_bigstring_load (string_like1, width1),
      String_or_bigstring_load (string_like2, width2) ) ->
    let c = Stdlib.compare string_like1 string_like2 in
    if c <> 0 then c else Stdlib.compare width1 width2
  | ( Bigarray_load (num_dim1, kind1, layout1),
      Bigarray_load (num_dim2, kind2, layout2) ) ->
    let c = Stdlib.compare num_dim1 num_dim2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare kind1 kind2 in
      if c <> 0 then c else Stdlib.compare layout1 layout2
  | Phys_equal comp1, Phys_equal comp2 -> Stdlib.compare comp1 comp2
  | Int_arith (kind1, op1), Int_arith (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Int_shift (kind1, op1), Int_shift (kind2, op2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Int_comp (kind1, comp_behaviour1), Int_comp (kind2, comp_behaviour2) ->
    let c = K.Standard_int.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare comp_behaviour1 comp_behaviour2
  | Float_arith (width1, op1), Float_arith (width2, op2) ->
    let c = Stdlib.compare width1 width2 in
    if c <> 0 then c else Stdlib.compare op1 op2
  | Float_comp (width1, comp1), Float_comp (width2, comp2) ->
    let c = Stdlib.compare width1 width2 in
    if c <> 0 then c else Stdlib.compare comp1 comp2
  | Bigarray_get_alignment align1, Bigarray_get_alignment align2 ->
    Int.compare align1 align2
  | ( Atomic_load_field block_access_field_kind1,
      Atomic_load_field block_access_field_kind2 ) ->
    Block_access_field_kind.compare block_access_field_kind1
      block_access_field_kind2
  | Poke kind1, Poke kind2 ->
    Flambda_kind.Standard_int_or_float.compare kind1 kind2
  | ( ( Block_set _ | Array_load _ | String_or_bigstring_load _
      | Bigarray_load _ | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _
      | Float_arith _ | Float_comp _ | Bigarray_get_alignment _
      | Atomic_load_field _ | Poke _ ),
      _ ) ->
    Stdlib.compare
      (binary_primitive_numbering p1)
      (binary_primitive_numbering p2)

let equal_binary_primitive p1 p2 = compare_binary_primitive p1 p2 = 0

let print_binary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Block_set { kind; init; field } ->
    fprintf ppf "@[(Block_set@ %a@ %a@ %a)@]" Block_access_kind.print kind
      Init_or_assign.print init Targetint_31_63.print field
  | Array_load (kind, load_kind, mut) ->
    fprintf ppf "@[(Array_load@ %a@ %a@ %a)@]" Array_kind.print kind
      Array_load_kind.print load_kind Mutability.print mut
  | String_or_bigstring_load (string_like, width) ->
    fprintf ppf "@[(String_load %a %a)@]" print_string_like_value string_like
      print_string_accessor_width width
  | Bigarray_load (num_dimensions, kind, layout) ->
    fprintf ppf
      "@[(Bigarray_load (num_dimensions@ %d)@ (kind@ %a)@ (layout@ %a))@]"
      num_dimensions Bigarray_kind.print kind Bigarray_layout.print layout
  | Phys_equal op ->
    Format.fprintf ppf "@[(Phys_equal %a)@]" print_equality_comparison op
  | Int_arith (_k, op) -> print_binary_int_arith_op ppf op
  | Int_shift (_k, op) -> print_int_shift_op ppf op
  | Int_comp (_, comp_behaviour) ->
    print_comparison_and_behaviour print_signed_or_unsigned ppf comp_behaviour
  | Float_arith (width, op) -> print_binary_float_arith_op ppf width op
  | Float_comp (_width, comp_behaviour) ->
    print_comparison_and_behaviour (fun _ppf () -> ()) ppf comp_behaviour;
    fprintf ppf "."
  | Bigarray_get_alignment align ->
    fprintf ppf "@[(Bigarray_get_alignment[%d])@]" align
  | Atomic_load_field block_access_field_kind ->
    Format.fprintf ppf "@[(Atomic_load_field@ %a)@]"
      Block_access_field_kind.print block_access_field_kind
  | Poke kind ->
    fprintf ppf "@[(Poke@ %a)@]"
      Flambda_kind.Standard_int_or_float.print_lowercase kind

let args_kind_of_binary_primitive p =
  match p with
  | Block_set { kind; _ } ->
    block_kind, Block_access_kind.element_kind_for_set kind
  | Array_load _ -> array_kind, array_index_kind
  | String_or_bigstring_load ((String | Bytes), _) ->
    string_or_bytes_kind, string_or_bigstring_index_kind
  | String_or_bigstring_load (Bigstring, _) ->
    bigstring_kind, string_or_bigstring_index_kind
  | Bigarray_load (_, _, _) -> bigarray_kind, bigarray_index_kind
  | Phys_equal _ -> K.value, K.value
  | Int_arith (kind, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Int_shift (kind, _) -> K.Standard_int.to_kind kind, K.naked_immediate
  | Int_comp (kind, _) ->
    let kind = K.Standard_int.to_kind kind in
    kind, kind
  | Float_arith (Float64, _) | Float_comp (Float64, _) ->
    K.naked_float, K.naked_float
  | Float_arith (Float32, _) | Float_comp (Float32, _) ->
    K.naked_float32, K.naked_float32
  | Bigarray_get_alignment _ -> bigstring_kind, K.naked_immediate
  | Atomic_load_field (Any_value | Immediate) -> K.value, K.value
  | Poke kind -> K.naked_nativeint, K.Standard_int_or_float.to_kind kind

let result_kind_of_binary_primitive p : result_kind =
  match p with
  | Block_set _ -> Unit
  | Array_load (_array_kind, array_load_kind, _mut) ->
    Singleton
      (Array_load_kind.kind_of_loaded_value array_load_kind
      |> K.With_subkind.kind)
  | String_or_bigstring_load (_, (Eight | Sixteen)) ->
    Singleton K.naked_immediate
  | String_or_bigstring_load (_, Thirty_two) -> Singleton K.naked_int32
  | String_or_bigstring_load (_, Single) -> Singleton K.naked_float32
  | String_or_bigstring_load (_, Sixty_four) -> Singleton K.naked_int64
  | String_or_bigstring_load (_, One_twenty_eight _) -> Singleton K.naked_vec128
  | String_or_bigstring_load (_, Two_fifty_six _) -> Singleton K.naked_vec256
  | String_or_bigstring_load (_, Five_twelve _) -> Singleton K.naked_vec512
  | Bigarray_load (_, kind, _) -> Singleton (Bigarray_kind.element_kind kind)
  | Int_arith (kind, _) | Int_shift (kind, _) ->
    Singleton (K.Standard_int.to_kind kind)
  | Float_arith (Float64, _) -> Singleton K.naked_float
  | Float_arith (Float32, _) -> Singleton K.naked_float32
  | Phys_equal _ | Int_comp _ | Float_comp _ -> Singleton K.naked_immediate
  | Bigarray_get_alignment _ -> Singleton K.naked_immediate
  | Atomic_load_field (Any_value | Immediate) -> Singleton K.value
  | Poke _ -> Unit

let effects_and_coeffects_of_binary_primitive p : Effects_and_coeffects.t =
  match p with
  | Block_set _ -> writing_to_a_block
  | Array_load (array_kind, _load_kind, mut) ->
    reading_from_an_array array_kind mut
  | Bigarray_load (_, kind, _) -> reading_from_a_bigarray kind
  | String_or_bigstring_load (String, _) ->
    reading_from_a_string_or_bigstring Immutable
  | String_or_bigstring_load ((Bytes | Bigstring), _) ->
    reading_from_a_string_or_bigstring Mutable
  | Phys_equal _ -> No_effects, No_coeffects, Strict
  | Int_arith (_kind, (Add | Sub | Mul | Div | Mod | And | Or | Xor)) ->
    No_effects, No_coeffects, Strict
  | Int_shift _ -> No_effects, No_coeffects, Strict
  | Int_comp _ -> No_effects, No_coeffects, Strict
  | Float_arith (_width, (Add | Sub | Mul | Div)) ->
    (* See comments for Unary Float_arith *)
    if Flambda_features.float_const_prop ()
    then No_effects, No_coeffects, Strict
    else No_effects, Has_coeffects, Strict
  | Float_comp _ ->
    (* See comments for Unary Float_arith *)
    if Flambda_features.float_const_prop ()
    then No_effects, No_coeffects, Strict
    else No_effects, Has_coeffects, Strict
  | Bigarray_get_alignment _ -> No_effects, No_coeffects, Strict
  | Atomic_load_field (Any_value | Immediate) ->
    Arbitrary_effects, Has_coeffects, Strict
  | Poke _ -> Arbitrary_effects, No_coeffects, Strict

let binary_classify_for_printing p =
  match p with
  | Array_load _ -> Destructive
  | Block_set _ | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _
  | Float_arith _ | Float_comp _ | Bigarray_load _ | String_or_bigstring_load _
  | Bigarray_get_alignment _ | Atomic_load_field _ | Poke _ ->
    Neither

let free_names_binary_primitive p =
  match p with
  | Block_set _ | Array_load _ | String_or_bigstring_load _ | Bigarray_load _
  | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _ | Float_arith _
  | Float_comp _ | Bigarray_get_alignment _ | Atomic_load_field _
  | Poke (_ : Flambda_kind.Standard_int_or_float.t) ->
    Name_occurrences.empty

let apply_renaming_binary_primitive p _renaming =
  match p with
  | Block_set _ | Array_load _ | String_or_bigstring_load _ | Bigarray_load _
  | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _ | Float_arith _
  | Float_comp _ | Bigarray_get_alignment _ | Atomic_load_field _
  | Poke (_ : Flambda_kind.Standard_int_or_float.t) ->
    p

let ids_for_export_binary_primitive p =
  match p with
  | Block_set _ | Array_load _ | String_or_bigstring_load _ | Bigarray_load _
  | Phys_equal _ | Int_arith _ | Int_shift _ | Int_comp _ | Float_arith _
  | Float_comp _ | Bigarray_get_alignment _ | Atomic_load_field _
  | Poke (_ : Flambda_kind.Standard_int_or_float.t) ->
    Ids_for_export.empty

type int_atomic_op =
  | Fetch_add
  | Add
  | Sub
  | And
  | Or
  | Xor

let print_int_atomic_op ppf op =
  let fprintf = Format.fprintf in
  match op with
  | Fetch_add -> fprintf ppf "xadd"
  | Add -> fprintf ppf "+"
  | Sub -> fprintf ppf "-"
  | And -> fprintf ppf "and"
  | Or -> fprintf ppf "or"
  | Xor -> fprintf ppf "xor"

type ternary_primitive =
  | Array_set of Array_kind.t * Array_set_kind.t
  | Bytes_or_bigstring_set of bytes_like_value * string_accessor_width
  | Bigarray_set of num_dimensions * Bigarray_kind.t * Bigarray_layout.t
  | Atomic_field_int_arith of int_atomic_op
  | Atomic_set_field of Block_access_field_kind.t
  | Atomic_exchange_field of Block_access_field_kind.t

type quaternary_primitive =
  | Atomic_compare_and_set_field of Block_access_field_kind.t
  | Atomic_compare_exchange_field of
      { atomic_kind : Block_access_field_kind.t;
        args_kind : Block_access_field_kind.t
      }

let ternary_primitive_eligible_for_cse p =
  match p with
  | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
  | Atomic_field_int_arith _
  | Atomic_set_field (Immediate | Any_value)
  | Atomic_exchange_field (Immediate | Any_value) ->
    false

let quaternary_primitive_eligible_for_cse p =
  match p with
  | Atomic_compare_and_set_field (Immediate | Any_value)
  | Atomic_compare_exchange_field
      { atomic_kind = Immediate | Any_value; args_kind = Immediate | Any_value }
    ->
    false

let compare_ternary_primitive p1 p2 =
  let ternary_primitive_numbering p =
    match p with
    | Array_set _ -> 0
    | Bytes_or_bigstring_set _ -> 1
    | Bigarray_set _ -> 2
    | Atomic_field_int_arith _ -> 3
    | Atomic_set_field _ -> 4
    | Atomic_exchange_field _ -> 5
  in
  match p1, p2 with
  | Array_set (kind1, set_kind1), Array_set (kind2, set_kind2) ->
    let c = Array_kind.compare kind1 kind2 in
    if c <> 0 then c else Array_set_kind.compare set_kind1 set_kind2
  | ( Bytes_or_bigstring_set (kind1, width1),
      Bytes_or_bigstring_set (kind2, width2) ) ->
    let c = Stdlib.compare kind1 kind2 in
    if c <> 0 then c else Stdlib.compare width1 width2
  | ( Bigarray_set (num_dims1, kind1, layout1),
      Bigarray_set (num_dims2, kind2, layout2) ) ->
    let c = Stdlib.compare num_dims1 num_dims2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare kind1 kind2 in
      if c <> 0 then c else Stdlib.compare layout1 layout2
  | Atomic_field_int_arith op1, Atomic_field_int_arith op2 ->
    Stdlib.compare op1 op2
  | ( Atomic_set_field block_access_field_kind1,
      Atomic_set_field block_access_field_kind2 ) ->
    Block_access_field_kind.compare block_access_field_kind1
      block_access_field_kind2
  | ( Atomic_exchange_field block_access_field_kind1,
      Atomic_exchange_field block_access_field_kind2 ) ->
    Block_access_field_kind.compare block_access_field_kind1
      block_access_field_kind2
  | ( ( Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
      | Atomic_field_int_arith _ | Atomic_set_field _ | Atomic_exchange_field _
        ),
      _ ) ->
    Stdlib.compare
      (ternary_primitive_numbering p1)
      (ternary_primitive_numbering p2)

let compare_quaternary_primitive p1 p2 =
  let quaternary_primitive_numbering p =
    match p with
    | Atomic_compare_and_set_field _ -> 0
    | Atomic_compare_exchange_field _ -> 1
  in
  match p1, p2 with
  | ( Atomic_compare_and_set_field block_access_field_kind1,
      Atomic_compare_and_set_field block_access_field_kind2 ) ->
    Block_access_field_kind.compare block_access_field_kind1
      block_access_field_kind2
  | ( Atomic_compare_exchange_field
        { atomic_kind = atomic_kind1; args_kind = args_kind1 },
      Atomic_compare_exchange_field
        { atomic_kind = atomic_kind2; args_kind = args_kind2 } ) ->
    let c = Block_access_field_kind.compare atomic_kind1 atomic_kind2 in
    if c <> 0 then c else Block_access_field_kind.compare args_kind1 args_kind2
  | (Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _), _ ->
    Stdlib.compare
      (quaternary_primitive_numbering p1)
      (quaternary_primitive_numbering p2)

let equal_ternary_primitive p1 p2 = compare_ternary_primitive p1 p2 = 0

let equal_quaternary_primitive p1 p2 = compare_quaternary_primitive p1 p2 = 0

let print_ternary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Array_set (kind, set_kind) ->
    fprintf ppf "(Array_set %a %a)" Array_kind.print kind Array_set_kind.print
      set_kind
  | Bytes_or_bigstring_set (kind, string_accessor_width) ->
    fprintf ppf "(Bytes_set %a %a)" print_bytes_like_value kind
      print_string_accessor_width string_accessor_width
  | Bigarray_set (num_dimensions, kind, layout) ->
    fprintf ppf
      "@[(Bigarray_set (num_dimensions@ %d)@ (kind@ %a)@ (layout@ %a))@]"
      num_dimensions Bigarray_kind.print kind Bigarray_layout.print layout
  | Atomic_field_int_arith op ->
    Format.fprintf ppf "@[(Atomic_field_int_arith %a)@]" print_int_atomic_op op
  | Atomic_set_field block_access_field_kind ->
    Format.fprintf ppf "@[(Atomic_set_field@ %a)@]"
      Block_access_field_kind.print block_access_field_kind
  | Atomic_exchange_field block_access_field_kind ->
    fprintf ppf "@[(Atomic_exchange_field@ %a)@]" Block_access_field_kind.print
      block_access_field_kind

let print_quaternary_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Atomic_compare_and_set_field block_access_field_kind ->
    fprintf ppf "@[(Atomic_compare_and_set_field@ %a)@]"
      Block_access_field_kind.print block_access_field_kind
  | Atomic_compare_exchange_field { atomic_kind; args_kind } ->
    fprintf ppf
      "@[(Atomic_compare_exchange_field@ (atomic_kind@ %a)@ (args_kind@ %a))@]"
      Block_access_field_kind.print atomic_kind Block_access_field_kind.print
      args_kind

let args_kind_of_ternary_primitive p =
  match p with
  | Array_set (_kind, array_set_kind) ->
    ( array_kind,
      array_index_kind,
      Array_set_kind.kind_of_new_value array_set_kind |> K.With_subkind.kind )
  | Bytes_or_bigstring_set (Bytes, (Eight | Sixteen)) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_immediate
  | Bytes_or_bigstring_set (Bytes, Thirty_two) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_int32
  | Bytes_or_bigstring_set (Bytes, Single) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_float32
  | Bytes_or_bigstring_set (Bytes, Sixty_four) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_int64
  | Bytes_or_bigstring_set (Bytes, One_twenty_eight _) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_vec128
  | Bytes_or_bigstring_set (Bytes, Two_fifty_six _) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_vec256
  | Bytes_or_bigstring_set (Bytes, Five_twelve _) ->
    string_or_bytes_kind, bytes_or_bigstring_index_kind, K.naked_vec512
  | Bytes_or_bigstring_set (Bigstring, (Eight | Sixteen)) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_immediate
  | Bytes_or_bigstring_set (Bigstring, Thirty_two) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_int32
  | Bytes_or_bigstring_set (Bigstring, Single) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_float32
  | Bytes_or_bigstring_set (Bigstring, Sixty_four) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_int64
  | Bytes_or_bigstring_set (Bigstring, One_twenty_eight _) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_vec128
  | Bytes_or_bigstring_set (Bigstring, Two_fifty_six _) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_vec256
  | Bytes_or_bigstring_set (Bigstring, Five_twelve _) ->
    bigstring_kind, bytes_or_bigstring_index_kind, K.naked_vec512
  | Bigarray_set (_, kind, _) ->
    bigarray_kind, bigarray_index_kind, Bigarray_kind.element_kind kind
  | Atomic_field_int_arith _
  | Atomic_set_field (Immediate | Any_value)
  | Atomic_exchange_field (Immediate | Any_value) ->
    K.value, K.value, K.value

let args_kind_of_quaternary_primitive p =
  match p with
  | Atomic_compare_and_set_field (Immediate | Any_value)
  | Atomic_compare_exchange_field
      { atomic_kind = Immediate | Any_value; args_kind = Immediate | Any_value }
    ->
    K.value, K.value, K.value, K.value

let result_kind_of_ternary_primitive p : result_kind =
  match p with
  | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
  | Atomic_field_int_arith (Add | Sub | And | Or | Xor)
  | Atomic_set_field _ ->
    Unit
  | Atomic_field_int_arith Fetch_add | Atomic_exchange_field _ ->
    Singleton K.value

let result_kind_of_quaternary_primitive p : result_kind =
  match p with
  | Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _ ->
    Singleton K.value

let effects_and_coeffects_of_ternary_primitive p :
    Effects.t * Coeffects.t * Placement.t =
  match p with
  | Array_set _ -> writing_to_an_array
  | Bytes_or_bigstring_set _ -> writing_to_bytes_or_bigstring
  | Bigarray_set (_, kind, _) -> writing_to_a_bigarray kind
  | Atomic_field_int_arith _ | Atomic_set_field _ | Atomic_exchange_field _ ->
    Arbitrary_effects, Has_coeffects, Strict

let effects_and_coeffects_of_quaternary_primitive p :
    Effects.t * Coeffects.t * Placement.t =
  match p with
  | Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _ ->
    Arbitrary_effects, Has_coeffects, Strict

let ternary_classify_for_printing p =
  match p with
  | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
  | Atomic_field_int_arith _ | Atomic_set_field _ | Atomic_exchange_field _ ->
    Neither

let quaternary_classify_for_printing p =
  match p with
  | Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _ -> Neither

let free_names_ternary_primitive p =
  match p with
  | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
  | Atomic_field_int_arith _ | Atomic_set_field _ | Atomic_exchange_field _ ->
    Name_occurrences.empty

let free_names_quaternary_primitive p =
  match p with
  | Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _ ->
    Name_occurrences.empty

let apply_renaming_ternary_primitive p _ =
  match p with
  | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
  | Atomic_field_int_arith _ | Atomic_set_field _ | Atomic_exchange_field _ ->
    p

let apply_renaming_quaternary_primitive p _ =
  match p with
  | Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _ -> p

let ids_for_export_ternary_primitive p =
  match p with
  | Array_set _ | Bytes_or_bigstring_set _ | Bigarray_set _
  | Atomic_field_int_arith _ | Atomic_set_field _ | Atomic_exchange_field _ ->
    Ids_for_export.empty

let ids_for_export_quaternary_primitive p =
  match p with
  | Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _ ->
    Ids_for_export.empty

type variadic_primitive =
  | Begin_region of { ghost : bool }
  | Begin_try_region of { ghost : bool }
  | Make_block of Block_kind.t * Mutability.t * Alloc_mode.For_allocations.t
  | Make_array of Array_kind.t * Mutability.t * Alloc_mode.For_allocations.t

let variadic_primitive_eligible_for_cse p ~args =
  match p with
  | Begin_region _ | Begin_try_region _ -> false
  | Make_block (_, _, Local _) | Make_array (_, _, Local _) -> false
  | Make_block (_, Mutable, _) | Make_array (_, Mutable, _) -> false
  | Make_block (_, Immutable_unique, _) | Make_array (_, Immutable_unique, _) ->
    false
  | Make_block (_, Immutable, Heap) | Make_array (_, Immutable, Heap) ->
    (* See comment in [unary_primitive_eligible_for_cse], above, on [Box_number]
       case. *)
    List.exists (fun arg -> Simple.is_var arg) args

let compare_variadic_primitive p1 p2 =
  match p1, p2 with
  | Begin_region { ghost = ghost1 }, Begin_region { ghost = ghost2 } ->
    Bool.compare ghost1 ghost2
  | Begin_try_region { ghost = ghost1 }, Begin_try_region { ghost = ghost2 } ->
    Bool.compare ghost1 ghost2
  | Make_block (kind1, mut1, alloc_mode1), Make_block (kind2, mut2, alloc_mode2)
    ->
    let c = Block_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare mut1 mut2 in
      if c <> 0
      then c
      else Alloc_mode.For_allocations.compare alloc_mode1 alloc_mode2
  | Make_array (kind1, mut1, alloc_mode1), Make_array (kind2, mut2, alloc_mode2)
    ->
    let c = Array_kind.compare kind1 kind2 in
    if c <> 0
    then c
    else
      let c = Stdlib.compare mut1 mut2 in
      if c <> 0
      then c
      else Alloc_mode.For_allocations.compare alloc_mode1 alloc_mode2
  | Begin_region _, (Begin_try_region _ | Make_block _ | Make_array _) -> -1
  | Begin_try_region _, (Make_block _ | Make_array _) -> -1
  | Begin_try_region _, Begin_region _ -> 1
  | Make_block _, Make_array _ -> -1
  | Make_block _, (Begin_region _ | Begin_try_region _) -> 1
  | Make_array _, (Begin_region _ | Begin_try_region _ | Make_block _) -> 1

let equal_variadic_primitive p1 p2 = compare_variadic_primitive p1 p2 = 0

let print_variadic_primitive ppf p =
  let fprintf = Format.fprintf in
  match p with
  | Begin_region { ghost } ->
    Format.fprintf ppf "Begin_region%s" (if ghost then "_ghost" else "")
  | Begin_try_region { ghost } ->
    Format.fprintf ppf "Begin_try_region%s" (if ghost then "_ghost" else "")
  | Make_block (kind, mut, alloc_mode) ->
    fprintf ppf "@[<hov 1>(Make_block@ %a@ %a@ %a)@]" Block_kind.print kind
      Mutability.print mut Alloc_mode.For_allocations.print alloc_mode
  | Make_array (kind, mut, alloc_mode) ->
    fprintf ppf "@[<hov 1>(Make_array@ %a@ %a@ %a)@]" Array_kind.print kind
      Mutability.print mut Alloc_mode.For_allocations.print alloc_mode

let args_kind_of_variadic_primitive p : arg_kinds =
  match p with
  | Begin_region _ | Begin_try_region _ -> Variadic_zero_or_one K.region
  | Make_block (Values _, _, _) -> Variadic_all_of_kind K.value
  | Make_block (Naked_floats, _, _) -> Variadic_all_of_kind K.naked_float
  | Make_block (Mixed (_tag, shape), _, _) -> Variadic_mixed shape
  | Make_array (kind, _, _) ->
    Variadic_unboxed_product (Array_kind.element_kinds_for_primitive kind)

let result_kind_of_variadic_primitive p : result_kind =
  match p with
  | Begin_region _ | Begin_try_region _ -> Singleton K.region
  | Make_block _ | Make_array _ -> Singleton K.value

let effects_and_coeffects_of_begin_region : Effects_and_coeffects.t =
  (* Ensure these don't get moved, but allow them to be deleted. *)
  Only_generative_effects Mutable, Has_coeffects, Strict

let effects_and_coeffects_of_variadic_primitive p =
  match p with
  | Begin_region _ | Begin_try_region _ -> effects_and_coeffects_of_begin_region
  | Make_block (_, mut, alloc_mode) | Make_array (_, mut, alloc_mode) ->
    let coeffects : Coeffects.t =
      match alloc_mode with
      | Heap -> Coeffects.No_coeffects
      | Local _ -> Coeffects.Has_coeffects
    in
    Effects.Only_generative_effects mut, coeffects, Placement.Strict

let variadic_classify_for_printing p =
  match p with
  | Begin_region _ | Begin_try_region _ -> Neither
  | Make_block _ | Make_array _ -> Constructive

let free_names_variadic_primitive p =
  match p with
  | Begin_region _ | Begin_try_region _ -> Name_occurrences.empty
  | Make_block (_kind, _mut, alloc_mode) ->
    Alloc_mode.For_allocations.free_names alloc_mode
  | Make_array (_kind, _mut, alloc_mode) ->
    Alloc_mode.For_allocations.free_names alloc_mode

let apply_renaming_variadic_primitive p renaming =
  match p with
  | Begin_region _ | Begin_try_region _ -> p
  | Make_block (kind, mut, alloc_mode) ->
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode' then p else Make_block (kind, mut, alloc_mode')
  | Make_array (kind, mut, alloc_mode) ->
    let alloc_mode' =
      Alloc_mode.For_allocations.apply_renaming alloc_mode renaming
    in
    if alloc_mode == alloc_mode' then p else Make_array (kind, mut, alloc_mode')

let ids_for_export_variadic_primitive p =
  match p with
  | Begin_region _ | Begin_try_region _ -> Ids_for_export.empty
  | Make_block (_kind, _mut, alloc_mode) ->
    Alloc_mode.For_allocations.ids_for_export alloc_mode
  | Make_array (_kind, _mut, alloc_mode) ->
    Alloc_mode.For_allocations.ids_for_export alloc_mode

type t =
  | Nullary of nullary_primitive
  | Unary of unary_primitive * Simple.t
  | Binary of binary_primitive * Simple.t * Simple.t
  | Ternary of ternary_primitive * Simple.t * Simple.t * Simple.t
  | Quaternary of
      quaternary_primitive * Simple.t * Simple.t * Simple.t * Simple.t
  | Variadic of variadic_primitive * Simple.t list

type primitive_application = t

let classify_for_printing t =
  match t with
  | Nullary prim -> nullary_classify_for_printing prim
  | Unary (prim, _) -> unary_classify_for_printing prim
  | Binary (prim, _, _) -> binary_classify_for_printing prim
  | Ternary (prim, _, _, _) -> ternary_classify_for_printing prim
  | Quaternary (prim, _, _, _, _) -> quaternary_classify_for_printing prim
  | Variadic (prim, _) -> variadic_classify_for_printing prim

let compare_primitive_application ~compare_simple t1 t2 =
  if t1 == t2
  then 0
  else
    let numbering t =
      match t with
      | Nullary _ -> 0
      | Unary _ -> 1
      | Binary _ -> 2
      | Ternary _ -> 3
      | Quaternary _ -> 4
      | Variadic _ -> 5
    in
    match t1, t2 with
    | Nullary p, Nullary p' -> compare_nullary_primitive p p'
    | Unary (p, s1), Unary (p', s1') ->
      let c = compare_unary_primitive p p' in
      if c <> 0 then c else compare_simple s1 s1'
    | Binary (p, s1, s2), Binary (p', s1', s2') ->
      let c = compare_binary_primitive p p' in
      if c <> 0
      then c
      else
        let c = compare_simple s1 s1' in
        if c <> 0 then c else compare_simple s2 s2'
    | Ternary (p, s1, s2, s3), Ternary (p', s1', s2', s3') ->
      let c = compare_ternary_primitive p p' in
      if c <> 0
      then c
      else
        let c = compare_simple s1 s1' in
        if c <> 0
        then c
        else
          let c = compare_simple s2 s2' in
          if c <> 0 then c else compare_simple s3 s3'
    | Quaternary (p, s1, s2, s3, s4), Quaternary (p', s1', s2', s3', s4') ->
      let c = compare_quaternary_primitive p p' in
      if c <> 0
      then c
      else
        let c = Simple.compare s1 s1' in
        if c <> 0
        then c
        else
          let c = Simple.compare s2 s2' in
          if c <> 0
          then c
          else
            let c = Simple.compare s3 s3' in
            if c <> 0 then c else Simple.compare s4 s4'
    | Variadic (p, s), Variadic (p', s') ->
      let c = compare_variadic_primitive p p' in
      if c <> 0 then c else List.compare compare_simple s s'
    | ( (Nullary _ | Unary _ | Binary _ | Ternary _ | Quaternary _ | Variadic _),
        _ ) ->
      Stdlib.compare (numbering t1) (numbering t2)

include Container_types.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    compare_primitive_application ~compare_simple:Simple.compare t1 t2

  let equal t1 t2 = compare t1 t2 = 0

  let hash _t = Misc.fatal_error "Not implemented"

  let [@ocamlformat "disable"] print ppf t =
    let colour =
      match classify_for_printing t with
      | Constructive -> Flambda_colours.prim_constructive
      | Destructive -> Flambda_colours.prim_destructive
      | Neither -> Flambda_colours.prim_neither
    in
    match t with
    | Nullary prim ->
      Format.fprintf ppf "@[<hov 1>%t%a%t@]"
        colour
        print_nullary_primitive prim
        Flambda_colours.pop
    | Unary (prim, v0) ->
      Format.fprintf ppf "@[<hov 1>(%t%a%t@ %a)@]"
        colour
        print_unary_primitive prim
        Flambda_colours.pop
        Simple.print v0
    | Binary (prim, v0, v1) ->
      Format.fprintf ppf "@[<hov 1>(%t%a%t@ %a@ %a)@]"
        colour
        print_binary_primitive prim
        Flambda_colours.pop
        Simple.print v0
        Simple.print v1
    | Ternary (prim, v0, v1, v2) ->
      Format.fprintf ppf "@[<hov 1>(%t%a%t@ %a@ %a@ %a)@]"
        colour
        print_ternary_primitive prim
        Flambda_colours.pop
        Simple.print v0
        Simple.print v1
        Simple.print v2
    | Quaternary (prim, v0, v1, v2, v3) ->
      Format.fprintf ppf "@[<hov 1>(%t%a%t@ %a@ %a@ %a@ %a)@]"
        colour
        print_quaternary_primitive prim
        Flambda_colours.pop
        Simple.print v0
        Simple.print v1
        Simple.print v2
        Simple.print v3
    | Variadic (prim, vs) ->
      Format.fprintf ppf "@[<hov 1>(%t%a%t@ %a)@]"
        colour
        print_variadic_primitive prim
        Flambda_colours.pop
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Simple.print) vs
end)

let equal t1 t2 = compare t1 t2 = 0

let free_names t =
  match t with
  | Nullary
      ( Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _
      | Dls_get | Poll | Cpu_relax ) ->
    Name_occurrences.empty
  | Unary (prim, x0) ->
    Name_occurrences.union
      (free_names_unary_primitive prim)
      (Simple.free_names x0)
  | Binary (prim, x0, x1) ->
    Name_occurrences.union_list
      [ free_names_binary_primitive prim;
        Simple.free_names x0;
        Simple.free_names x1 ]
  | Ternary (prim, x0, x1, x2) ->
    Name_occurrences.union_list
      [ free_names_ternary_primitive prim;
        Simple.free_names x0;
        Simple.free_names x1;
        Simple.free_names x2 ]
  | Quaternary (prim, x0, x1, x2, x3) ->
    Name_occurrences.union_list
      [ free_names_quaternary_primitive prim;
        Simple.free_names x0;
        Simple.free_names x1;
        Simple.free_names x2;
        Simple.free_names x3 ]
  | Variadic (prim, xs) ->
    Name_occurrences.union
      (free_names_variadic_primitive prim)
      (Simple.List.free_names xs)

let apply_renaming t renaming =
  let apply simple = Simple.apply_renaming simple renaming in
  match t with
  | Nullary
      ( Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _
      | Dls_get | Poll | Cpu_relax ) ->
    t
  | Unary (prim, x0) ->
    let prim' = apply_renaming_unary_primitive prim renaming in
    let x0' = apply x0 in
    if prim == prim' && x0' == x0 then t else Unary (prim', x0')
  | Binary (prim, x0, x1) ->
    let prim' = apply_renaming_binary_primitive prim renaming in
    let x0' = apply x0 in
    let x1' = apply x1 in
    if prim == prim' && x0' == x0 && x1' == x1
    then t
    else Binary (prim', x0', x1')
  | Ternary (prim, x0, x1, x2) ->
    let prim' = apply_renaming_ternary_primitive prim renaming in
    let x0' = apply x0 in
    let x1' = apply x1 in
    let x2' = apply x2 in
    if prim == prim' && x0' == x0 && x1' == x1 && x2' == x2
    then t
    else Ternary (prim', x0', x1', x2')
  | Quaternary (prim, x0, x1, x2, x3) ->
    let prim' = apply_renaming_quaternary_primitive prim renaming in
    let x0' = apply x0 in
    let x1' = apply x1 in
    let x2' = apply x2 in
    let x3' = apply x3 in
    if prim == prim' && x0' == x0 && x1' == x1 && x2' == x2 && x3' == x3
    then t
    else Quaternary (prim', x0', x1', x2', x3')
  | Variadic (prim, xs) ->
    let prim' = apply_renaming_variadic_primitive prim renaming in
    let xs' = Simple.List.apply_renaming xs renaming in
    if prim == prim' && xs' == xs then t else Variadic (prim', xs')

let ids_for_export t =
  match t with
  | Nullary
      ( Invalid _ | Optimised_out _ | Probe_is_enabled _ | Enter_inlined_apply _
      | Dls_get | Poll | Cpu_relax ) ->
    Ids_for_export.empty
  | Unary (prim, x0) ->
    Ids_for_export.union
      (ids_for_export_unary_primitive prim)
      (Ids_for_export.from_simple x0)
  | Binary (prim, x0, x1) ->
    Ids_for_export.union
      (ids_for_export_binary_primitive prim)
      (Ids_for_export.add_simple (Ids_for_export.from_simple x0) x1)
  | Ternary (prim, x0, x1, x2) ->
    Ids_for_export.union
      (ids_for_export_ternary_primitive prim)
      (Ids_for_export.add_simple
         (Ids_for_export.add_simple (Ids_for_export.from_simple x0) x1)
         x2)
  | Quaternary (prim, x0, x1, x2, x3) ->
    Ids_for_export.union
      (ids_for_export_quaternary_primitive prim)
      (Ids_for_export.add_simple
         (Ids_for_export.add_simple
            (Ids_for_export.add_simple (Ids_for_export.from_simple x0) x1)
            x2)
         x3)
  | Variadic (prim, xs) ->
    Ids_for_export.union
      (ids_for_export_variadic_primitive prim)
      (List.fold_left Ids_for_export.add_simple Ids_for_export.empty xs)

let args t =
  match t with
  | Nullary _ -> []
  | Unary (_, x0) -> [x0]
  | Binary (_, x0, x1) -> [x0; x1]
  | Ternary (_, x0, x1, x2) -> [x0; x1; x2]
  | Quaternary (_, x0, x1, x2, x3) -> [x0; x1; x2; x3]
  | Variadic (_, xs) -> xs

let map_args f t =
  match t with
  | Nullary _ -> t
  | Unary (p, x0) ->
    let x0' = f x0 in
    if x0 == x0' then t else Unary (p, x0')
  | Binary (p, x0, x1) ->
    let x0' = f x0 in
    let x1' = f x1 in
    if x0 == x0' && x1 == x1' then t else Binary (p, x0', x1')
  | Ternary (p, x0, x1, x2) ->
    let x0' = f x0 in
    let x1' = f x1 in
    let x2' = f x2 in
    if x0 == x0' && x1 == x1' && x2 == x2' then t else Ternary (p, x0', x1', x2')
  | Quaternary (p, x0, x1, x2, x3) ->
    let x0' = f x0 in
    let x1' = f x1 in
    let x2' = f x2 in
    let x3' = f x3 in
    if x0 == x0' && x1 == x1' && x2 == x2' && x3 == x3'
    then t
    else Quaternary (p, x0', x1', x2', x3')
  | Variadic (p, xs) ->
    let xs' = Misc.Stdlib.List.map_sharing f xs in
    if xs == xs' then t else Variadic (p, xs')

let result_kind (t : t) =
  match t with
  | Nullary prim -> result_kind_of_nullary_primitive prim
  | Unary (prim, _) -> result_kind_of_unary_primitive prim
  | Binary (prim, _, _) -> result_kind_of_binary_primitive prim
  | Ternary (prim, _, _, _) -> result_kind_of_ternary_primitive prim
  | Quaternary (prim, _, _, _, _) -> result_kind_of_quaternary_primitive prim
  | Variadic (prim, _) -> result_kind_of_variadic_primitive prim

let result_kind' t =
  match result_kind t with Singleton kind -> kind | Unit -> K.value

let result_kind_of_nullary_primitive' t =
  match result_kind_of_nullary_primitive t with
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_unary_primitive' t =
  match result_kind_of_unary_primitive t with
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_binary_primitive' t =
  match result_kind_of_binary_primitive t with
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_ternary_primitive' t =
  match result_kind_of_ternary_primitive t with
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_quaternary_primitive' t =
  match result_kind_of_quaternary_primitive t with
  | Singleton kind -> kind
  | Unit -> K.value

let result_kind_of_variadic_primitive' t =
  match result_kind_of_variadic_primitive t with
  | Singleton kind -> kind
  | Unit -> K.value

let effects_and_coeffects (t : t) =
  match t with
  | Nullary prim -> effects_and_coeffects_of_nullary_primitive prim
  | Unary (prim, _) -> effects_and_coeffects_of_unary_primitive prim
  | Binary (prim, _, _) -> effects_and_coeffects_of_binary_primitive prim
  | Ternary (prim, _, _, _) -> effects_and_coeffects_of_ternary_primitive prim
  | Quaternary (prim, _, _, _, _) ->
    effects_and_coeffects_of_quaternary_primitive prim
  | Variadic (prim, _) -> effects_and_coeffects_of_variadic_primitive prim

let no_effects_or_coeffects t =
  match effects_and_coeffects t with
  | No_effects, No_coeffects, _ -> true
  | ( (No_effects | Only_generative_effects _ | Arbitrary_effects),
      (No_coeffects | Has_coeffects),
      _ ) ->
    false

let at_most_generative_effects t =
  match effects_and_coeffects t with
  | (No_effects | Only_generative_effects _), _, _ -> true
  | Arbitrary_effects, _, _ -> false

let only_generative_effects t =
  match effects_and_coeffects t with
  | Only_generative_effects _, _, _ -> true
  | (No_effects | Arbitrary_effects), _, _ -> false

module Eligible_for_cse : sig
  type t

  include Contains_names.S with type t := t

  val create : primitive_application -> t option

  val create_exn : primitive_application -> t

  val create_is_int : variant_only:bool -> immediate_or_block:Name.t -> t

  val create_get_tag : block:Name.t -> t

  val eligible : primitive_application -> bool

  val to_primitive : t -> primitive_application

  val fold_args : t -> init:'a -> f:('a -> Simple.t -> 'a * Simple.t) -> 'a * t

  val filter_map_args : t -> f:(Simple.t -> Simple.t option) -> t option

  include Container_types.S with type t := t
end = struct
  type t = primitive_application

  let create t =
    (* CR mshinwell: Possible way of handling commutativity: for eligible
       primitives, sort the arguments here *)
    let prim_eligible =
      match t with
      | Nullary prim -> nullary_primitive_eligible_for_cse prim
      | Unary (prim, arg) -> unary_primitive_eligible_for_cse prim ~arg
      | Binary (prim, _, _) -> binary_primitive_eligible_for_cse prim
      | Ternary (prim, _, _, _) -> ternary_primitive_eligible_for_cse prim
      | Quaternary (prim, _, _, _, _) ->
        quaternary_primitive_eligible_for_cse prim
      | Variadic (prim, args) -> variadic_primitive_eligible_for_cse prim ~args
    in
    let eligible = prim_eligible && List.exists Simple.is_var (args t) in
    let effects_and_coeffects_ok =
      match effects_and_coeffects t with
      | No_effects, No_coeffects, _ -> true
      | Only_generative_effects Immutable, No_coeffects, _ ->
        (* Allow constructions of immutable blocks to be shared. *)
        true
      | ( ( No_effects
          | Only_generative_effects (Immutable | Immutable_unique | Mutable)
          | Arbitrary_effects ),
          (No_coeffects | Has_coeffects),
          _ ) ->
        false
    in
    if not ((not eligible) || effects_and_coeffects_ok)
    then Misc.fatal_errorf "Eligible_for_cse.create inconsistency: %a" print t;
    if not eligible
    then None
    else
      let t =
        match t with
        | Nullary _ | Unary _ | Binary _ | Ternary _ | Quaternary _ -> t
        | Variadic (prim, args) ->
          (* We can't recover subkind information from Flambda types, but
             sometimes we want to add CSE equations for [Make_block] and
             [Make_array] irrespective of the _sub_kinds. As such we ignore the
             subkinds here by erasing them. *)
          let prim =
            match prim with
            | Begin_region _ | Begin_try_region _ -> assert false
            | Make_block (Values (tag, kinds), mutability, alloc_mode) ->
              let kinds = List.map K.With_subkind.erase_subkind kinds in
              Make_block (Values (tag, kinds), mutability, alloc_mode)
            | Make_block ((Naked_floats | Mixed _), _, _) | Make_array _ -> prim
          in
          Variadic (prim, args)
      in
      Some t

  let create_exn prim =
    match create prim with
    | Some t -> t
    | None -> Misc.fatal_errorf "Primitive %a not eligible for CSE" print prim

  let create_is_int ~variant_only ~immediate_or_block =
    Unary (Is_int { variant_only }, Simple.name immediate_or_block)

  let create_get_tag ~block = Unary (Get_tag, Simple.name block)

  let eligible t = match create t with None -> false | Some _ -> true

  let to_primitive t = t

  let fold_args t ~init ~f =
    match t with
    | Nullary _ -> init, t
    | Unary (prim, arg) ->
      let acc, arg = f init arg in
      acc, Unary (prim, arg)
    | Binary (prim, arg1, arg2) ->
      let acc, arg1 = f init arg1 in
      let acc, arg2 = f acc arg2 in
      acc, Binary (prim, arg1, arg2)
    | Ternary (prim, arg1, arg2, arg3) ->
      let acc, arg1 = f init arg1 in
      let acc, arg2 = f acc arg2 in
      let acc, arg3 = f acc arg3 in
      acc, Ternary (prim, arg1, arg2, arg3)
    | Quaternary (prim, arg1, arg2, arg3, arg4) ->
      let acc, arg1 = f init arg1 in
      let acc, arg2 = f acc arg2 in
      let acc, arg3 = f acc arg3 in
      let acc, arg4 = f acc arg4 in
      acc, Quaternary (prim, arg1, arg2, arg3, arg4)
    | Variadic (prim, args) ->
      let acc, args =
        List.fold_left
          (fun (acc, args) arg ->
            let acc, arg = f acc arg in
            acc, arg :: args)
          (init, []) args
      in
      acc, Variadic (prim, List.rev args)

  let filter_map_args t ~f =
    match t with
    | Nullary _ -> Some t
    | Unary (prim, arg) -> (
      match f arg with
      | None -> None
      | Some arg' -> if arg == arg' then Some t else Some (Unary (prim, arg')))
    | Binary (prim, arg1, arg2) -> (
      match f arg1 with
      | None -> None
      | Some arg1' -> (
        match f arg2 with
        | None -> None
        | Some arg2' ->
          if arg1 == arg1' && arg2 == arg2'
          then Some t
          else Some (Binary (prim, arg1', arg2'))))
    | Ternary (prim, arg1, arg2, arg3) -> (
      match f arg1 with
      | None -> None
      | Some arg1' -> (
        match f arg2 with
        | None -> None
        | Some arg2' -> (
          match f arg3 with
          | None -> None
          | Some arg3' ->
            if arg1 == arg1' && arg2 == arg2' && arg3 == arg3'
            then Some t
            else Some (Ternary (prim, arg1', arg2', arg3')))))
    | Quaternary (prim, arg1, arg2, arg3, arg4) -> (
      match f arg1 with
      | None -> None
      | Some arg1' -> (
        match f arg2 with
        | None -> None
        | Some arg2' -> (
          match f arg3 with
          | None -> None
          | Some arg3' -> (
            match f arg4 with
            | None -> None
            | Some arg4' ->
              if arg1 == arg1' && arg2 == arg2' && arg3 == arg3'
                 && arg4 == arg4'
              then Some t
              else Some (Quaternary (prim, arg1', arg2', arg3', arg4'))))))
    | Variadic (prim, args) ->
      let args' = List.filter_map f args in
      if List.compare_lengths args args' = 0
      then
        if List.for_all2 ( == ) args args'
        then Some t
        else Some (Variadic (prim, args'))
      else None

  let free_names = free_names

  let apply_renaming = apply_renaming

  include Container_types.Make (struct
    type nonrec t = t

    let compare = compare

    let equal = equal

    let hash = hash

    let print = print
  end)

  let equal t1 t2 = compare t1 t2 = 0
end

let args t =
  match t with
  | Nullary _ -> []
  | Unary (_, arg) -> [arg]
  | Binary (_, arg1, arg2) -> [arg1; arg2]
  | Ternary (_, arg1, arg2, arg3) -> [arg1; arg2; arg3]
  | Quaternary (_, arg1, arg2, arg3, arg4) -> [arg1; arg2; arg3; arg4]
  | Variadic (_, args) -> args

module Without_args = struct
  type t =
    | Nullary of nullary_primitive
    | Unary of unary_primitive
    | Binary of binary_primitive
    | Ternary of ternary_primitive
    | Quaternary of quaternary_primitive
    | Variadic of variadic_primitive

  let [@ocamlformat "disable"] print ppf (t : t) =
    match t with
    | Nullary prim -> print_nullary_primitive ppf prim
    | Unary prim -> print_unary_primitive ppf prim
    | Binary prim -> print_binary_primitive ppf prim
    | Ternary prim -> print_ternary_primitive ppf prim
    | Quaternary prim -> print_quaternary_primitive ppf prim
    | Variadic prim -> print_variadic_primitive ppf prim

  let effects_and_coeffects (t : t) =
    match t with
    | Nullary prim -> effects_and_coeffects_of_nullary_primitive prim
    | Unary prim -> effects_and_coeffects_of_unary_primitive prim
    | Binary prim -> effects_and_coeffects_of_binary_primitive prim
    | Ternary prim -> effects_and_coeffects_of_ternary_primitive prim
    | Quaternary prim -> effects_and_coeffects_of_quaternary_primitive prim
    | Variadic prim -> effects_and_coeffects_of_variadic_primitive prim
end

let is_begin_or_end_region t =
  match t with
  | Variadic ((Begin_region _ | Begin_try_region _), _)
  | Unary ((End_region _ | End_try_region _), _) ->
    true
  | _ -> false
  [@@ocaml.warning "-fragile-match"]

let is_begin_region t =
  match t with
  | Variadic ((Begin_region _ | Begin_try_region _), _) -> true
  | _ -> false
  [@@ocaml.warning "-fragile-match"]

let is_end_region t =
  match t with
  | Unary (End_region _, region) -> (
    match Simple.must_be_var region with
    | Some (region, _coercion) -> Some region
    | None ->
      Misc.fatal_errorf "End_region with non-Variable argument:@ %a"
        Simple.print region)
  | _ -> None
  [@@ocaml.warning "-fragile-match"]
