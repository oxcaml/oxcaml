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
