(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Int_ids.Const

let of_descr (descr : Descr.t) =
  match descr with
  | Naked_immediate i -> naked_immediate i
  | Tagged_immediate i -> tagged_immediate i
  | Naked_float32 f -> naked_float32 f
  | Naked_float f -> naked_float f
  | Naked_int8 i -> naked_int8 i
  | Naked_int16 i -> naked_int16 i
  | Naked_int32 i -> naked_int32 i
  | Naked_int64 i -> naked_int64 i
  | Naked_nativeint i -> naked_nativeint i
  | Naked_vec128 v -> naked_vec128 v
  | Naked_vec256 v -> naked_vec256 v
  | Naked_vec512 v -> naked_vec512 v
  | Null -> const_null

let is_null t = equal t const_null

let is_naked_immediate t =
  match descr t with
  | Naked_immediate i -> Some i
  | Tagged_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int8 _
  | Naked_int16 _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ | Null ->
    None

let is_tagged_immediate t =
  match descr t with
  | Tagged_immediate i -> Some i
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int8 _
  | Naked_int16 _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ | Null ->
    None

let kind t =
  match descr t with
  | Tagged_immediate _ | Null -> Flambda_kind.value
  | Naked_immediate _ -> Flambda_kind.naked_immediate
  | Naked_float _ -> Flambda_kind.naked_float
  | Naked_float32 _ -> Flambda_kind.naked_float32
  | Naked_int8 _ -> Flambda_kind.naked_int8
  | Naked_int16 _ -> Flambda_kind.naked_int16
  | Naked_int32 _ -> Flambda_kind.naked_int32
  | Naked_int64 _ -> Flambda_kind.naked_int64
  | Naked_nativeint _ -> Flambda_kind.naked_nativeint
  | Naked_vec128 _ -> Flambda_kind.naked_vec128
  | Naked_vec256 _ -> Flambda_kind.naked_vec256
  | Naked_vec512 _ -> Flambda_kind.naked_vec512

let of_int_of_kind (kind : Flambda_kind.t) i =
  match kind with
  | Value -> tagged_immediate (Targetint_31_63.of_int i)
  | Naked_number Naked_float ->
    naked_float (Numeric_types.Float_by_bit_pattern.create (float_of_int i))
  | Naked_number Naked_float32 ->
    naked_float32 (Numeric_types.Float32_by_bit_pattern.create (float_of_int i))
  | Naked_number Naked_immediate -> naked_immediate (Targetint_31_63.of_int i)
  | Naked_number Naked_int8 -> naked_int8 (Numeric_types.Int8.of_int i)
  | Naked_number Naked_int16 -> naked_int16 (Numeric_types.Int16.of_int i)
  | Naked_number Naked_int32 -> naked_int32 (Int32.of_int i)
  | Naked_number Naked_int64 -> naked_int64 (Int64.of_int i)
  | Naked_number Naked_nativeint -> naked_nativeint (Targetint_32_64.of_int i)
  | Naked_number Naked_vec128 ->
    let i = Int64.of_int i in
    naked_vec128
      (Vector_types.Vec128.Bit_pattern.of_bits { word0 = i; word1 = i })
  | Naked_number Naked_vec256 ->
    let i = Int64.of_int i in
    naked_vec256
      (Vector_types.Vec256.Bit_pattern.of_bits
         { word0 = i; word1 = i; word2 = i; word3 = i })
  | Naked_number Naked_vec512 ->
    let i = Int64.of_int i in
    naked_vec512
      (Vector_types.Vec512.Bit_pattern.of_bits
         { word0 = i;
           word1 = i;
           word2 = i;
           word3 = i;
           word4 = i;
           word5 = i;
           word6 = i;
           word7 = i
         })
  | Region | Rec_info ->
    Misc.fatal_errorf "Invalid kind %a" Flambda_kind.print kind
