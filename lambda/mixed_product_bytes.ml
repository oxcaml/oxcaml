(******************************************************************************
 *                             flambda-backend                                *
 *                          Ryan Tjoa, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

module Byte_count = struct
  type t = int

  let on_64_bit_arch t = t

  let zero = 0

  let is_zero = Int.equal 0

  let add = Int.add
end

type t =
  { value : int;
    flat : int
  }

let zero = { value = 0; flat = 0 }

let add { value; flat } { value = value'; flat = flat' } =
  { value = value + value'; flat = flat + flat' }

let rec count (el : _ Lambda.mixed_block_element) : t =
  match el with
  | Value _ -> { value = 8; flat = 0 }
  | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64 | Word
  | Untagged_immediate ->
    (* In a record, bits8/bits16/bits32/float32 aren't packed tightly *)
    { value = 0; flat = 8 }
  | Vec128 -> { value = 0; flat = 16 }
  | Vec256 -> { value = 0; flat = 32 }
  | Vec512 -> { value = 0; flat = 64 }
  | Product layouts ->
    Array.fold_left (fun cts l -> add cts (count l)) zero layouts

let has_value_and_flat { value; flat } = value > 0 && flat > 0

module Wrt_path = struct
  type nonrec t =
    { here : t;
      left : t;
      right : t
    }

  let zero = { here = zero; left = zero; right = zero }

  let add { here; left; right } { here = here'; left = left'; right = right' } =
    { here = add here here'; left = add left left'; right = add right right' }

  let rec count_wrt_path (el : _ Lambda.mixed_block_element) path =
    match path with
    | [] -> { zero with here = count el }
    | i :: path_rest -> (
      match el with
      | Product shape -> count_shape_wrt_path shape i path_rest
      | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32
      | Bits64 | Word | Vec128 | Vec256 | Vec512 | Untagged_immediate ->
        Misc.fatal_error "Mixed_product_bytes_wrt_path: bad mixed block path")

  and count_shape_wrt_path (shape : Lambda.mixed_block_shape) pos path =
    let _, totals =
      Array.fold_left
        (fun (i, totals) el ->
          ( i + 1,
            add totals
              (if i = pos
              then count_wrt_path el path
              else if i < pos
              then { zero with left = count el }
              else { zero with right = count el }) ))
        (0, zero) shape
    in
    totals

  let count = count_wrt_path

  let count_shape = count_shape_wrt_path

  let all { here; left; right } =
    let value = Byte_count.(add (add here.value left.value) right.value) in
    let flat = Byte_count.(add (add here.flat left.flat) right.flat) in
    { value; flat }

  let lowest_invalid_gap_on_64_bit_arch = 1 lsl 16

  type offset_and_gap_bytes =
    { offset_bytes : Byte_count.t;
      gap_bytes : Byte_count.t
    }

  let offset_and_gap { here; left; right } =
    let offset_bytes =
      if Byte_count.is_zero here.value
      then
        (* If the element is all flats, then the offset points to the first flat
           element rather than the first value element *)
        Byte_count.(add (add left.value left.flat) right.value)
      else left.value
    in
    if has_value_and_flat here
    then
      let gap_bytes = Byte_count.add left.flat right.value in
      (* Conservatively assumes that *all* values and flats in [here] can become
          part of the gap upon deepening (but really, if the deepened pointer
          still has a gap, it must have at least one value and one flat.) *)
      let deepened_gap_upper_bound =
        Byte_count.(add (add gap_bytes here.value) here.flat)
      in
      if Byte_count.on_64_bit_arch deepened_gap_upper_bound
         >= lowest_invalid_gap_on_64_bit_arch
      then None
      else Some { offset_bytes; gap_bytes }
    else Some { offset_bytes; gap_bytes = Byte_count.zero }
end
