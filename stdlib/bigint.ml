(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Sign-magnitude representation with immutable, least-significant-first,
   half-word limbs.  Half-word limbs keep every limb product, with its
   carries, within [max_int], so the arithmetic below never overflows.

   The representation is canonical: no leading zero limbs, and zero has
   sign 0 with an empty magnitude.  Canonicity is load-bearing: it is what
   makes polymorphic equality agree with [equal].  Every magnitude is
   built by [trim], which is the one place that enforces it. *)

let radix_bits = (Sys.int_size - 1) / 2
let radix = 1 lsl radix_bits
let mask = radix - 1

type t : immutable_data =
  { sign : int  (* -1, 0 or 1; 0 exactly for zero *)
  ; mag : int iarray  (* each limb in [0, radix); no leading zero limb *)
  }

let zero = { sign = 0; mag = Iarray.init 0 (fun _ -> 0) }

(* Freeze a scratch magnitude, dropping leading zero limbs. *)
let trim scratch =
  let length = ref (Array.length scratch) in
  while !length > 0 && scratch.(!length - 1) = 0 do
    length := !length - 1
  done;
  Iarray.init !length (fun index -> scratch.(index))

(* [make sign mag] assembles a value from a possibly-empty magnitude,
   forcing sign 0 on the empty one. *)
let make sign mag =
  if Iarray.length mag = 0 then zero else { sign; mag }

let is_zero value = value.sign = 0

let int_compare (left : int) right =
  if left < right then -1 else if left > right then 1 else 0

let compare_magnitude left right =
  let left_length = Iarray.length left in
  let right_length = Iarray.length right in
  if left_length <> right_length
  then int_compare left_length right_length
  else begin
    let rec from index =
      if index < 0
      then 0
      else begin
        let order =
          int_compare (Iarray.get left index) (Iarray.get right index)
        in
        if order <> 0 then order else from (index - 1)
      end
    in
    from (left_length - 1)
  end

let compare left right =
  if left.sign <> right.sign
  then int_compare left.sign right.sign
  else if left.sign >= 0
  then compare_magnitude left.mag right.mag
  else compare_magnitude right.mag left.mag

let equal left right = compare left right = 0
let lt left right = compare left right < 0
let le left right = compare left right <= 0
let gt left right = compare left right > 0
let ge left right = compare left right >= 0

let neg value = make (-value.sign) value.mag
let abs value = make 1 value.mag

let add_magnitude left right =
  let left_length = Iarray.length left in
  let right_length = Iarray.length right in
  let length =
    (if left_length > right_length then left_length else right_length) + 1
  in
  let scratch = Array.make length 0 in
  let carry = ref 0 in
  for index = 0 to length - 1 do
    let left_limb = if index < left_length then Iarray.get left index else 0 in
    let right_limb =
      if index < right_length then Iarray.get right index else 0
    in
    let sum = left_limb + right_limb + !carry in
    scratch.(index) <- sum land mask;
    carry := sum lsr radix_bits
  done;
  (* The final carry landed in the extra limb, so [!carry] is 0 here. *)
  trim scratch

(* [left] must be at least [right]. *)
let subtract_magnitude left right =
  let left_length = Iarray.length left in
  let right_length = Iarray.length right in
  let scratch = Array.make left_length 0 in
  let borrow = ref 0 in
  for index = 0 to left_length - 1 do
    let right_limb =
      if index < right_length then Iarray.get right index else 0
    in
    let difference = Iarray.get left index - right_limb - !borrow in
    if difference < 0
    then begin
      scratch.(index) <- difference + radix;
      borrow := 1
    end
    else begin
      scratch.(index) <- difference;
      borrow := 0
    end
  done;
  trim scratch

(* Schoolbook multiplication.  Every intermediate value is at most
   (radix - 1)^2 + 2 * (radix - 1) = radix^2 - 1 <= max_int. *)
let multiply_magnitude left right =
  let left_length = Iarray.length left in
  let right_length = Iarray.length right in
  let scratch = Array.make (left_length + right_length) 0 in
  for left_index = 0 to left_length - 1 do
    let left_limb = Iarray.get left left_index in
    let carry = ref 0 in
    for right_index = 0 to right_length - 1 do
      let product =
        (left_limb * Iarray.get right right_index)
        + scratch.(left_index + right_index)
        + !carry
      in
      scratch.(left_index + right_index) <- product land mask;
      carry := product lsr radix_bits
    done;
    scratch.(left_index + right_length) <- !carry
  done;
  trim scratch

let add left right =
  if left.sign = 0
  then right
  else if right.sign = 0
  then left
  else if left.sign = right.sign
  then make left.sign (add_magnitude left.mag right.mag)
  else begin
    let magnitude_order = compare_magnitude left.mag right.mag in
    if magnitude_order >= 0
    then make left.sign (subtract_magnitude left.mag right.mag)
    else make right.sign (subtract_magnitude right.mag left.mag)
  end

let sub left right = add left (neg right)

let mul left right =
  make (left.sign * right.sign) (multiply_magnitude left.mag right.mag)

let of_int integer =
  if integer = 0
  then zero
  else begin
    (* Peel limbs from the negated value: [-max_int] is representable but
       [-min_int] is not, so the loop works on non-positive numbers.
       Three limbs always suffice: 3 * radix_bits >= Sys.int_size. *)
    let scratch = Array.make 3 0 in
    let remaining = ref (if integer < 0 then integer else -integer) in
    let index = ref 0 in
    while !remaining <> 0 do
      scratch.(!index) <- -(!remaining mod radix);
      remaining := !remaining / radix;
      index := !index + 1
    done;
    { sign = (if integer < 0 then -1 else 1); mag = trim scratch }
  end

let one = of_int 1
let min_int_bigint = of_int min_int
let max_int_bigint = of_int max_int

let to_int_opt value =
  if lt value min_int_bigint || gt value max_int_bigint
  then None
  else begin
    (* In range, so folding up the negated value stays within
       [min_int, 0] and cannot overflow. *)
    let rec negated index acc =
      if index < 0
      then acc
      else negated (index - 1) ((acc * radix) - Iarray.get value.mag index)
    in
    let negated_value = negated (Iarray.length value.mag - 1) 0 in
    Some (if value.sign < 0 then negated_value else -negated_value)
  end

(* Decimal conversion peels [decimal_chunk_width] digits at a time.  The
   chunk is sized so that the division step's intermediate value,
   (decimal_chunk - 1) * radix + mask, stays within [max_int]. *)
let decimal_chunk, decimal_chunk_width =
  if Sys.int_size <= 32 then 10_000, 4 else 1_000_000_000, 9

(* Divide a magnitude by a small positive integer; quotient and remainder. *)
let divide_magnitude_small magnitude divisor =
  let length = Iarray.length magnitude in
  let scratch = Array.make length 0 in
  let remainder = ref 0 in
  for index = length - 1 downto 0 do
    let current = (!remainder * radix) + Iarray.get magnitude index in
    scratch.(index) <- current / divisor;
    remainder := current mod divisor
  done;
  trim scratch, !remainder

let to_string value =
  if value.sign = 0
  then "0"
  else begin
    let rec chunks magnitude collected =
      if Iarray.length magnitude = 0
      then collected
      else begin
        let quotient, chunk =
          divide_magnitude_small magnitude decimal_chunk
        in
        chunks quotient (chunk :: collected)
      end
    in
    match chunks value.mag [] with
    | [] -> assert false (* a nonzero value has a nonempty magnitude *)
    | most_significant :: rest ->
      let buffer = Buffer.create 32 in
      if value.sign < 0 then Buffer.add_char buffer '-';
      Buffer.add_string buffer (string_of_int most_significant);
      List.iter
        (fun chunk ->
          let digits = string_of_int chunk in
          for _ = String.length digits + 1 to decimal_chunk_width do
            Buffer.add_char buffer '0'
          done;
          Buffer.add_string buffer digits)
        rest;
      Buffer.contents buffer
  end

let of_string string =
  let reject reason = invalid_arg ("Bigint.of_string: " ^ reason) in
  let length = String.length string in
  if length = 0 then reject "empty string";
  let negative = string.[0] = '-' in
  let first = if negative then 1 else 0 in
  if first >= length then reject "no digits";
  String.iteri
    (fun index character ->
      if index >= first && not ('0' <= character && character <= '9')
      then reject "non-digit character")
    string;
  if string.[first] = '0' && length - first > 1
  then reject "redundant leading zero";
  if negative && string.[first] = '0' then reject "negative zero";
  let ten = of_int 10 in
  let magnitude = ref zero in
  for index = first to length - 1 do
    magnitude :=
      add
        (mul !magnitude ten)
        (of_int (Char.code string.[index] - Char.code '0'))
  done;
  if negative then neg !magnitude else !magnitude
