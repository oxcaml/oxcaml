(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Recognition of logical immediate arguments *)

(* An automaton to recognize ( 0+1+0* | 1+0+1* )
 *
 *             0          1          0
 *            / \        / \        / \
 *            \ /        \ /        \ /
 *      -0--> [1] --1--> [2] --0--> [3]
 *     /
 *   [0]
 *     \
 *      -1--> [4] --0--> [5] --1--> [6]
 *            / \        / \        / \
 *            \ /        \ /        \ /
 *             1          0          1
 *
 * The accepting states are 2, 3, 5 and 6. *)
let[@ocamlformat "disable"] auto_table =
  [| (* accepting?, next on 0, next on 1 *)
     (* state 0 *) false, 1, 4;
     (* state 1 *) false, 1, 2;
     (* state 2 *) true,  3, 2;
     (* state 3 *) true,  3, 7;
     (* state 4 *) false, 5, 4;
     (* state 5 *) true,  5, 6;
     (* state 6 *) true,  7, 6;
     (* state 7 *) false, 7, 7 (* error state *)
  |]

let rec run_automata nbits state input =
  let acc, next0, next1 = auto_table.(state) in
  if nbits <= 0
  then acc
  else
    run_automata (nbits - 1)
      (if Nativeint.equal (Nativeint.logand input 1n) 0n then next0 else next1)
      (Nativeint.shift_right_logical input 1)

(* The following function determines a length [e] such that [x] is a repetition
   [BB...B] of a bit pattern [B] of length [e]. [e] ranges over 64, 32, 16, 8,
   4, 2. The smaller [e] the better. *)

let logical_imm_length x =
  (* [test n] checks that the low [2n] bits of [x] are of the form [BB], that
     is, two occurrences of the same [n] bits *)
  let test n =
    let mask = Nativeint.(sub (shift_left 1n n) 1n) in
    let low_n_bits = Nativeint.(logand x mask) in
    let next_n_bits = Nativeint.(logand (shift_right_logical x n) mask) in
    Nativeint.equal low_n_bits next_n_bits
  in
  (* If [test n] fails, we know that the length [e] is at least [2n]. Hence we
     test with decreasing values of [n]: 32, 16, 8, 4, 2. *)
  if not (test 32)
  then 64
  else if not (test 16)
  then 32
  else if not (test 8)
  then 16
  else if not (test 4)
  then 8
  else if not (test 2)
  then 4
  else 2

(* A valid logical immediate is - neither [0] nor [-1]; - composed of a
   repetition [BBBBB] of a bit-pattern [B] of length [e] - the low [e] bits of
   the number, that is, [B], match [0+1+0*] or [1+0+1*]. *)

let is_logical_immediate x =
  (not (Nativeint.equal x 0n))
  && (not (Nativeint.equal x (-1n)))
  && run_automata (logical_imm_length x) 0 x

(* XXX mshinwell: this needs checking carefully *)
(* Encode a logical immediate into N, immr, imms fields for ARM64 instructions.
   Returns (N, immr, imms) tuple.

   The encoding works as follows:

   - N=1 for 64-bit patterns, N=0 for smaller element sizes

   - Element size is the smallest repeating pattern (2, 4, 8, 16, 32, or 64
   bits)

   - Within each element, we have a contiguous run of 1s (possibly rotated)

   - imms encodes both the element size and number of 1s

   - immr encodes the rotation amount *)

(* Create a mask with [n] low bits set to 1. For n=64, shift_left 1n 64 wraps to
   1 due to modular shift on 64-bit nativeint, so we handle this case specially
   by returning -1n (all bits set). *)
let mask_of_width n =
  if n >= 64 then -1n else Nativeint.(sub (shift_left 1n n) 1n)

type encoded_logical_immediate =
  { n : int;
    immr : int;
    imms : int
  }

let encode_logical_immediate_fields (x : nativeint) : encoded_logical_immediate =
  if not (is_logical_immediate x)
  then
    Misc.fatal_error
      "encode_logical_immediate_fields: not a valid logical immediate";
  let len = logical_imm_length x in
  let mask = mask_of_width len in
  let pattern = Nativeint.(logand x mask) in
  (* Find the rightmost set bit position (start of ones run if not rotated) *)
  let rec find_first_one p pos =
    if pos >= len
    then 0
    else if Nativeint.equal (Nativeint.logand p 1n) 1n
    then pos
    else find_first_one (Nativeint.shift_right_logical p 1) (pos + 1)
  in
  (* Count consecutive ones starting from position *)
  let rec count_ones p pos count =
    if pos >= len || Nativeint.equal (Nativeint.logand p 1n) 0n
    then count
    else count_ones (Nativeint.shift_right_logical p 1) (pos + 1) (count + 1)
  in
  (* Count total ones in the pattern *)
  let rec count_all_ones p pos acc =
    if pos >= len
    then acc
    else
      let bit = Nativeint.(logand p 1n) in
      let acc' = if Nativeint.equal bit 1n then acc + 1 else acc in
      count_all_ones (Nativeint.shift_right_logical p 1) (pos + 1) acc'
  in
  (* Find position of first zero starting from LSB *)
  let rec find_first_zero p pos =
    if pos >= len
    then len
    else if Nativeint.equal (Nativeint.logand p 1n) 0n
    then pos
    else find_first_zero (Nativeint.shift_right_logical p 1) (pos + 1)
  in
  (* Find position of first one starting from a position *)
  let rec find_one_from p pos =
    if pos >= len
    then len
    else if Nativeint.equal (Nativeint.logand p 1n) 1n
    then pos
    else find_one_from (Nativeint.shift_right_logical p 1) (pos + 1)
  in
  (* Determine rotation and ones count based on pattern type *)
  let rotation, ones =
    let first_one = find_first_one pattern 0 in
    if first_one > 0
    then
      (* Pattern starts with zeros: 0+1+0* type *)
      (* Rotate right by first_one to put ones at LSB *)
      let rotated =
        let shift = first_one in
        Nativeint.(
          logor
            (shift_right_logical pattern shift)
            (logand (shift_left pattern (len - shift)) mask))
      in
      let ones = count_ones rotated 0 0 in
      first_one, ones
    else
      (* Pattern starts with one: check if it's 1+0+1* (ones wrap around) *)
      let ones_from_lsb = count_ones pattern 0 0 in
      if ones_from_lsb = len
      then
        (* XXX mshinwell: should this be a failure case? *)
        0, len (* All ones - shouldn't happen for valid logical immediate *)
      else
        let first_zero = find_first_zero pattern 0 in
        let shifted_for_search =
          Nativeint.shift_right_logical pattern first_zero
        in
        let next_one_rel = find_one_from shifted_for_search 0 in
        if next_one_rel >= len - first_zero
        then (* No more ones after zeros: simple 1+0* pattern *)
          0, ones_from_lsb
        else
          (* 1+0+1* pattern: ones at both ends wrap around *)
          let total_ones = count_all_ones pattern 0 0 in
          let first_one_after_zeros = first_zero + next_one_rel in
          (* To canonicalize, rotate right by first_one_after_zeros *)
          (* Then immr = (len - rotation) mod len will give correct result *)
          first_one_after_zeros, total_ones
  in
  (* Encode N based on element size *)
  let n = if len = 64 then 1 else 0 in
  (* immr encodes the rotation: ARM rotates the canonical form (1s at LSB) right
     by immr to produce the actual value. Since we rotated right by 'rotation'
     to get the canonical form, the inverse is (len - rotation) mod len. *)
  let immr = (len - rotation) mod len in
  (* imms encoding: the highest 0 bit indicates element size, remaining bits
     encode (ones - 1).

     For N=0:

     - 32-bit element: imms = 0xxxxx (bit 5 = 0, bits 0-4 for ones)

     - 16-bit element: imms = 10xxxx (bits 0-3 for ones)

     - 8-bit element: imms = 110xxx (bits 0-2 for ones)

     - 4-bit element: imms = 1110xx (bits 0-1 for ones)

     - 2-bit element: imms = 11110x (bit 0 for ones).

     For N=1 (64-bit), all 6 bits encode ones. *)
  let size_encoding =
    match len with
    | 64 -> 0b000000
    | 32 -> 0b000000
    | 16 -> 0b100000
    | 8 -> 0b110000
    | 4 -> 0b111000
    | 2 -> 0b111100
    | size -> Misc.fatal_errorf "invalid element size: %d" size ()
  in
  let imms = size_encoding lor (ones - 1) in
  { n; immr; imms }
