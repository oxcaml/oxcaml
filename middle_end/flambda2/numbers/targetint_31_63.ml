(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR-someday mshinwell/gbury: maybe we might want to consider adding some more
   checks in some of the conversions functions to be more safe and more
   consistent in the handling of overflows ? For instance One_bit_fewer.of_int
   silently truncates the input int to make it fit, whereas we probably want to
   make it produce an error ? *)

module T0 = struct
  include Targetint_32_64

  let ten = Targetint_32_64.of_int 10

  let hex_ff = Targetint_32_64.of_int 0xff

  let bool_true = one

  let bool_false = zero

  let bool b = if b then bool_true else bool_false

  let min_value = Targetint_32_64.min_int

  let max_value = Targetint_32_64.max_int

  let bottom_byte_to_int t =
    Targetint_32_64.to_int (Targetint_32_64.logand t hex_ff)

  let xor = Targetint_32_64.logxor

  let or_ = Targetint_32_64.logor

  let and_ = Targetint_32_64.logand

  let mod_ = Targetint_32_64.rem

  let of_char c = Targetint_32_64.of_int (Char.code c)

  let of_int_option i = Some (of_int i)

  let to_targetint t = t

  let of_targetint t = t

  let max t1 t2 = if Targetint_32_64.compare t1 t2 < 0 then t2 else t1

  let min t1 t2 = if Targetint_32_64.compare t1 t2 < 0 then t1 else t2

  let ( <= ) t1 t2 = Stdlib.( <= ) (Targetint_32_64.compare t1 t2) 0

  let ( >= ) t1 t2 = Stdlib.( >= ) (Targetint_32_64.compare t1 t2) 0

  let ( < ) t1 t2 = Stdlib.( < ) (Targetint_32_64.compare t1 t2) 0

  let ( > ) t1 t2 = Stdlib.( > ) (Targetint_32_64.compare t1 t2) 0

  let to_int_option t =
    let min_int_as_int64 = Targetint_32_64.of_int Stdlib.min_int in
    let max_int_as_int64 = Targetint_32_64.of_int Stdlib.max_int in
    if min_int_as_int64 <= t && t <= max_int_as_int64
    then Some (to_int t)
    else None

  let to_int_exn t =
    match to_int_option t with
    | Some i -> i
    | None ->
      Misc.fatal_errorf "Targetint_31_63.to_int_exn: %a out of range"
        Targetint_32_64.print t

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Targetint_32_64.logand t hex_ff in
    let second_to_least_significant_byte =
      Targetint_32_64.shift_right_logical
        (Targetint_32_64.logand t (Targetint_32_64.of_int 0xff00))
        8
    in
    Targetint_32_64.logor second_to_least_significant_byte
      (Targetint_32_64.shift_left least_significant_byte 8)

  let is_non_negative t = t >= zero
end

module Self = struct
  include T0

  (* Note: the [include T0] must be first so that the [One_bit_fewer] functions
     take precedence. *)
  include One_bit_fewer.Make (T0)
  include Container_types.Make (T0)
end

include Self

let all_bools = Set.of_list [bool_true; bool_false]

let zero_one_and_minus_one = Set.of_list [zero; one; minus_one]

module Pair = struct
  type nonrec t = t * t

  include Container_types.Make_pair (Self) (Self)
end

let cross_product = Pair.create_from_cross_product
