(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type t

  val machine_width : t -> Target_system.Machine_width.t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val print : Format.formatter -> t -> unit

  val min_value : Target_system.Machine_width.t -> t

  val max_value : Target_system.Machine_width.t -> t

  val minus_one : Target_system.Machine_width.t -> t

  val zero : Target_system.Machine_width.t -> t

  val one : Target_system.Machine_width.t -> t

  val ten : Target_system.Machine_width.t -> t

  val hex_ff : Target_system.Machine_width.t -> t

  val ( <= ) : t -> t -> bool

  val ( >= ) : t -> t -> bool

  val ( < ) : t -> t -> bool

  val ( > ) : t -> t -> bool

  val bottom_byte_to_int : t -> int

  val of_char : Target_system.Machine_width.t -> char -> t

  val of_int :
    Target_system.Machine_width.t ->
    int ->
    t (* CR mshinwell: clarify semantics *)

  val of_int_option : Target_system.Machine_width.t -> int -> t option

  val of_int32 : Target_system.Machine_width.t -> int32 -> t

  val of_int64 : Target_system.Machine_width.t -> int64 -> t

  val of_targetint : Target_system.Machine_width.t -> Targetint_32_64.t -> t

  val of_float : Target_system.Machine_width.t -> float -> t

  val to_float : t -> float

  val to_int : t -> int

  val to_int_exn : t -> int

  val to_int_option : t -> int option

  val to_int32 : t -> int32

  val to_int64 : t -> int64

  val to_targetint : Target_system.Machine_width.t -> t -> Targetint_32_64.t

  val neg : t -> t

  val get_least_significant_16_bits_then_byte_swap : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val mod_ : t -> t -> t

  val div : t -> t -> t

  val and_ : t -> t -> t

  val or_ : t -> t -> t

  val xor : t -> t -> t

  val shift_left : t -> int -> t

  val shift_right : t -> int -> t

  val shift_right_logical : t -> int -> t

  val max : t -> t -> t

  val min : t -> t -> t
end

(* The goal of this module is to take as argument a module that operates on
   integers of size {n} bits, and return a module that operates on integers of
   size {n-1} bits.

   We assume that {n} > 16, so that all constants can be represented and the
   get_least_significant_16_bits_then_byte_swap function actually has a defined
   semantics.*)
module Make (I : S) : S with type t = I.t = struct
  (* We represent a {n-1} bit integer with an {n} bit integer that falls in the
     range of numbers representable in {n-1} bits. *)
  type t = I.t

  let machine_width = I.machine_width

  let compare = I.compare

  let equal = I.equal

  let hash = I.hash

  let [@ocamlformat "disable"] print = I.print

  (* sign extension can "correct" an {n} bits value that has overflowed over the
     range of {n-1} bits integer, back into the range of {n-1} bits integers, by
     considering that arithmetic operation wrap-around (i.e. arithmetic is done
     modulo 2^{n-2} bits. *)
  let sign_extend t = I.shift_right (I.shift_left t 1) 1

  let min_value machine_width = I.shift_right (I.min_value machine_width) 1

  let max_value machine_width = I.shift_right (I.max_value machine_width) 1

  let minus_one machine_width = I.minus_one machine_width

  let zero machine_width = I.zero machine_width

  let one machine_width = I.one machine_width

  let ten machine_width = I.ten machine_width

  let hex_ff machine_width = I.hex_ff machine_width

  let ( <= ) = I.( <= )

  let ( >= ) = I.( >= )

  let ( < ) = I.( < )

  let ( > ) = I.( > )

  let is_in_range machine_width n =
    I.( >= ) n (min_value machine_width) && I.( <= ) n (max_value machine_width)

  let bottom_byte_to_int = I.bottom_byte_to_int

  let of_char machine_width = I.of_char machine_width

  let of_int machine_width t = sign_extend (I.of_int machine_width t)

  let of_int_option machine_width t =
    Option.map sign_extend (I.of_int_option machine_width t)

  let of_int32 machine_width t =
    let x = I.of_int32 machine_width t in
    sign_extend x

  let of_int64 machine_width t =
    let x = I.of_int64 machine_width t in
    sign_extend x

  let of_targetint machine_width t =
    let x = I.of_targetint machine_width t in
    sign_extend x

  let of_float machine_width t =
    let x = I.of_float machine_width t in
    sign_extend x

  let to_float = I.to_float

  let to_int = I.to_int

  let to_int_exn = I.to_int_exn

  let to_int_option = I.to_int_option

  let to_int32 = I.to_int32

  let to_int64 = I.to_int64

  let to_targetint machine_width = I.to_targetint machine_width

  let neg t = sign_extend (I.neg t)

  let get_least_significant_16_bits_then_byte_swap t =
    let res = I.get_least_significant_16_bits_then_byte_swap t in
    assert (is_in_range (machine_width t) res);
    res

  let add x y = sign_extend (I.add x y)

  let sub x y = sign_extend (I.sub x y)

  let mul x y = sign_extend (I.mul x y)

  (* No sign-extension: the result is always in the correct range *)
  let mod_ = I.mod_

  let div x y = sign_extend (I.div x y)

  let and_ = I.and_

  let or_ = I.or_

  let xor = I.xor

  let shift_left t i = sign_extend (I.shift_left t i)

  (* No sign-extension: the result is always in the correct range *)
  let shift_right = I.shift_right

  let shift_right_logical t i =
    I.shift_right (I.shift_right_logical (I.shift_left t 1) i) 1

  let max = I.max

  let min = I.min
end
