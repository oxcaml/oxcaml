(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module MW = Target_system.Machine_width

type t =
  | Int32 of int32
  | Int64 of int64

type targetint = t

module Int32 = struct
  include Int32

  let of_int_exn =
    match Sys.int_size with
    (* size of [int] on the host *)
    | 31 -> Int32.of_int
    | 63 ->
      fun n ->
        if n < Int32.to_int Int32.min_int || n > Int32.to_int Int32.max_int
        then Misc.fatal_errorf "Targetint_32_64.of_int_exn: 0x%x out of range" n
        else Int32.of_int n
    | _ -> assert false

  let to_int64 = Int64.of_int32

  include Container_types.Make (struct
    type nonrec t = t

    let compare = Int32.compare

    let equal = Int32.equal

    let hash = Hashtbl.hash

    let [@ocamlformat "disable"] print ppf t = Format.fprintf ppf "%ld" t
  end)

  let min t1 t2 = if Int32.compare t1 t2 <= 0 then t1 else t2

  let max t1 t2 = if Int32.compare t1 t2 <= 0 then t2 else t1

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int32.logand t 0xffl in
    let second_to_least_significant_byte =
      shift_right_logical (Int32.logand t 0xff00l) 8
    in
    Int32.logor second_to_least_significant_byte
      (shift_left least_significant_byte 8)

  external swap_byte_endianness : t -> t = "%bswap_int32"
end

module Int64 = struct
  include Int64

  let of_int_exn = Int64.of_int

  include Container_types.Make (struct
    type nonrec t = t

    let compare = Int64.compare

    let equal = Int64.equal

    let hash = Hashtbl.hash

    let [@ocamlformat "disable"] print ppf t = Format.fprintf ppf "%Ld" t
  end)

  let min t1 t2 = if Int64.compare t1 t2 <= 0 then t1 else t2

  let max t1 t2 = if Int64.compare t1 t2 <= 0 then t2 else t1

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int64.logand t 0xffL in
    let second_to_least_significant_byte =
      Int64.shift_right_logical (Int64.logand t 0xff00L) 8
    in
    Int64.logor second_to_least_significant_byte
      (Int64.shift_left least_significant_byte 8)

  external swap_byte_endianness : t -> t = "%bswap_int64"
end

(* Print function *)
let print ppf t =
  match t with
  | Int32 x -> Format.fprintf ppf "%ld" x
  | Int64 x -> Format.fprintf ppf "%Ld" x

(* Creation functions *)
let zero machine_width =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 Int32.zero
  | MW.Sixty_four -> Int64 Int64.zero

let zero_like t =
  match t with Int32 _ -> Int32 Int32.zero | Int64 _ -> Int64 Int64.zero

let one machine_width =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 Int32.one
  | MW.Sixty_four -> Int64 Int64.one

let minus_one machine_width =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 Int32.minus_one
  | MW.Sixty_four -> Int64 Int64.minus_one

let max_int machine_width =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 Int32.max_int
  | MW.Sixty_four -> Int64 Int64.max_int

let min_int machine_width =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 Int32.min_int
  | MW.Sixty_four -> Int64 Int64.min_int

(* Unary operations *)
let neg t =
  match t with Int32 x -> Int32 (Int32.neg x) | Int64 x -> Int64 (Int64.neg x)

let abs t =
  match t with Int32 x -> Int32 (Int32.abs x) | Int64 x -> Int64 (Int64.abs x)

let succ t =
  match t with
  | Int32 x -> Int32 (Int32.succ x)
  | Int64 x -> Int64 (Int64.succ x)

let pred t =
  match t with
  | Int32 x -> Int32 (Int32.pred x)
  | Int64 x -> Int64 (Int64.pred x)

let lognot t =
  match t with
  | Int32 x -> Int32 (Int32.lognot x)
  | Int64 x -> Int64 (Int64.lognot x)

(* Binary operations - need to check compatibility *)
let add t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.add x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.add x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.add: incompatible types %a and %a" print
      t1 print t2

let sub t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.sub x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.sub x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.sub: incompatible types %a and %a" print
      t1 print t2

let mul t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.mul x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.mul x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.mul: incompatible types %a and %a" print
      t1 print t2

let div t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.div x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.div x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.div: incompatible types %a and %a" print
      t1 print t2

let unsigned_div t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.unsigned_div x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.unsigned_div x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf
      "Targetint_32_64.unsigned_div: incompatible types %a and %a" print t1
      print t2

let rem t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.rem x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.rem x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.rem: incompatible types %a and %a" print
      t1 print t2

let unsigned_rem t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.unsigned_rem x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.unsigned_rem x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf
      "Targetint_32_64.unsigned_rem: incompatible types %a and %a" print t1
      print t2

let logand t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.logand x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.logand x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.logand: incompatible types %a and %a"
      print t1 print t2

let logor t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.logor x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.logor x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.logor: incompatible types %a and %a"
      print t1 print t2

let logxor t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.logxor x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.logxor x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.logxor: incompatible types %a and %a"
      print t1 print t2

(* Shift operations *)
let shift_left t n =
  match t with
  | Int32 x -> Int32 (Int32.shift_left x n)
  | Int64 x -> Int64 (Int64.shift_left x n)

let shift_right t n =
  match t with
  | Int32 x -> Int32 (Int32.shift_right x n)
  | Int64 x -> Int64 (Int64.shift_right x n)

let shift_right_logical t n =
  match t with
  | Int32 x -> Int32 (Int32.shift_right_logical x n)
  | Int64 x -> Int64 (Int64.shift_right_logical x n)

(* Comparison functions *)
let compare t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32.compare x1 x2
  | Int64 x1, Int64 x2 -> Int64.compare x1 x2
  | Int32 _, Int64 _ -> -1
  | Int64 _, Int32 _ -> 1

let equal t1 t2 = compare t1 t2 = 0

let unsigned_compare t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32.unsigned_compare x1 x2
  | Int64 x1, Int64 x2 -> Int64.unsigned_compare x1 x2
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf
      "Targetint_32_64.unsigned_compare: incompatible types %a and %a" print t1
      print t2

let min t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.min x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.min x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.min: incompatible types %a and %a" print
      t1 print t2

let max t1 t2 =
  match t1, t2 with
  | Int32 x1, Int32 x2 -> Int32 (Int32.max x1 x2)
  | Int64 x1, Int64 x2 -> Int64 (Int64.max x1 x2)
  | Int32 _, Int64 _ | Int64 _, Int32 _ ->
    Misc.fatal_errorf "Targetint_32_64.max: incompatible types %a and %a" print
      t1 print t2

(* Conversion functions *)
let of_int machine_width n =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int32.of_int n)
  | MW.Sixty_four -> Int64 (Int64.of_int n)

let of_int_exn machine_width n =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int32.of_int_exn n)
  | MW.Sixty_four -> Int64 (Int64.of_int_exn n)

let to_int t =
  match t with Int32 x -> Int32.to_int x | Int64 x -> Int64.to_int x

let of_int32 machine_width x =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 x
  | MW.Sixty_four -> Int64 (Int64.of_int32 x)

let to_int32 t = match t with Int32 x -> x | Int64 x -> Int64.to_int32 x

let of_int64 machine_width x =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int64.to_int32 x)
  | MW.Sixty_four -> Int64 x

let to_int64 t = match t with Int32 x -> Int32.to_int64 x | Int64 x -> x

let of_float machine_width f =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int32.of_float f)
  | MW.Sixty_four -> Int64 (Int64.of_float f)

let to_float t =
  match t with Int32 x -> Int32.to_float x | Int64 x -> Int64.to_float x

let of_string machine_width s =
  match machine_width with
  | MW.Thirty_two | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int32.of_string s)
  | MW.Sixty_four -> Int64 (Int64.of_string s)

let to_string t =
  match t with Int32 x -> Int32.to_string x | Int64 x -> Int64.to_string x

(* Other utility functions *)
let get_least_significant_16_bits_then_byte_swap t =
  match t with
  | Int32 x -> Int32 (Int32.get_least_significant_16_bits_then_byte_swap x)
  | Int64 x -> Int64 (Int64.get_least_significant_16_bits_then_byte_swap x)

let swap_byte_endianness t =
  match t with
  | Int32 x -> Int32 (Int32.swap_byte_endianness x)
  | Int64 x -> Int64 (Int64.swap_byte_endianness x)

let hash t =
  match t with Int32 x -> Hashtbl.hash x | Int64 x -> Hashtbl.hash x

(* Container_types implementation *)
module Self = struct
  type nonrec t = t

  let compare = compare

  let equal = equal

  let hash = hash

  let print = print
end

include Container_types.Make (Self)
module Targetint_set = Set

module Pair = struct
  type nonrec t = t * t

  module T_pair = Container_types.Pair (Self) (Self)
  include Container_types.Make (T_pair)
end

let cross_product set1 set2 =
  Set.fold
    (fun elt1 result ->
      Set.fold (fun elt2 result -> Pair.Set.add (elt1, elt2) result) set2 result)
    set1 Pair.Set.empty

let to_int_checked machine_width t =
  let i = to_int t in
  let t' = of_int machine_width i in
  if not (equal t t')
  then
    Misc.fatal_errorf "Cannot translate Targetint_32_64 %a to an OCaml integer"
      print t;
  i

(* Define repr type and function at the very end to avoid constructor
   shadowing *)
type repr =
  | Int32 of int32
  | Int64 of int64

let repr (t : t) : repr = match t with Int32 x -> Int32 x | Int64 x -> Int64 x
