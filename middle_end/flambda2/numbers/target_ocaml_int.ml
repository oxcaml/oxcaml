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

module MW = Target_system.Machine_width

type t =
  | Int31 of int32
  | Int32 of int32
  | Int63 of int64

(* Wrapper module for Int32 to work with One_bit_fewer *)
module Int32_base = struct
  type t = int32

  let machine_width _t = MW.Thirty_two

  let compare = Int32.compare

  let equal = Int32.equal

  let hash = Hashtbl.hash

  let print ppf t = Format.fprintf ppf "%ld" t

  let min_value _machine_width = Int32.min_int

  let max_value _machine_width = Int32.max_int

  let minus_one _machine_width = Int32.minus_one

  let zero _machine_width = Int32.zero

  let one _machine_width = Int32.one

  let ten _machine_width = 10l

  let hex_ff _machine_width = 0xffl

  let ( <= ) x y = Int32.compare x y <= 0

  let ( >= ) x y = Int32.compare x y >= 0

  let ( < ) x y = Int32.compare x y < 0

  let ( > ) x y = Int32.compare x y > 0

  let bottom_byte_to_int t = Int32.to_int (Int32.logand t 0xffl)

  let of_char _machine_width c = Int32.of_int (Char.code c)

  let of_int _machine_width i = Int32.of_int i

  let of_int_option _machine_width i = Some (Int32.of_int i)

  let of_int32 _machine_width i = i

  let of_int64 _machine_width i = Int64.to_int32 i

  let of_targetint _machine_width t =
    match Targetint_32_64.repr t with
    | Targetint_32_64.Int32 x -> x
    | Targetint_32_64.Int64 x -> Int64.to_int32 x

  let of_float _machine_width f = Int32.of_float f

  let to_float = Int32.to_float

  let to_int = Int32.to_int

  let to_int_exn = Int32.to_int

  let to_int_option t = Some (Int32.to_int t)

  let to_int32 t = t

  let to_int64 = Int64.of_int32

  let to_targetint machine_width t = Targetint_32_64.of_int32 machine_width t

  let neg = Int32.neg

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int32.logand t 0xffl in
    let second_to_least_significant_byte =
      Int32.shift_right_logical (Int32.logand t 0xff00l) 8
    in
    Int32.logor second_to_least_significant_byte
      (Int32.shift_left least_significant_byte 8)

  let add = Int32.add

  let sub = Int32.sub

  let mul = Int32.mul

  let mod_ = Int32.rem

  let div = Int32.div

  let and_ = Int32.logand

  let or_ = Int32.logor

  let xor = Int32.logxor

  let shift_left = Int32.shift_left

  let shift_right = Int32.shift_right

  let shift_right_logical = Int32.shift_right_logical

  let max x y = if Stdlib.( > ) (Int32.compare x y) 0 then x else y

  let min x y = if Stdlib.( < ) (Int32.compare x y) 0 then x else y
end

(* Wrapper module for Int64 to work with One_bit_fewer *)
module Int64_base = struct
  type t = int64

  let machine_width _t = MW.Sixty_four

  let compare = Int64.compare

  let equal = Int64.equal

  let hash = Hashtbl.hash

  let print ppf t = Format.fprintf ppf "%Ld" t

  let min_value _machine_width = Int64.min_int

  let max_value _machine_width = Int64.max_int

  let minus_one _machine_width = Int64.minus_one

  let zero _machine_width = Int64.zero

  let one _machine_width = Int64.one

  let ten _machine_width = 10L

  let hex_ff _machine_width = 0xffL

  let ( <= ) x y = Int64.compare x y <= 0

  let ( >= ) x y = Int64.compare x y >= 0

  let ( < ) x y = Int64.compare x y < 0

  let ( > ) x y = Int64.compare x y > 0

  let bottom_byte_to_int t = Int64.to_int (Int64.logand t 0xffL)

  let of_char _machine_width c = Int64.of_int (Char.code c)

  let of_int _machine_width i = Int64.of_int i

  let of_int_option _machine_width i = Some (Int64.of_int i)

  let of_int32 _machine_width i = Int64.of_int32 i

  let of_int64 _machine_width i = i

  let of_targetint _machine_width t =
    match Targetint_32_64.repr t with
    | Targetint_32_64.Int32 x -> Int64.of_int32 x
    | Targetint_32_64.Int64 x -> x

  let of_float _machine_width f = Int64.of_float f

  let to_float = Int64.to_float

  let to_int = Int64.to_int

  let to_int_exn = Int64.to_int

  let to_int_option t = Some (Int64.to_int t)

  let to_int32 = Int64.to_int32

  let to_int64 t = t

  let to_targetint machine_width t = Targetint_32_64.of_int64 machine_width t

  let neg = Int64.neg

  let get_least_significant_16_bits_then_byte_swap t =
    let least_significant_byte = Int64.logand t 0xffL in
    let second_to_least_significant_byte =
      Int64.shift_right_logical (Int64.logand t 0xff00L) 8
    in
    Int64.logor second_to_least_significant_byte
      (Int64.shift_left least_significant_byte 8)

  let add = Int64.add

  let sub = Int64.sub

  let mul = Int64.mul

  let mod_ = Int64.rem

  let div = Int64.div

  let and_ = Int64.logand

  let or_ = Int64.logor

  let xor = Int64.logxor

  let shift_left = Int64.shift_left

  let shift_right = Int64.shift_right

  let shift_right_logical = Int64.shift_right_logical

  let max x y = if Stdlib.( > ) (Int64.compare x y) 0 then x else y

  let min x y = if Stdlib.( < ) (Int64.compare x y) 0 then x else y
end

(* Create the One_bit_fewer versions for 31-bit and 63-bit *)
module Int31 = One_bit_fewer.Make (Int32_base)
module Int63 = One_bit_fewer.Make (Int64_base)

let print ppf = function
  | Int31 x -> Format.fprintf ppf "%ld" x
  | Int32 x -> Format.fprintf ppf "%ld" x
  | Int63 x -> Format.fprintf ppf "%Ld" x

let machine_width = function
  | Int31 _ -> MW.Thirty_two
  | Int32 _ -> MW.Thirty_two_no_gc_tag_bit
  | Int63 _ -> MW.Sixty_four

let compare t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31.compare x1 x2
  | Int32 x1, Int32 x2 -> Int32.compare x1 x2
  | Int63 x1, Int63 x2 -> Int63.compare x1 x2
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.compare: incompatible types %a and %a"
      print t1 print t2

let equal t1 t2 = compare t1 t2 = 0

let hash = function
  | Int31 x -> Int31.hash x
  | Int32 x -> Hashtbl.hash x
  | Int63 x -> Int63.hash x

let zero machine_width =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.zero MW.Thirty_two)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 0l
  | MW.Sixty_four -> Int63 (Int63.zero MW.Sixty_four)

let one machine_width =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.one MW.Thirty_two)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 1l
  | MW.Sixty_four -> Int63 (Int63.one MW.Sixty_four)

let minus_one machine_width =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.minus_one MW.Thirty_two)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 (-1l)
  | MW.Sixty_four -> Int63 (Int63.minus_one MW.Sixty_four)

let ten machine_width =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.ten MW.Thirty_two)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 10l
  | MW.Sixty_four -> Int63 (Int63.ten MW.Sixty_four)

let hex_ff machine_width =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.hex_ff MW.Thirty_two)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 0xffl
  | MW.Sixty_four -> Int63 (Int63.hex_ff MW.Sixty_four)

let min_value machine_width =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.min_value MW.Thirty_two)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 Int32.min_int
  | MW.Sixty_four -> Int63 (Int63.min_value MW.Sixty_four)

let max_value machine_width =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.max_value MW.Thirty_two)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 Int32.max_int
  | MW.Sixty_four -> Int63 (Int63.max_value MW.Sixty_four)

let bool_true machine_width = one machine_width

let bool_false machine_width = zero machine_width

let bool machine_width b =
  if b then bool_true machine_width else bool_false machine_width

let ( <= ) t1 t2 = compare t1 t2 <= 0

let ( >= ) t1 t2 = compare t1 t2 >= 0

let ( < ) t1 t2 = compare t1 t2 < 0

let bottom_byte_to_int = function
  | Int31 x -> Int31.bottom_byte_to_int x
  | Int32 x -> Int32.to_int (Int32.logand x 0xffl)
  | Int63 x -> Int63.bottom_byte_to_int x

let of_char machine_width c =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.of_char MW.Thirty_two c)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int32.of_int (Char.code c))
  | MW.Sixty_four -> Int63 (Int63.of_char MW.Sixty_four c)

let of_int machine_width i =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.of_int MW.Thirty_two i)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int32.of_int i)
  | MW.Sixty_four -> Int63 (Int63.of_int MW.Sixty_four i)

let of_int_option machine_width i = Some (of_int machine_width i)

let of_int32 machine_width i =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.of_int32 MW.Thirty_two i)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 i
  | MW.Sixty_four -> Int63 (Int63.of_int32 MW.Sixty_four i)

let of_int64 machine_width i =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.of_int64 MW.Thirty_two i)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int64.to_int32 i)
  | MW.Sixty_four -> Int63 (Int63.of_int64 MW.Sixty_four i)

let of_float machine_width f =
  match machine_width with
  | MW.Thirty_two -> Int31 (Int31.of_float MW.Thirty_two f)
  | MW.Thirty_two_no_gc_tag_bit -> Int32 (Int32.of_float f)
  | MW.Sixty_four -> Int63 (Int63.of_float MW.Sixty_four f)

let to_float = function
  | Int31 x -> Int31.to_float x
  | Int32 x -> Int32.to_float x
  | Int63 x -> Int63.to_float x

let to_int = function
  | Int31 x -> Int31.to_int x
  | Int32 x -> Int32.to_int x
  | Int63 x -> Int63.to_int x

let to_int32 = function
  | Int31 x -> Int31.to_int32 x
  | Int32 x -> x
  | Int63 x -> Int63.to_int32 x

let to_int64 = function
  | Int31 x -> Int31.to_int64 x
  | Int32 x -> Int64.of_int32 x
  | Int63 x -> Int63.to_int64 x

let to_int_option t =
  (* CR selee: maybe change to [to_int_in_range_option t ~min ~max] *)
  let t_as_int64 = to_int64 t in
  let min_int_as_int64 = Int64.of_int Stdlib.min_int in
  let max_int_as_int64 = Int64.of_int Stdlib.max_int in
  let le x y = Stdlib.( <= ) (Int64.compare x y) 0 in
  if le min_int_as_int64 t_as_int64 && le t_as_int64 max_int_as_int64
  then Some (to_int t)
  else None

let to_int_exn t =
  match to_int_option t with
  | Some i -> i
  | None ->
    Misc.fatal_errorf "Target_ocaml_int.to_int_exn: %a out of range" print t

let of_targetint machine_width t =
  match machine_width, Targetint_32_64.repr t with
  | MW.Thirty_two, Targetint_32_64.Int32 x ->
    Int31 (Int31.of_int32 MW.Thirty_two x)
  | MW.Thirty_two_no_gc_tag_bit, Targetint_32_64.Int32 x -> Int32 x
  | MW.Sixty_four, Targetint_32_64.Int64 x ->
    Int63 (Int63.of_int64 MW.Sixty_four x)
  | MW.Thirty_two, Targetint_32_64.Int64 _
  | MW.Thirty_two_no_gc_tag_bit, Targetint_32_64.Int64 _
  | MW.Sixty_four, Targetint_32_64.Int32 _ ->
    Misc.fatal_errorf
      "Target_ocaml_int.of_targetint: incompatible machine width and targetint"

let to_targetint machine_width t =
  match machine_width, t with
  | MW.Thirty_two, Int31 x ->
    Targetint_32_64.of_int32 MW.Thirty_two (Int31.to_int32 x)
  | MW.Thirty_two_no_gc_tag_bit, Int32 x ->
    Targetint_32_64.of_int32 MW.Thirty_two_no_gc_tag_bit x
  | MW.Sixty_four, Int63 x ->
    Targetint_32_64.of_int64 MW.Sixty_four (Int63.to_int64 x)
  | MW.Thirty_two, (Int32 _ | Int63 _)
  | MW.Thirty_two_no_gc_tag_bit, (Int31 _ | Int63 _)
  | MW.Sixty_four, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.to_targetint: mismatched machine width"

let neg = function
  | Int31 x -> Int31 (Int31.neg x)
  | Int32 x -> Int32 (Int32.neg x)
  | Int63 x -> Int63 (Int63.neg x)

let add t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.add x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.add x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.add x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.add: incompatible types %a and %a" print
      t1 print t2

let sub t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.sub x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.sub x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.sub x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.sub: incompatible types %a and %a" print
      t1 print t2

let mul t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.mul x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.mul x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.mul x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.mul: incompatible types %a and %a" print
      t1 print t2

let div t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.div x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.div x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.div x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.div: incompatible types %a and %a" print
      t1 print t2

let mod_ t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.mod_ x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.rem x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.mod_ x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.mod_: incompatible types %a and %a"
      print t1 print t2

let and_ t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.and_ x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.logand x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.and_ x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.and_: incompatible types %a and %a"
      print t1 print t2

let or_ t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.or_ x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.logor x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.or_ x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.or_: incompatible types %a and %a" print
      t1 print t2

let xor t1 t2 =
  match t1, t2 with
  | Int31 x1, Int31 x2 -> Int31 (Int31.xor x1 x2)
  | Int32 x1, Int32 x2 -> Int32 (Int32.logxor x1 x2)
  | Int63 x1, Int63 x2 -> Int63 (Int63.xor x1 x2)
  | Int31 _, (Int32 _ | Int63 _)
  | Int32 _, (Int31 _ | Int63 _)
  | Int63 _, (Int31 _ | Int32 _) ->
    Misc.fatal_errorf "Target_ocaml_int.xor: incompatible types %a and %a" print
      t1 print t2

let shift_left t i =
  match t with
  | Int31 x -> Int31 (Int31.shift_left x i)
  | Int32 x -> Int32 (Int32.shift_left x i)
  | Int63 x -> Int63 (Int63.shift_left x i)

let shift_right t i =
  match t with
  | Int31 x -> Int31 (Int31.shift_right x i)
  | Int32 x -> Int32 (Int32.shift_right x i)
  | Int63 x -> Int63 (Int63.shift_right x i)

let shift_right_logical t i =
  match t with
  | Int31 x -> Int31 (Int31.shift_right_logical x i)
  | Int32 x -> Int32 (Int32.shift_right_logical x i)
  | Int63 x -> Int63 (Int63.shift_right_logical x i)

let max t1 t2 = if Stdlib.( < ) (compare t1 t2) 0 then t2 else t1

let min t1 t2 = if Stdlib.( < ) (compare t1 t2) 0 then t1 else t2

let get_least_significant_16_bits_then_byte_swap t =
  let mw = machine_width t in
  let least_significant_byte = and_ t (hex_ff mw) in
  let second_to_least_significant_byte =
    shift_right_logical (and_ t (of_int mw 0xff00)) 8
  in
  or_ second_to_least_significant_byte (shift_left least_significant_byte 8)

let is_non_negative t = t >= zero (machine_width t)

let of_int8 machine_width i = of_int machine_width (Numeric_types.Int8.to_int i)

let of_int16 machine_width i =
  of_int machine_width (Numeric_types.Int16.to_int i)

module Self = struct
  type nonrec t = t

  let print = print

  let compare = compare

  let equal = equal

  let hash = hash
end

include Container_types.Make (Self)

let all_bools machine_width =
  Set.of_list [bool_true machine_width; bool_false machine_width]

let zero_one_and_minus_one machine_width =
  Set.of_list [zero machine_width; one machine_width; minus_one machine_width]

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
