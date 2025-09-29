(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Additional information about kinds of numbers (mainly relating to
    conversions and boxing/unboxing) in a standard form that can be fed to
    functors parametric in number kinds. *)

module type Num_common = sig
  include Container_types.S

  module Pair : sig
    type nonrec t = t * t

    include Container_types.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

  val zero : Target_system.Machine_width.t -> t

  val one : Target_system.Machine_width.t -> t

  val minus_one : Target_system.Machine_width.t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t option

  val mod_ : t -> t -> t option

  val to_const : t -> Reg_width_const.t

  val to_immediate : t -> Target_system.Machine_width.t -> Target_ocaml_int.t

  val to_naked_float32 : t -> Numeric_types.Float32_by_bit_pattern.t

  val to_naked_float : t -> Numeric_types.Float_by_bit_pattern.t

  val to_naked_int8 : t -> Numeric_types.Int8.t

  val to_naked_int16 : t -> Numeric_types.Int16.t

  val to_naked_int32 : t -> Numeric_types.Int32.t

  val to_naked_int64 : t -> Numeric_types.Int64.t

  val to_naked_nativeint :
    t -> Target_system.Machine_width.t -> Targetint_32_64.t
end

module type Number_kind_common = sig
  module Num : Container_types.S

  val standard_int_or_float_kind : Flambda_kind.Standard_int_or_float.t

  val unboxed_prover :
    Flambda2_types.Typing_env.t ->
    Flambda2_types.t ->
    Num.Set.t Flambda2_types.meet_shortcut

  val this_unboxed : Num.t -> Flambda2_types.t

  val these_unboxed : Num.Set.t -> Flambda2_types.t

  val term_unboxed : Num.t -> Flambda.Named.t
end

module type Number_kind = sig
  module Num : Num_common

  include Number_kind_common with module Num := Num
end

module type Int_number_kind = sig
  module Num : sig
    include Num_common

    val and_ : t -> t -> t

    val or_ : t -> t -> t

    val xor : t -> t -> t

    val shift_left : t -> Target_ocaml_int.t -> t

    (* [shift_right] is arithmetic shift right, matching [Int32], [Int64],
       etc. *)
    val shift_right : t -> Target_ocaml_int.t -> t

    val shift_right_logical : t -> Target_ocaml_int.t -> t

    val swap_byte_endianness : t -> t

    val neg : t -> t

    val compare_unsigned : t -> t -> int
  end

  include Number_kind_common with module Num := Num

  val standard_int_kind : Flambda_kind.Standard_int.t
end

module type Boxable = sig
  module Num : Container_types.S

  val boxable_number_kind : Flambda_kind.Boxable_number.t

  val this_boxed : Num.t -> Alloc_mode.For_types.t -> Flambda2_types.t

  val these_boxed : Num.Set.t -> Alloc_mode.For_types.t -> Flambda2_types.t

  val box : Flambda2_types.t -> Alloc_mode.For_types.t -> Flambda2_types.t
end

module type Boxable_number_kind = sig
  include Number_kind

  include Boxable with module Num := Num
end

module type Boxable_int_number_kind = sig
  include Int_number_kind

  include Boxable with module Num := Num
end

module For_tagged_immediates : Int_number_kind

module For_naked_immediates : Int_number_kind

module For_float32s : Boxable_number_kind
(* Float32s in JSOO are actually normal 64-bit floats, and each operation is
   performed in 64-bit precision and then rounded to the nearest 32-bit float.
   Since this is not identical to true 32-bit operations, there is a mismatch
   between the semantics of [For_float32s] and the semantics of the JSIR target.
   However, this should be okay: IEEE 754 only fully specifies the result of
   certain operations such as sqrt and rint [1], which seem to be exact in our
   setting [2, Table 1]. Other operations are only "recommended", and indeed
   many libraries produce different errors [3].

   [1]
   https://sourceware.org/glibc/manual/2.42/html_node/Errors-in-Math-Functions.html

   [2] https://dl.acm.org/doi/pdf/10.1145/221332.221334

   [3] https://inria.hal.science/hal-03141101 *)

module For_floats : Boxable_number_kind

module For_int8s : Int_number_kind

module For_int16s : Int_number_kind

module For_int32s : Boxable_int_number_kind

module For_int64s : Boxable_int_number_kind

module For_nativeints : Boxable_int_number_kind
