(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module defines:
    The scalar types intrinsic to the OCaml compiler, and all of the
    primitive operations defined on them.

    Overview: This module provides a comprehensive type system for scalar values in OCaml.

    Type Hierarchy:
    - Scalar.t
      - Integral: Integer types
        - Taggable (fits in word_size-1 bits): Int8, Int16, Int
        - Boxable (requires boxing): Int32, Int64, Nativeint
      - Floating: Float32, Float64

    Representation Forms:
    - Value: Tagged (small integers) or boxed representation
    - Naked: Untagged (<=31 bits) or unboxed (>31 bits) representation

    Operations:
    - Unary: Negation, successor/predecessor, byte swap, static cast
    - Binary: Arithmetic (add, sub, mul, div), bitwise (and, or, xor),
             shift (lsl, asr, lsr), comparisons (integer and float)

    The locality parameter tracks where boxed values are allocated.

    A [Scalar.t] represents a particular OCaml type that represents a scalar value. It
    might be tagged, boxed, or neither. There is also a type parameter for the locality of
    the scalar value, which represents the location in which that boxed values are
    allocated.

    The important consideration is for a [Scalar.t] to represent all of the argument and
    return types of all the primitives that we want to support. The submodules are
    organized to make it easy for use different subsets of scalars in different places.
    Some examples:

    - Primitive arguments don't depend on the locality of their arguments, but the results
    do. This is achieved using [ignore_locality : _ t -> any_locality_mode t] to convert
    arguments to [any_locality_mode] when defining primitives, e.g.:
      [Icmp (Scalar.Integral.ignore_locality size, cmp)]

    - Some primitives only take integers, some take only floats, and Three_way_compare
    takes any scalar type. This is represented using specialized operation types:
      - [Intrinsic.Binary.Int_op.t] for integer-only ops (Add, Sub, Mul, etc.)
      - [Intrinsic.Binary.Float_op.t] for float-only ops
      - [Intrinsic.Binary.Three_way_compare of any_locality_mode t] accepts any scalar

    - The bytecode compiler wants to easily map unboxed/untagged values to their [value]
    equivalents. This is supported by [Maybe_naked.t]:
      - [Value of 'a] represents boxed/tagged values
      - [Naked of 'b] represents unboxed/untagged values
      Example: [Scalar.Maybe_naked.Value (Integral.Width.Taggable Int)] vs
               [Scalar.Maybe_naked.Naked (Integral.Width.Taggable Int)]

    - The middle-end wants to easily cast between any integral values using only certain
    primitives. This is enabled by [Intrinsic.Unary.Static_cast]:
      [Static_cast { src : any_locality_mode t; dst : 'mode t }]
      which allows casting between different scalar types, e.g., from Int8 to Int32.
      See jane/doc/scalars.md for documentation on the semantics.

    In some cases, only the number of bits used in the container (e.g. a
    machine register or stack slot) is important.  We use [Width] for this.
    [Width.t] always represents the width of a scalar layout -- it has exactly one
    constuctor for each bit-width of the corresponding scalar. The type
    parameter is irrelevant within the [Width] module, that's why it's usually
    fixed to any_locality_mode when the corresponding function is expecting a
    simple sum type (e.g., [to_string]).
*)

type any_locality_mode = Any_locality_mode

val equal_any_locality_mode : any_locality_mode -> any_locality_mode -> bool

module Maybe_naked : sig
  type ('a, 'b) t =
    | Value of 'a
    | Naked of 'b
        (** "Naked" means either untagged or unboxed, depending whether the type fits in
            31 bits or not. e.g., [Naked Int8] is untagged, but [Naked (Int32
            Any_locality_mode)] would be unboxed *)
end

val naked : 'a -> (_, 'a) Maybe_naked.t

module type S := sig
  type 'a width

  type nonrec 'a t = ('a width, any_locality_mode width) Maybe_naked.t

  val all : any_locality_mode t list

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val ignore_locality : _ t -> any_locality_mode t

  val width : _ t -> any_locality_mode width

  val to_string : any_locality_mode t -> string

  val sort : any_locality_mode t -> Jkind_types.Sort.Const.t

  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
end

(* The following module types define convenient shorthand values for users of scalar.ml,
   so that they don't have to use the large variant constructors. *)

module Integral : sig
  module Taggable : sig
    module Width : sig
      type t =
        | Int8
        | Int16
        | Int

      val to_string : t -> string
    end

    include S with type 'a width := Width.t
  end

  module Boxable : sig
    module Width : sig
      type 'mode t =
        | Int32 of 'mode
        | Nativeint of 'mode
        | Int64 of 'mode

      val map : 'a t -> f:('a -> 'b) -> 'b t

      val to_string : any_locality_mode t -> string
    end

    include S with type 'a width := 'a Width.t
  end

  module Width : sig
    type 'mode t =
      | Taggable of Taggable.Width.t
      | Boxable of 'mode Boxable.Width.t

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  include S with type 'a width := 'a Width.t
end

module Floating : sig
  module Width : sig
    type 'mode t =
      | Float32 of 'mode
      | Float64 of 'mode

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val to_string : any_locality_mode t -> string
  end

  include S with type 'a width := 'a Width.t
end

module Width : sig
  type 'mode t =
    | Floating of 'mode Floating.Width.t
    | Integral of 'mode Integral.Width.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val to_string : any_locality_mode t -> string

  val ignore_locality : _ t -> any_locality_mode t
end

include S with type 'a width := 'a Width.t

val integral : 'a Integral.t -> 'a t

val floating : 'a Floating.t -> 'a t

module Integer_comparison : sig
  type t =
    | Ceq
    | Cne
    | Clt
    | Cgt
    | Cle
    | Cge

  val to_string : t -> string

  val swap : t -> t

  val negate : t -> t
end

module Float_comparison : sig
  type t =
    | CFeq
    | CFneq
    | CFlt
    | CFnlt
    | CFgt
    | CFngt
    | CFle
    | CFnle
    | CFge
    | CFnge

  val to_string : t -> string

  val swap : t -> t

  val negate : t -> t
end

type 'a scalar := 'a t

module Operation : sig
  type 'mode info =
    { can_raise : bool;
      result : 'mode t
    }

  module Unary : sig
    module Int_op : sig
      type t =
        | Neg
        | Succ  (** add 1 *)
        | Pred  (** subtract 1 *)
        | Bswap  (** byte swap; see scalars.md for semantics *)

      val to_string : t -> string
    end

    module Float_op : sig
      type t =
        | Neg
        | Abs

      val to_string : t -> string
    end

    type 'mode t =
      | Integral of 'mode Integral.t * Int_op.t
      | Floating of 'mode Floating.t * Float_op.t
      | Static_cast of
          { src : any_locality_mode scalar;
            dst : 'mode scalar
          }
          (** [Static_cast] performs a conversion between numeric types, which
              may include (un)tagging or (un)boxing.  The jane/doc/scalars.md
              file contains detailed documentation of the semantics.  The
              middle end expands the static cast operations into finer-grained
              primitives (for example %int64_as_int, which is represented
              as a static case, will turn into an unboxing followed by an
              int64 -> int conversion, the latter of which takes the integer
              modulo 2^63 (2^31 on 32-bit targets). *)

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val info : 'a t -> 'a info

    val sort :
      any_locality_mode t -> Jkind_types.Sort.Const.t * Jkind_types.Sort.Const.t
  end

  module Binary : sig
    module Int_op : sig
      type division_is_safe =
        | Safe
        | Unsafe

      type t =
        | Add
        | Sub
        | Mul
        | Div of division_is_safe
        | Mod of division_is_safe
        | And
        | Or
        | Xor

      val to_string : t -> string
    end

    module Shift_op : sig
      module Rhs : sig
        type t = Int
      end

      type t =
        | Lsl
        | Asr
        | Lsr

      val to_string : t -> string
    end

    module Float_op : sig
      type t =
        | Add
        | Sub
        | Mul
        | Div

      val to_string : t -> string
    end

    (* CR jvanburen: add comparisons that return naked values *)

    (** comparisons return a tagged immediate *)
    type 'mode t =
      | Integral of 'mode Integral.t * Int_op.t
      | Shift of 'mode Integral.t * Shift_op.t * Shift_op.Rhs.t
      | Floating of 'mode Floating.t * Float_op.t
      | Icmp of any_locality_mode Integral.t * Integer_comparison.t
      | Fcmp of any_locality_mode Floating.t * Float_comparison.t
      | Three_way_compare of any_locality_mode scalar

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val info : 'a t -> 'a info

    val sort :
      any_locality_mode t ->
      Jkind_types.Sort.Const.t
      * Jkind_types.Sort.Const.t
      * Jkind_types.Sort.Const.t
  end

  type 'mode t =
    | Unary of 'mode Unary.t
    | Binary of 'mode Binary.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val sort : any_locality_mode t -> Jkind_types.Sort.Const.t list

  val arity : _ t -> int

  val info : 'a t -> 'a info

  val to_string : any_locality_mode t -> string

  module With_percent_prefix : sig
    type nonrec t = any_locality_mode t

    val to_string : t -> string

    val of_string : string -> t
  end
end
