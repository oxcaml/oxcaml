(******************************************************************************
 *                                  OxCaml                                    *
 *                        Simon Spies, Jane Street                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025--2026 Jane Street Group LLC                             *
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

module RS = Runtime_shape
module Layout = Jkind_types.Sort.Const

(** Complex shape types.

    Complex shapes represent type layouts that may include unboxed products and
    void types, in addition to "simple" runtime shapes that fit in a single
    register. The term "complex" follows Flambda 2's naming convention (see
    [Flambda_arity.Component_for_creation] in middle_end/flambda2/kinds), where
    "complex" arities can contain unboxed products, as opposed to "singleton"
    arities that describe single-register values.

    A complex shape can be:
    - [Runtime rs]: A runtime shape that has a concrete runtime representation
    - [Void]: A value with no runtime representation
    - [Unboxed_product]: A composite structure containing multiple components,
      each of which is itself a complex shape

    Complex shapes are used during DWARF type generation to accurately describe
    OxCaml types with unboxed layouts before they are flattened into sequences
    of runtime shapes. *)

type t = private
  { desc : desc;
    layout : Layout.t;
    hash : int
  }

and desc = private
  | Runtime of RS.t
  | Void
  | Unboxed_product of
      { kind : unboxed_product_kind;
        components : t list
      }

and unboxed_product_kind = private
  | Unboxed_record of string list
  | Unboxed_tuple

val runtime : RS.t -> t

val unboxed_tuple : t list -> t

val record_unboxed : (string * t) list -> t

val void : t

val to_layout : t -> Layout.t

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val hash : t -> int

(** Extract the runtime shape from a complex shape, if it is a simple runtime
    shape. Returns [None] for [Void] and [Unboxed_product] variants. *)
val runtime_shape : t -> RS.t option

(** Flatten a complex shape into a list of runtime shapes and voids. Can be used
    to simulate the effect of unarization on function arguments. *)
val flatten_complex_shape : t -> RS.t RS.Or_void.t list

(** Convert a type shape and layout to a complex shape for DWARF type
    generation. Requires the shape to be fully reduced and evaluated beforehand.
*)
val type_shape_to_complex_shape : Shape.t -> Layout.t -> t
