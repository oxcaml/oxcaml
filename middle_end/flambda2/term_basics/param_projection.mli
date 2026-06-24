(******************************************************************************
 *                                  OxCaml                                    *
 *                        Pierre Chambart, OCamlPro                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

(** Symbolic description of a value as a projection out of a function parameter
    (or [my_closure]).

    For example, the result of [%get_tag] applied to the untagged form of the
    first parameter is described by [get_tag (untag (param 0))]. *)

type source =
  | Param of int
  | My_closure

type projection_key =
  | Get_tag
  | Untag
  | Is_int
  | Block_load of
      { field : Target_ocaml_int.t;
        kind : Flambda_kind.t
      }

type projection =
  | Identity
  | Projection of projection_key * projection

type t =
  { source : source;
    projection : projection
  }

module Key : sig
  type t = projection_key

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit
end

module Map : Map.S with type key = Key.t and type 'a t = 'a Map.Make(Key).t

val compare_projection : projection -> projection -> int

val param : int -> t

val my_closure : t

val get_tag : t -> t

val untag : t -> t

val is_int : t -> t

val block_load : Target_ocaml_int.t -> Flambda_kind.t -> t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val print_projection : Format.formatter -> projection -> unit

val print : Format.formatter -> t -> unit
