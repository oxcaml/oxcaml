(******************************************************************************
 *                                  OxCaml                                    *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2014--2025 Jane Street Group LLC                             *
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

(** Unified interface to [Available_ranges_vars] and
    [Available_ranges_phantom_vars] for a consumer. *)

module Subrange_info : sig
  type t = private
    | Non_phantom of
        { reg : Reg.t;
          offset : Stack_reg_offset.t option
        }
    | Phantom of Linear.phantom_defining_expr
end

module Subrange : sig
  type t

  val info : t -> Subrange_info.t

  val start_pos : t -> Linear.label

  val start_pos_offset : t -> int

  val end_pos : t -> Linear.label

  val end_pos_offset : t -> int
end

module Range_info : sig
  type t

  val provenance : t -> Backend_var.Provenance.t option

  val debuginfo : t -> Debuginfo.t

  val is_parameter : t -> Is_parameter.t

  type phantom_defining_expr = private
    | Non_phantom
    | Phantom of Linear.phantom_defining_expr

  val phantom_defining_expr : t -> phantom_defining_expr
end

module Range : sig
  type t

  val info : t -> Range_info.t

  val extremities : t -> (Linear.label * Linear.label) option

  val fold : t -> init:'a -> f:('a -> Subrange.t -> 'a) -> 'a
end

type t

val empty : t

val create :
  available_ranges_vars:Available_ranges_vars.t ->
  available_ranges_phantom_vars:Available_ranges_phantom_vars.t ->
  Linear.fundecl ->
  t

val iter : t -> f:(Backend_var.t -> Range.t -> unit) -> unit

val fold : t -> init:'a -> f:('a -> Backend_var.t -> Range.t -> 'a) -> 'a
