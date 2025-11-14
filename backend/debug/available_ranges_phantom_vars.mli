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

(** Like [Available_ranges_vars], but with phantom variables associated to
    subranges, rather than normal (non-phantom) variables.

    Phantom variables correspond to variables that once bound computations that
    have now been optimised out.
*)

module Key : Compute_ranges_intf.S_key with type t = Backend_var.t

module Subrange_state : sig
  type t

  val create : unit -> t

  val advance_over_instruction : t -> Linear.instruction -> t
end

module Subrange_info : sig
  type t

  val create :
    Backend_var.t ->
    Subrange_state.t ->
    fun_contains_calls:bool ->
    fun_num_stack_slots:int Stack_class.Tbl.t ->
    t

  val print : Format.formatter -> t -> unit
end

module Range_info : sig
  type t

  val create :
    Linear.fundecl ->
    Backend_var.t ->
    start_insn:Linear.instruction ->
    (Backend_var.t * t) option

  val provenance : t -> Backend_var.Provenance.t option

  val is_parameter : t -> Is_parameter.t

  val defining_expr : t -> Linear.phantom_defining_expr

  val print : Format.formatter -> t -> unit
end

include
  Compute_ranges_intf.S
    with module Index := Backend_var
    with module Key := Key
    with module Subrange_state := Subrange_state
    with module Subrange_info := Subrange_info
    with module Range_info := Range_info
