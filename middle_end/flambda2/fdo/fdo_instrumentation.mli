(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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

(** The FDO counters (see [Fdo_counter]) of the calls and switch arms of a
    function, attached during the translation from Lambda.

    Counters are numbered by visiting the Lambda body: the index of a call or
    switch is its position in the order of the translation, which only depends
    on the body (whose body hash guards it), not on how the translation compiles
    it. Calls and switches that the translation introduces (e.g. bounds checks,
    or the dispatch on whether a variant is an immediate) are not in the source
    and get no counters of their own. *)

type t

(** The instrumentation of the body of the given function. *)
val create :
  function_:Fdo_counter.Function.t -> body_hash:Fdo_counter.Body_hash.t -> t

(** The counters of the entry edge of the function, with its body hash. *)
val entry_annotation : t -> Fdo_annotation.With_body_hash.t

(** The counter of a call of the source program ([Lapply] or [Lsend]) at the
    given location. *)
val call : t -> Lambda.scoped_location -> Fdo_counter.t

(** The counters of the arms of a switch the translation emits. *)
module Switch_counters : sig
  type t

  (** The counters of the arm of the given discriminant. A discriminant without
      an arm of its own goes to the default arm, and gets its counters. *)
  val case : t -> int -> Fdo_annotation.t

  (** The counters of the default arm. *)
  val failaction : t -> Fdo_annotation.t
end

(** The counters of the switches a [Lambda] switch translates to. *)
module Lambda_switch_counters : sig
  type t

  (** Constants *)
  val consts : t -> Switch_counters.t

  (** Tags of the blocks *)
  val blocks : t -> Switch_counters.t

  (** Dispatch between [blocks] and [consts] on whether the scrutinee is an
      immediate. Normally doesn't have counters, but if [blocks] or [consts]
      collapse into a single arm, their counters are attached here instead. *)
  val is_int : t -> Switch_counters.t
end

(** Arm 0 of a [Lambda] switch is its default arm, followed by its constants and
    then its tags. *)
val switch : t -> Lambda.lambda_switch -> Lambda_switch_counters.t
