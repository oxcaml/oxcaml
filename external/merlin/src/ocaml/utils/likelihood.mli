(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

(** This modules defines the type for static likelihood information on switch
    arms. A likelihood for an arm represents an estimate of the (unnormalized)
    probability for any execution reaching the switch containing that arm to
    select that arm. In other words, the frequency with which we select the
    arm is the reation of its likelihood to the sum of the likelihoods of all
    arms.

    The use of unnormalized probabilities (weights) allows to freely delete
    switch arms without having to recompute normalized probabilities.

    As a special case, "cold" arms are supported: they are expected to be chosen
    a minuscule amount of time compared to the other arms and effectively have
    probability 0.
 *)

type t

val cold : t

val default : t
(** [default] is [from_weight 1.] *)

(** Construct a likelihood from a weight (unnormalized probability). *)
val from_weight : float -> t

type classification =
  | Cold
  | Weight of float

val classify : t -> classification

val is_cold : t -> bool

val rescale : total:t -> t -> t

val sum_list : t list -> t

val is_uniform : t list -> bool
(** [is_uniform l] is [true] iff all elements in the list that are not [cold]
have the same relative likelihood. *)
