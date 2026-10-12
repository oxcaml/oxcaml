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

(** Annotations for a given branch (i.e. switch arm). *)
type t

val create : likelihood:Likelihood.t -> fdo_annotation:Fdo_annotation.t -> t

val from_likelihood : Likelihood.t -> t

val default : t

val cold : t

(** Likelihood of this branch being selected, relative to a specific set of
    potential branches.

    This is the (unnormalized) conditional probability of reaching the branch
    target (continuation), knowing that we have reached its source (switch
    expression). *)
val likelihood : t -> Likelihood.t

val with_likelihood : t -> Likelihood.t -> t

(** The FDO counters of the edge this branch takes. See [Fdo_annotation]. *)
val fdo_annotation : t -> Fdo_annotation.t

val with_fdo_annotation : t -> Fdo_annotation.t -> t

(** Combine multiple branch annotations into annotations for a shared branch.

    The returned branch information must be valid for an intermediate jump
    target that captures all the provided branches (this may be followed by a
    branching point to dispatch to the provided branches, but not necessarily if
    the branches have been found to be identical and shared). The shared branch
    carries the FDO counters of all the provided branches. *)
val sum_list : t list -> t

(** [add t1 t2] is [sum_list [t1; t2]]. *)
val add : t -> t -> t
