(******************************************************************************
 *                                   OxCaml                                   *
 *                          Basile Clément, OCamlPro                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 OCamlPro                                                *
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

type t

include Container_types.S with type t := t

val create : Bound_parameter.t -> t

val var : t -> Variable.t

val simple : t -> Simple.t

val kind : t -> Flambda_kind.With_subkind.t

val rename_list_as_bound_parameters : t list -> Renaming.t * Bound_parameters.t

(** [place_lifted ~is_exn_handler ~lifted l] adds lifted variables (parameters
    or arguments) from [lifted] to the list [l].

    Lifted variables come first, except in exception handlers, where they come
    last, because the first param must remain the exception bucket. *)
val place_lifted : is_exn_handler:bool -> lifted:'a list -> 'a list -> 'a list
