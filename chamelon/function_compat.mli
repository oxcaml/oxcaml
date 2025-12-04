(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
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

(* Dummy expressions *)

open Typedtree
open Compat

(** [cases_view] is similar to an old version of the [texp_function] type.
   It would take some work to update old clients to use the new [texp_function]
   type, so instead we have this compatibility layer between [cases_view] and
   the new version of [texp_function].

    (Though, at some point we should just update the clients and remove this
    compatibility layer.)
*)

type cases_view_identifier =
  | Cases of texp_function_cases_identifier
  | Param of texp_function_param_identifier

type cases_view = {
  arg_label : Asttypes.arg_label;
  param : Ident.t;
  cases : value case list;
  partial : partial;
  optional_default : expression option;
  cases_view_identifier : cases_view_identifier;
}

val cases_view_to_function : cases_view -> texp_function
val function_to_cases_view : texp_function -> cases_view
