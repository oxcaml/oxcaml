(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 *                         Basile Clément, OCamlPro                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 OCamlPro                                                *
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

type t =
  | Atom of string
  | List of t list

type sexp = t

val print : Format.formatter -> t -> unit

val from_string : string -> sexp list

module Fields : sig
  type _ t =
    | [] : unit t
    | ( :: ) : (string * (sexp -> 'a)) * 'b t -> ('a * 'b) t

  val parse_list : 'a t -> sexp list -> 'a

  val parse : 'a t -> sexp -> 'a
end

val variant_of_sexp :
  ?default:(string -> sexp list -> 'a) ->
  (string * (sexp list -> 'a)) list ->
  sexp ->
  'a

val ( let+ ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val one_arg : ('a -> 'b) -> 'a list -> 'b

val pair : ('a -> 'b) -> ('a -> 'c) -> 'a list -> 'b * 'c

val string_of_sexp : sexp -> string

val bool_of_sexp : sexp -> bool

val list_of_sexp : (sexp -> 'a) -> sexp -> 'a list
