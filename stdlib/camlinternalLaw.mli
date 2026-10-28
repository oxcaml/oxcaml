# 2 "camlinternalLaw.mli"
(******************************************************************************
 *                                  OxCaml                                    *
 *                          Simon Spies, Jane Street                          *
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

(** The representation of laws in generated laws files, interpreted by a
    testing backend. Not for end-user use. *)

module type Choice = sig
  type 'a t
end

(** A range of the interface declaring the laws, as the compiler was given
    the file. Lines count from 1 and columns from 0; the end is exclusive.
    [Assumption] and [Assertion] locate their clause; the [locations] of a
    generated file locate each law, from [law?] to its end. *)
type location =
  { file : string;
    start_line : int;
    start_col : int;
    end_line : int;
    end_col : int;
  }

module type S = sig
  type 'a choice

  type 'a t =
    | Return : 'a -> 'a t
    | Bind : 'x t * ('x -> 'a t) -> 'a t
    | Choose : 'a choice -> 'a t
    | Assumption :
        { value : 'x; source : 'x choice; predicate : 'x -> bool;
          location : location }
        -> unit t
    | Assertion :
        { value : 'x; source : 'x choice; predicate : 'x -> bool;
          location : location }
        -> unit t
end

module Make (Choice : Choice) : S with type 'a choice = 'a Choice.t
