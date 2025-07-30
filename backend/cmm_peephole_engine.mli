(******************************************************************************
 *                             flambda-backend                                *
 *                       Vincent Laviron, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 OCamlPro SAS                                   *
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

type _ pattern_kind =
  | Expr : Cmm.expression pattern_kind
  | Int : int pattern_kind
  | Natint : Nativeint.t pattern_kind

type 'a pattern_var

val create_var : 'a pattern_kind -> string -> 'a pattern_var

module Default_variables : sig
  val c : Cmm.expression pattern_var

  val c1 : Cmm.expression pattern_var

  val c2 : Cmm.expression pattern_var

  val n : int pattern_var

  val n1 : int pattern_var

  val n2 : int pattern_var
end

module Env : sig
  type t
end

type binop =
  | Add
  | Sub
  | Lsl
  | Lsr
  | Asr
  | Or
  | And
  | Comparison
  | Bitwise_op

type cmm_pattern =
  | Any of Cmm.expression pattern_var
  | As of Cmm.expression pattern_var * cmm_pattern
  | Const_int_fixed of int
  | Const_int of int pattern_var
  | Const_natint_fixed of Nativeint.t
  | Const_natint of Nativeint.t pattern_var
  | Binop of binop * cmm_pattern * cmm_pattern
  | Guarded of
      { pat : cmm_pattern;
        guard : Env.t -> bool
      }

type 'a clause

val run : Cmm.expression -> Cmm.expression clause list -> Cmm.expression

val run_default :
  default:(Cmm.expression -> 'a) -> Cmm.expression -> 'a clause list -> 'a

module Syntax : sig
  val ( => ) : cmm_pattern -> (Env.t -> 'a) -> 'a clause

  val ( #. ) : Env.t -> 'a pattern_var -> 'a
end
