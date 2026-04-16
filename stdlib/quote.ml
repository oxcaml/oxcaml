(******************************************************************************
 *                                  OxCaml                                    *
 *                      Andrej Ivaskovic, Jane Street                         *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

(* CR metaprogramming aivaskovic: This file has not been code reviewed *)

#syntax quotations on

open CamlinternalQuote

module Expr = struct

  let inject_constr x =
    Code.of_exp Loc.unknown
      (Exp.mk (Exp_desc.construct (Constructor.ident x) None) [])

  let bool b =
    let q = match b with
    | true -> inject_constr Identifier.Constructor.true_
    | false -> inject_constr Identifier.Constructor.false_
    in (Obj.magic q : <[bool]> expr)

  let inject x = Code.of_exp Loc.unknown (Exp.mk (Exp_desc.constant x) [])

  let int x = (Obj.magic (inject (Constant.int x)) : <[int]> expr)
  let int32 x = (Obj.magic (inject (Constant.int32 x)) : <[int32]> expr)
  let int64 x = (Obj.magic (inject (Constant.int64 x)) : <[int64]> expr)
  let nativeint x =
    (Obj.magic (inject (Constant.nativeint x)) : <[nativeint]> expr)
  let float x =
    let s = Format.sprintf "%h" x
    in (Obj.magic (inject (Constant.float s)) : <[float]> expr)
  let char x = (Obj.magic (inject (Constant.char x)) : <[char]> expr)
  let string x = (Obj.magic (inject (Constant.string x None)) : <[string]> expr)

end

let print fmt e =
  Format.fprintf fmt "%a" Exp.print (Code.to_exp (Obj.magic e : Code.t))

let string_of_expr e = Format.asprintf "%a" print e
