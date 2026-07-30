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

(* A quotation value [_ expr] is represented at run time by a
   [CamlinternalParsetree.expression] (built by [Translquotes]); it is printed
   with the copy of [Pprintast] linked into the standard library. *)

#syntax quotations on

open CamlinternalParsetree

let none = CamlinternalLocation.none

let mk pexp_desc =
  { pexp_desc; pexp_loc = none; pexp_loc_stack = []; pexp_attributes = [] }

let const pconst_desc = mk (Pexp_constant { pconst_desc; pconst_loc = none })

module Expr = struct
  let bool b : <[bool]> expr =
    let name = if b then "true" else "false" in
    let txt = CamlinternalLongident.Lident name in
    let lid = { CamlinternalLocation.txt; loc = none } in
    Obj.magic (mk (Pexp_construct (lid, None)))

  let int x : <[int]> expr =
    Obj.magic (const (Pconst_integer (string_of_int x, None)))
  let int32 x : <[int32]> expr =
    Obj.magic (const (Pconst_integer (Int32.to_string x, Some 'l')))
  let int64 x : <[int64]> expr =
    Obj.magic (const (Pconst_integer (Int64.to_string x, Some 'L')))
  let nativeint x : <[nativeint]> expr =
    Obj.magic (const (Pconst_integer (Nativeint.to_string x, Some 'n')))
  let float x : <[float]> expr =
    Obj.magic (const (Pconst_float (Printf.sprintf "%h" x, None)))
  let char x : <[char]> expr = Obj.magic (const (Pconst_char x))
  let string x : <[string]> expr =
    Obj.magic (const (Pconst_string (x, none, None)))
end

let duplicate e =
  let e = Obj.magic_many e in
  e, e

let print fmt e =
  let e : expression = Obj.magic e in
  CamlinternalPprintast.expression fmt (CamlinternalPprintast.normalize_quote e)

let string_of_expr e = Format.asprintf "%a" print (Obj.magic_many e)
