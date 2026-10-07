(******************************************************************************
 *                                  OxCaml                                    *
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

type t = Jkind_types.Sort.Var.id

let of_sort_var var =
  if not (Jkind_types.Sort.Var.is_root var)
  then Misc.fatal_error "Layout_ident.of_sort_var: not a root";
  Jkind_types.Sort.Var.get_id var

include Identifiable.Make (struct
  type nonrec t = t

  let equal (var1 : t) (var2 : t) = Int.equal (var1 :> int) (var2 :> int)

  let hash (var : t) = Hashtbl.hash (var :> int)

  let compare (var1 : t) (var2 : t) = Int.compare (var1 :> int) (var2 :> int)

  let output oc (var : t) =
    output_string oc "s_";
    output_binary_int oc (var :> int)

  let print ppf (var : t) = Format.fprintf ppf "layout_%i" (var :> int)
end)
