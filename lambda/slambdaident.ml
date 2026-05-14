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

open Local_store

type t =
  | Local of
      { name : string;
        stamp : int
      }
  | Ident of Ident.t

let currentstamp = s_ref 0

let create_local name =
  incr currentstamp;
  Local { name; stamp = !currentstamp }

let of_ident ident = Ident ident

let equal i1 i2 =
  match i1, i2 with
  | Local { stamp = s1; _ }, Local { stamp = s2; _ } -> s1 = s2
  | Ident i1, Ident i2 -> Ident.equal i1 i2
  | _ -> false

let hash = function
  | Local { stamp = s; _ } -> Hashtbl.hash (0, s)
  | Ident i -> Hashtbl.hash (1, Ident.hash i)

let compare i1 i2 =
  match i1, i2 with
  | Local { stamp = s1; _ }, Local { stamp = s2; _ } -> compare s1 s2
  | Local _, _ -> 1
  | _, Local _ -> -1
  | Ident i1, Ident i2 -> Ident.compare i1 i2

let output oc =
  let open Format in
  function
  | Local { name; stamp } -> output_string oc (sprintf "s_%s_%i" name stamp)
  | Ident ident ->
    output_string oc "l_";
    Ident.output oc ident

let print ppf =
  let open Format in
  function
  | Local { name; stamp } ->
    fprintf ppf "#%s%s" name
      (if !Clflags.unique_ids then sprintf "/%i" stamp else "")
  | Ident ident -> fprintf ppf "%a" Ident.print ident

include Identifiable.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = hash

  let compare = compare

  let output = output

  let print = print
end)
