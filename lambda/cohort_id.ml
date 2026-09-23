(******************************************************************************
 *                                  OxCaml                                    *
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

type t =
  { cu : Compilation_unit.t;
    name : string
  }

let enabled = ref true

let create cu name = { cu; name }

let compilation_unit t = t.cu

let name t = t.name

let equal t1 t2 =
  Compilation_unit.equal t1.cu t2.cu && String.equal t1.name t2.name

let compare t1 t2 =
  let c = Compilation_unit.compare t1.cu t2.cu in
  if c <> 0 then c else String.compare t1.name t2.name

let hash t = Hashtbl.hash (Compilation_unit.hash t.cu, t.name)

let print ppf t =
  Format.fprintf ppf "%s/%s" (Compilation_unit.full_path_as_string t.cu) t.name

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

(* Only determinism matters for the mangled name; it is sanitised purely for
   legibility in symbol tables. *)
let sanitise name =
  String.map
    (fun c ->
      match c with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> c | _ -> '_')
    name

let linkage_name t ~suffix =
  Symbol.for_name t.cu ("cohort__" ^ sanitise t.name ^ suffix)
  |> Symbol.linkage_name

let code_linkage_name t = linkage_name t ~suffix:"_code"

let closure_linkage_name t = linkage_name t ~suffix:""
