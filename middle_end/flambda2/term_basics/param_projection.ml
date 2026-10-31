(******************************************************************************
 *                                  OxCaml                                    *
 *                        Pierre Chambart, OCamlPro                           *
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

type source =
  | Param of int
  | My_closure

type projection_key =
  | Get_tag
  | Untag
  | Is_int
  | Block_load of
      { field : Target_ocaml_int.t;
        kind : Flambda_kind.t
      }

type projection =
  | Identity
  | Projection of projection_key * projection

type t =
  { source : source;
    projection : projection
  }

let param i = { source = Param i; projection = Identity }

let my_closure = { source = My_closure; projection = Identity }

let get_tag p = { p with projection = Projection (Get_tag, p.projection) }

let untag p = { p with projection = Projection (Untag, p.projection) }

let is_int p = { p with projection = Projection (Is_int, p.projection) }

let block_load field kind p =
  { p with projection = Projection (Block_load { field; kind }, p.projection) }

let compare_source s1 s2 =
  match s1, s2 with
  | Param i1, Param i2 -> Int.compare i1 i2
  | My_closure, My_closure -> 0
  | Param _, My_closure -> -1
  | My_closure, Param _ -> 1

module Key = struct
  type t = projection_key

  let compare p1 p2 =
    match p1, p2 with
    | Get_tag, Get_tag -> 0
    | Untag, Untag -> 0
    | Is_int, Is_int -> 0
    | Block_load { field = f1; kind = k1 }, Block_load { field = f2; kind = k2 }
      -> (
      match Target_ocaml_int.compare f1 f2 with
      | 0 -> Flambda_kind.compare k1 k2
      | c -> c)
    | Get_tag, _ -> -1
    | _, Get_tag -> 1
    | Untag, _ -> -1
    | _, Untag -> 1
    | Is_int, _ -> -1
    | _, Is_int -> 1

  let[@ocamlformat "disable"] print ppf = function
    | Get_tag -> Format.fprintf ppf "Get_tag"
    | Untag -> Format.fprintf ppf "Untag"
    | Is_int -> Format.fprintf ppf "Is_int"
    | Block_load { field; kind } ->
        Format.fprintf ppf "(Block_load %a %a)"
          Target_ocaml_int.print field
          Flambda_kind.print kind
end

module Map = Map.Make (Key)

let rec compare_projection p1 p2 =
  match p1, p2 with
  | Identity, Identity -> 0
  | Identity, Projection _ -> -1
  | Projection _, Identity -> 1
  | Projection (k1, p1), Projection (k2, p2) -> (
    match Key.compare k1 k2 with 0 -> compare_projection p1 p2 | c -> c)

let compare t1 t2 =
  match compare_source t1.source t2.source with
  | 0 -> compare_projection t1.projection t2.projection
  | c -> c

let equal t1 t2 = compare t1 t2 = 0

let[@ocamlformat "disable"] print_source ppf = function
  | Param i -> Format.fprintf ppf "(Param %d)" i
  | My_closure -> Format.fprintf ppf "My_closure"

let[@ocamlformat "disable"] rec print_projection source ppf = function
  | Identity -> source ppf
  | Projection (key, p) ->
      Format.fprintf ppf "(%a %a)" Key.print key
        (print_projection source) p

let print ppf t =
  print_projection (fun ppf -> print_source ppf t.source) ppf t.projection

let print_projection ppf projection =
  print_projection (fun ppf -> Format.fprintf ppf "Identity") ppf projection
