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

(* CR mshinwell: This file needs to be code reviewed *)

module MOF = Measure_object_files

type kind =
  | Main
  | Large_code of int

let symbol_prefix = function
  | Main -> "main"
  | Large_code n -> Printf.sprintf "p%d" n

let section_prefix = function
  | Main -> ""
  | Large_code n -> Printf.sprintf ".caml.p%d" n

type t =
  { kind : kind;
    files : MOF.File_size.t list;
    total_size : int64
  }

let kind t = t.kind

let files t = t.files

let total_size t = t.total_size

let create ~kind files =
  let total_size =
    List.fold_left
      (fun acc entry -> Int64.add acc (MOF.File_size.size entry))
      0L files
  in
  { kind; files; total_size }

module Linked = struct
  type partition = t

  type t =
    { partition : partition;
      linked_object : string
    }

  let partition t = t.partition

  let linked_object t = t.linked_object

  let create ~partition ~linked_object = { partition; linked_object }
end
