(******************************************************************************
 *                                  OxCaml                                    *
 *                        Jacob Van Buren, Jane Street                        *
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

type t = { frame_pointers : bool }

let flags_variant () = { frame_pointers = !Clflags.frame_pointers }

let default = { frame_pointers = Config.with_frame_pointers }

(* The axes describe native code generation only.  A JSIR compilation
   (-target js_of_ocaml or wasm_of_ocaml) emits no native code, and the
   JSIR libraries are installed once, in the default library root, so such
   a compilation always counts as the default variant whatever the axis
   flags say: its artifacts carry the default fingerprint and its library
   root is never remapped. *)
let current () =
  if Clflags.target_is_jsir () then default else flags_variant ()

let equal t1 t2 = Bool.equal t1.frame_pointers t2.frame_pointers

let is_default t = equal t default

let to_string t = if t.frame_pointers then "fp" else "base"

let of_string = function
  | "fp" -> Some { frame_pointers = true }
  | "base" -> Some { frame_pointers = false }
  | _ -> None

type fingerprint = (string * string) list

let fingerprint t =
  [ "frame_pointers", string_of_bool t.frame_pointers;
    "address_sanitizer", string_of_bool Config.with_address_sanitizer ]

let current_fingerprint () = fingerprint (current ())

let equal_fingerprint f1 f2 =
  let sort f = List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) f in
  List.equal
    (fun (n1, v1) (n2, v2) -> String.equal n1 n2 && String.equal v1 v2)
    (sort f1) (sort f2)

let string_of_fingerprint f =
  String.concat ", " (List.map (fun (n, v) -> n ^ "=" ^ v) f)
