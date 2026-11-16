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

let apply t = Clflags.frame_pointers := t.frame_pointers

(* Naming goes through the dependency-free [Codegen_variant_name] token
   scheme; a new axis extends [name]/[of_name] alongside [t]. *)
let name t =
  Codegen_variant_name.of_tokens
    (if t.frame_pointers then [Codegen_variant_name.Frame_pointers] else [])

let of_name n =
  { frame_pointers = Codegen_variant_name.(mem Frame_pointers n) }

let to_string t = Codegen_variant_name.to_string (name t)

let of_string s = Option.map of_name (Codegen_variant_name.of_string s)

let runtime_archive_suffix t =
  Codegen_variant_name.runtime_archive_suffix (name t) ~default:(name default)

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

let variant_root t =
  Filename.concat
    (Filename.concat Config.standard_library "variants")
    (to_string t)

let directory_exists dir = try Sys.is_directory dir with Sys_error _ -> false

let effective_standard_library () =
  let t = current () in
  if is_default t
  then Config.standard_library
  else
    (* Remap only when the variant tree is present.  When it is absent the
       base root may itself serve the requested variant (the in-tree build
       compiles with -frame-pointers against an fp stdlib via OCAMLLIB, and
       users may point OCAMLLIB at a custom-built variant tree); genuinely
       wrong roots are caught by the artifact-fingerprint check at the first
       .cmx import and by the runtime-archive lookup at link, which append
       [missing_root_hint]. *)
    let root = variant_root t in
    if directory_exists root then root else Config.standard_library

let missing_root_hint () =
  let t = current () in
  if is_default t || directory_exists (variant_root t)
  then None
  else
    Some
      (Printf.sprintf "note: %s does not contain codegen variant '%s'"
         Config.standard_library (to_string t))

type directory_error =
  { directory : string;
    variant : string;
    variant_root : string
  }

exception Directory_not_provided_by_variant of directory_error

let is_plus_directory s = String.length s > 0 && s.[0] = '+'

let expand_directory_unchecked s =
  if is_plus_directory s
  then Misc.expand_directory (effective_standard_library ()) s
  else s

let expand_directory s =
  if is_plus_directory s
  then begin
    let root = effective_standard_library () in
    let expanded = Misc.expand_directory root s in
    if String.equal root Config.standard_library || directory_exists expanded
    then expanded
    else if
      (* The selected variant's tree exists but lacks this subdirectory.
         When the DEFAULT root provides it, the include would silently
         resolve to a nonexistent directory while a directory of that name
         exists one root over, so fail with a targeted error naming what
         is missing from the selected root.  (Nothing is assumed about the
         default root's copy: whether its artifacts suit this variant is
         for the fingerprint checks to decide if the caller names it by
         absolute path.)  Directories absent from both roots keep the
         permissive expansion (a nonexistent include directory is not an
         error). *)
      directory_exists (Misc.expand_directory Config.standard_library s)
    then
      raise
        (Directory_not_provided_by_variant
           { directory = s;
             variant = to_string (current ());
             variant_root = root
           })
    else expanded
  end
  else s

(* [Clflags] computes the -I flag for the standard library but cannot depend
   on this module (this module reads the [Clflags.frame_pointers] flag). *)
let () = Clflags.effective_standard_library := effective_standard_library
