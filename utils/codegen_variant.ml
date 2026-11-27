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

type t =
  { address_sanitizer : bool;
    frame_pointers : bool;
    poll_insertion : bool;
    stack_checks : bool
  }

let flags_variant () =
  { address_sanitizer = !Clflags.address_sanitizer;
    frame_pointers = !Clflags.frame_pointers;
    poll_insertion = !Clflags.poll_insertion;
    stack_checks = !Clflags.stack_checks
  }

(* The base variant: what every installation compiles for when no flag
   and no OCAMLPARAM setting selects another, and what its library root
   holds.  It is not configurable; see the default values of the axis
   flags in [Clflags], which this must agree with. *)
let default =
  { address_sanitizer = false;
    frame_pointers = false;
    poll_insertion = false;
    stack_checks = false
  }

(* The axes describe native code generation only.  A JSIR compilation
   (-target js_of_ocaml or wasm_of_ocaml) emits no native code, and the
   JSIR libraries are installed once, in the default library root, so such
   a compilation always counts as the default variant whatever the axis
   flags say: its artifacts carry the default fingerprint and its library
   root is never remapped. *)
let current () =
  if Clflags.target_is_jsir () then default else flags_variant ()

let equal t1 t2 =
  Bool.equal t1.address_sanitizer t2.address_sanitizer
  && Bool.equal t1.frame_pointers t2.frame_pointers
  && Bool.equal t1.poll_insertion t2.poll_insertion
  && Bool.equal t1.stack_checks t2.stack_checks

let is_default t = equal t default

let apply t =
  Clflags.address_sanitizer := t.address_sanitizer;
  Clflags.frame_pointers := t.frame_pointers;
  Clflags.poll_insertion := t.poll_insertion;
  Clflags.stack_checks := t.stack_checks

(* Naming goes through the dependency-free [Codegen_variant_name] token
   scheme; a new axis extends [name]/[of_name] alongside [t]. *)
let name t =
  Codegen_variant_name.of_tokens
    ((if t.address_sanitizer
     then [Codegen_variant_name.Address_sanitizer]
     else [])
    @ (if t.frame_pointers then [Codegen_variant_name.Frame_pointers] else [])
    @ (if t.poll_insertion then [Codegen_variant_name.Poll_insertion] else [])
    @ if t.stack_checks then [Codegen_variant_name.Stack_checks] else [])

let of_name n =
  { address_sanitizer = Codegen_variant_name.(mem Address_sanitizer n);
    frame_pointers = Codegen_variant_name.(mem Frame_pointers n);
    poll_insertion = Codegen_variant_name.(mem Poll_insertion n);
    stack_checks = Codegen_variant_name.(mem Stack_checks n)
  }

let to_string t = Codegen_variant_name.to_string (name t)

let of_string s = Option.map of_name (Codegen_variant_name.of_string s)

let runtime_archive_suffix t =
  Codegen_variant_name.runtime_archive_suffix (name t) ~default:(name default)

let runtime_guard_symbol t = Codegen_variant_name.runtime_guard_symbol (name t)

(* The target checks mirror the cases of configure.ac that decide which
   variants can be built: frame pointers need the amd64 backend, the C
   runtime's stack checks are mandatory everywhere but on amd64 and arm64,
   and AddressSanitizer is Clang-only (the runtime's report wrappers rely
   on Clang-only attributes, see runtime/memory.c). *)
let check_supported t =
  let frame_pointers_supported =
    match Config.architecture, Config.system with
    | "amd64", ("linux" | "macosx") -> true
    | _, _ -> false
  in
  let stack_checks_optional =
    match Config.architecture with "amd64" | "arm64" -> true | _ -> false
  in
  if t.frame_pointers && not frame_pointers_supported
  then
    Error
      "Frame pointers are not supported on this platform; remove \
       -frame-pointers (or frame-pointers=1 in OCAMLPARAM)"
  else if t.stack_checks && t.address_sanitizer
  then
    Error
      "Stack checks together with AddressSanitizer is an untested \
       combination; disable one of them (-no-stack-checks or -no-asan, or \
       stack-checks=0 / asan=0 in OCAMLPARAM)"
  else if not (t.stack_checks || stack_checks_optional)
  then
    Error
      "Stack checks cannot be disabled on this architecture; remove \
       -no-stack-checks (or stack-checks=0 in OCAMLPARAM)"
  else if not t.address_sanitizer
  then Ok ()
  else if not t.frame_pointers
  then
    Error
      "AddressSanitizer requires frame pointers; add -frame-pointers (or \
       frame-pointers=1 in OCAMLPARAM) or disable AddressSanitizer with \
       -no-asan (or asan=0 in OCAMLPARAM)"
  else if Config.tsan
  then
    Error
      "AddressSanitizer cannot be combined with ThreadSanitizer (the C \
       compiler rejects -fsanitize=address together with \
       -fsanitize=thread, and this compiler was configured with \
       --enable-tsan); remove -asan (or asan=1 in OCAMLPARAM)"
  else
    match Config.architecture, Config.system, Config.c_compiler_is_clang with
    | "amd64", "linux", true -> Ok ()
    | "amd64", "linux", false ->
      Error
        "AddressSanitizer requires a compiler configured with Clang as the \
         C compiler; remove -asan (or asan=1 in OCAMLPARAM)"
    | _, _, _ ->
      Error
        "AddressSanitizer is not supported on this platform; remove -asan \
         (or asan=1 in OCAMLPARAM)"

(* 4096 is the smallest page size of any supported target; the runtime
   asserts [page_size >= Stack_guard_stride] when allocating a guard-paged
   stack (see Stack_guard_stride in runtime/caml/config.h, which must agree
   with this value, and the assertion in runtime/fiber.c). *)
let stack_guard_stride () = if !Clflags.stack_checks then 0 else 4096

type fingerprint = (string * string) list

(* Poll insertion is deliberately NOT a fingerprint key: mixing polled and
   non-polled units is sound (the program is merely not fully polled), so
   it is recorded as the separate per-unit [ui_polled] bit that the linker
   ANDs into [caml_code_fully_polled] rather than enforced by equality. *)
let fingerprint t =
  [ "frame_pointers", string_of_bool t.frame_pointers;
    "stack_checks", string_of_bool t.stack_checks;
    "address_sanitizer", string_of_bool t.address_sanitizer ]

let current_fingerprint () = fingerprint (current ())

let equal_fingerprint f1 f2 =
  let sort f = List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) f in
  List.equal
    (fun (n1, v1) (n2, v2) -> String.equal n1 n2 && String.equal v1 v2)
    (sort f1) (sort f2)

let string_of_fingerprint f =
  String.concat ", " (List.map (fun (n, v) -> n ^ "=" ^ v) f)

let variants_directory = "variants"

let variant_root t =
  Filename.concat
    (Filename.concat Config.standard_library variants_directory)
    (to_string t)

let directory_exists dir = try Sys.is_directory dir with Sys_error _ -> false

(* The library tree built for exactly [t], when the installation has it. *)
let tree t =
  if is_default t
  then Some (t, Config.standard_library)
  else
    let root = variant_root t in
    if directory_exists root then Some (t, root) else None

(* Poll insertion is a property of each unit and not of the program (see
   [fingerprint]), and it changes nothing that the users of a unit see: the
   trees of two variants that differ only in it have the same interfaces,
   implementation CRCs and symbols.  So either tree serves both variants,
   at compile time and at link time alike, and the poll-insertion setting
   of an invocation only says which of the two it prefers. *)
let selected_tree () =
  let t = current () in
  match tree t with
  | Some _ as selected -> selected
  | None -> tree { t with poll_insertion = not t.poll_insertion }

let effective_standard_library () =
  match selected_tree () with
  | Some (_, root) -> root
  | None ->
    (* The base root may itself serve the requested variant (the in-tree
       build compiles with -frame-pointers against an fp stdlib via
       OCAMLLIB, and users may point OCAMLLIB at a custom-built variant
       tree); genuinely wrong roots are caught by the artifact-fingerprint
       check at the first .cmx import and by the runtime-archive lookup at
       link, which append [missing_root_hint].  A root that differs from
       the requested variant in poll insertion alone is not a wrong one:
       its libraries are used, with their own polled bits. *)
    Config.standard_library

let missing_root_hint () =
  match selected_tree () with
  | Some _ -> None
  | None ->
    Some
      (Printf.sprintf "note: %s does not contain codegen variant '%s'"
         Config.standard_library
         (to_string (current ())))

type directory_error =
  { directory : string;
    variant : string;
    variant_root : string
  }

exception Directory_not_provided_by_variant of directory_error

let is_plus_directory s = String.length s > 0 && s.[0] = '+'

(* Variant trees only exist on systems whose directory separator is '/'.
   Empty components (repeated and trailing separators) and "." components
   do not count. *)
let path_components path =
  List.filter
    (fun component ->
      not
        (String.equal component ""
        || String.equal component Filename.current_dir_name))
    (String.split_on_char '/' path)

let rec strip_prefix ~prefix components =
  match prefix, components with
  | [], rest -> Some rest
  | p :: prefix, c :: components when String.equal p c ->
    strip_prefix ~prefix components
  | _ :: _, _ -> None

(* Paths are compared as files: by the components of the absolute path, a
   relative one being taken from the current directory, so that a file or
   a directory is the same however its path is spelled.  Symbolic links are
   not resolved, and ".." is a component like another: a path that goes
   through one is not recognized as inside the directory that it comes
   back to. *)
let absolute_components path =
  path_components
    (if Filename.is_relative path
     then Filename.concat (Sys.getcwd ()) path
     else path)

(* The same file under any spelling of its path.  ".." stays where it is:
   it follows a directory that can be a symbolic link, so it is not known
   which directory it leads to. *)
let file_identity path =
  String.concat Filename.dir_sep (absolute_components path)

let same_file path other =
  String.equal (file_identity path) (file_identity other)

(* A file that this invocation wrote is the file to read back, wherever it
   is: what the tree of the selected variant has under the same name is
   another file. *)
let written_files : (string, unit) Hashtbl.t = Hashtbl.create 16

let record_product path = Hashtbl.replace written_files (file_identity path) ()

let is_product path =
  Hashtbl.length written_files > 0
  && Hashtbl.mem written_files (file_identity path)

let selected_root_mapping () =
  let default_root = Config.standard_library in
  let root = effective_standard_library () in
  if String.equal root default_root
  then Fun.id
  else
    let prefix = absolute_components default_root in
    fun path ->
      match strip_prefix ~prefix (absolute_components path) with
      | None -> path
      | Some rest ->
        (* A path that already names a variant's tree was chosen as such,
           and a ".." component could leave the root again. *)
        let names_variant_tree =
          match rest with
          | first :: _ -> String.equal first variants_directory
          | [] -> false
        in
        if names_variant_tree
           || List.mem Filename.parent_dir_name rest
           || is_product path
        then path
        else
          let counterpart = List.fold_left Filename.concat root rest in
          (* A directory is replaced by a directory only; "" is the current
             one. *)
          let is_directory path =
            directory_exists
              (if String.equal path "" then Filename.current_dir_name else path)
          in
          if Sys.file_exists counterpart
             && not (is_directory path && not (is_directory counterpart))
          then counterpart
          else path

let in_selected_root path = selected_root_mapping () path

let is_product_in_replaced_directory path =
  is_product path
  &&
  let directory = Filename.dirname path in
  not (String.equal (in_selected_root directory) directory)

let expand_directory_unchecked s =
  if is_plus_directory s
  then Misc.expand_directory (effective_standard_library ()) s
  else in_selected_root s

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
  else in_selected_root s

(* [Clflags] computes the -I flag for the standard library but cannot depend
   on this module (this module reads the [Clflags.frame_pointers] flag). *)
let () = Clflags.effective_standard_library := effective_standard_library
