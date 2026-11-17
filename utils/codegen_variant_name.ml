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

(* This module must stay dependency-free (it may not refer to [Clflags],
   [Config], [Misc] or any other compiler module): it is shared by file copy
   with build tools that cannot link compilerlibs (stdlib/dune_rules/gen.ml,
   ocamltest; see also otherlibs/dynlink/dune).

   Token reservation rule: a token name never contains '-' or '_', and never
   equals a runtime-variant suffix string ("d", "i", "pic", "nnp"), so that
   the runtime-archive name composition in asmcomp/asmlink.ml (runtime_lib)
   and stdlib/dune_rules/gen.ml stays injective.  The P1 consistency test
   (testsuite/tests/frame-pointers/codegen_variant_names.ml) checks the rule
   for every installed variant. *)

type token =
  | Frame_pointers
  | Stack_checks

(* In canonical (sorted-name) order. *)
let all_tokens = [Frame_pointers; Stack_checks]

let token_name = function Frame_pointers -> "fp" | Stack_checks -> "sc"

let token_of_name = function
  | "fp" -> Some Frame_pointers
  | "sc" -> Some Stack_checks
  | _ -> None

let equal_token t1 t2 =
  match t1, t2 with
  | Frame_pointers, Frame_pointers -> true
  | Stack_checks, Stack_checks -> true
  | (Frame_pointers | Stack_checks), _ -> false

let compare_token t1 t2 = String.compare (token_name t1) (token_name t2)

(* Per-token data tables: the single source of the name/flag scheme.  The
   Makefile-side tables in Makefile.codegen-variants are a second encoder of
   these and must be kept in sync.  What catches drift: the consistency
   test keeps the NAMES honest; ocamlopt-axis drift shows up as fingerprint
   mismatches when the variant trees are built; and the cc/aspp/OCAMLPARAM
   columns are asserted against these tables when stdlib/dune_rules/gen.ml
   parses the build-settings records (no fingerprint covers runtime C or
   assembler flags). *)

let ocamlopt_flag token ~enabled =
  match token, enabled with
  | Frame_pointers, true -> "-frame-pointers"
  | Frame_pointers, false -> "-no-frame-pointers"
  | Stack_checks, true -> "-stack-checks"
  | Stack_checks, false -> "-no-stack-checks"

let ocamlparam token ~enabled =
  let value = if enabled then "1" else "0" in
  match token with
  | Frame_pointers -> "frame-pointers=" ^ value
  | Stack_checks -> "stack-checks=" ^ value

(* The macro flags always use the explicit value form (-D<macro>=1 or
   -D<macro>=0, never -U and never mere absence): m.h resolves each absent
   axis macro to the configure default's value (the <macro>_DEFAULT
   templates), so only an explicit =1|0 can pin a compilation, and a -U
   would re-expose the m.h default rather than turn the axis off. *)
let cc_flags token ~enabled =
  match token, enabled with
  | Frame_pointers, true ->
    ["-DWITH_FRAME_POINTERS=1"; "-fno-omit-frame-pointer"; "-g"]
  | Frame_pointers, false -> ["-DWITH_FRAME_POINTERS=0"]
  | Stack_checks, true -> ["-DSTACK_CHECKS_ENABLED=1"]
  | Stack_checks, false -> ["-DSTACK_CHECKS_ENABLED=0"]

let aspp_flags token ~enabled =
  match token, enabled with
  | Frame_pointers, true -> ["-DWITH_FRAME_POINTERS=1"]
  | Frame_pointers, false -> ["-DWITH_FRAME_POINTERS=0"]
  | Stack_checks, true -> ["-DSTACK_CHECKS_ENABLED=1"]
  | Stack_checks, false -> ["-DSTACK_CHECKS_ENABLED=0"]

(* Whether the token changes the runtime (so its archives differ); tokens
   that only affect OCaml code generation share the projection's archives. *)
let is_runtime_axis = function Frame_pointers -> true | Stack_checks -> true

type t = token list

let base = []

let of_tokens tokens = List.sort_uniq compare_token tokens

let tokens t = t

let mem token t = List.exists (equal_token token) t

let equal t1 t2 = List.equal equal_token t1 t2

let base_name = "base"

let to_string = function
  | [] -> base_name
  | tokens -> String.concat "-" (List.map token_name tokens)

(* Only the canonical form is accepted: known tokens, strictly sorted,
   '-'-joined; the all-off variant is spelled "base", never "". *)
let of_string s =
  if String.equal s base_name
  then Some []
  else
    let rec parse acc = function
      | [] -> Some (List.rev acc)
      | part :: rest -> (
        match token_of_name part with
        | None -> None
        | Some token -> (
          match acc with
          | prev :: _ when compare_token prev token >= 0 -> None
          | _ -> parse (token :: acc) rest))
    in
    parse [] (String.split_on_char '-' s)

let runtime_projection t = List.filter is_runtime_axis t

let runtime_archive_suffix t ~default =
  let projection = runtime_projection t in
  if equal projection (runtime_projection default)
  then ""
  else
    "_"
    ^ String.map
        (fun c -> if c = '-' then '_' else c)
        (to_string projection)

let ocamlopt_flags t =
  List.map (fun token -> ocamlopt_flag token ~enabled:(mem token t)) all_tokens

let ocamlparams t =
  List.map (fun token -> ocamlparam token ~enabled:(mem token t)) all_tokens

let variant_cc_flags t =
  List.concat_map (fun token -> cc_flags token ~enabled:(mem token t))
    all_tokens

let variant_aspp_flags t =
  List.concat_map (fun token -> aspp_flags token ~enabled:(mem token t))
    all_tokens
