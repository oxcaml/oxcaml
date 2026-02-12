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

(* Test harness for Binary_emitter_verify.For_testing *)

let unix = (module Unix : Compiler_owee.Unix_intf.S)

let failures = ref 0

let test ~name ~expected_pathname ~actual_pathname ~expect =
  let result =
    Binary_emitter_verify.For_testing.compare_object_files unix
      ~expected_pathname ~actual_pathname
  in
  let pass = expect result in
  if pass
  then Printf.printf "PASS: %s\n" name
  else (
    incr failures;
    Printf.printf "FAIL: %s\n" name;
    Printf.printf "  Result: ";
    Binary_emitter_verify.print_result Format.std_formatter result)

let is_match = function Binary_emitter_verify.Match _ -> true | _ -> false

let is_section_content_mismatch = function
  | Binary_emitter_verify.Mismatch (Section_content _) -> true
  | _ -> false

let is_section_size_mismatch = function
  | Binary_emitter_verify.Mismatch (Section_size _) -> true
  | _ -> false

let is_text_relocation_mismatch = function
  | Binary_emitter_verify.Mismatch (Relocation { section_name; _ }) ->
    String.equal section_name ".text"
  | _ -> false

let is_data_relocation_mismatch = function
  | Binary_emitter_verify.Mismatch (Relocation { section_name; _ }) ->
    String.equal section_name ".data"
  | _ -> false

let is_data_content_mismatch = function
  | Binary_emitter_verify.Mismatch (Section_content { section_name; _ }) ->
    String.equal section_name "data"
  | _ -> false

let is_data_size_mismatch = function
  | Binary_emitter_verify.Mismatch (Section_size { section_name; _ }) ->
    String.equal section_name "data"
  | _ -> false

(* Platform-specific behavior for "missing" sections: Assemblers may either omit
   sections entirely (returning Missing_section) or create 0-byte sections
   (returning Section_size with one side being 0). We accept either behavior as
   valid for "missing section" tests. *)
let is_missing_text_section result =
  match result with
  | Binary_emitter_verify.Mismatch (Missing_section s) ->
    String.starts_with ~prefix:".text" s
  | Binary_emitter_verify.Mismatch
      (Section_size { section_name; expected; actual }) ->
    section_name = ".text" && (expected = 0 || actual = 0)
  | _ -> false

let is_missing_data_section result =
  match result with
  | Binary_emitter_verify.Mismatch (Missing_section s) ->
    String.starts_with ~prefix:"data" s
  | Binary_emitter_verify.Mismatch
      (Section_size { section_name; expected; actual }) ->
    section_name = "data" && (expected = 0 || actual = 0)
  | _ -> false

let () =
  Printf.printf "Binary emitter verification tests\n";
  Printf.printf "==================================\n\n";
  (* Test: identical files should match *)
  test ~name:"identical files match" ~expected_pathname:"reference.o"
    ~actual_pathname:"match.o" ~expect:is_match;
  (* Test: different instruction content *)
  test ~name:"text content mismatch detected" ~expected_pathname:"reference.o"
    ~actual_pathname:"mismatch_text_content.o"
    ~expect:is_section_content_mismatch;
  (* Test: different text section size *)
  test ~name:"text size mismatch detected" ~expected_pathname:"reference.o"
    ~actual_pathname:"mismatch_text_size.o" ~expect:is_section_size_mismatch;
  (* Test: different text relocation *)
  test ~name:"text relocation mismatch detected"
    ~expected_pathname:"reference.o" ~actual_pathname:"mismatch_text_reloc.o"
    ~expect:is_text_relocation_mismatch;
  (* Test: different data relocation *)
  test ~name:"data relocation mismatch detected"
    ~expected_pathname:"reference.o" ~actual_pathname:"mismatch_data_reloc.o"
    ~expect:is_data_relocation_mismatch;
  (* Test: different data content *)
  test ~name:"data content mismatch detected" ~expected_pathname:"reference.o"
    ~actual_pathname:"mismatch_data_content.o" ~expect:is_data_content_mismatch;
  (* Test: different data section size *)
  test ~name:"data size mismatch detected" ~expected_pathname:"reference.o"
    ~actual_pathname:"mismatch_data_size.o" ~expect:is_data_size_mismatch;
  (* Test: missing data section in actual *)
  test ~name:"missing data section detected (in actual)"
    ~expected_pathname:"reference.o" ~actual_pathname:"text_only.o"
    ~expect:is_missing_data_section;
  (* Test: missing data section in expected *)
  test ~name:"missing data section detected (in expected)"
    ~expected_pathname:"text_only.o" ~actual_pathname:"reference.o"
    ~expect:is_missing_data_section;
  (* Test: missing text section in actual *)
  test ~name:"missing text section detected (in actual)"
    ~expected_pathname:"reference.o" ~actual_pathname:"data_only.o"
    ~expect:is_missing_text_section;
  (* Test: missing text section in expected *)
  test ~name:"missing text section detected (in expected)"
    ~expected_pathname:"data_only.o" ~actual_pathname:"reference.o"
    ~expect:is_missing_text_section;
  (* Summary *)
  Printf.printf "\n";
  if !failures = 0
  then Printf.printf "All tests passed!\n"
  else (
    Printf.printf "%d test(s) failed.\n" !failures;
    exit 1)
