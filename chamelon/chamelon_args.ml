(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
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

type subcommand = Run

let spec_list = ref []

let anon_fun = ref ignore

let subcommand = ref None

let usage_msg =
  Format.asprintf
    "usage: %s <file1> [<file2>] ... -c \"<command>\" [-m <minimizers>] [-x \
     <minimizers>] [-e <error>] [[-t <typing command>] | [--cmt <cmt file>]] \
     [-i | [-o <output>]]"
    (Filename.basename Sys.executable_name)

(* `chamelon run` *)

let input_files : string list ref = ref []

let arg_minimizers = ref ""

let exclude_minimizers = ref ""

let command = ref ""

let typing_command = ref ""

let cmt_files : string list ref = ref []

let output_file = ref ""

let test = ref (-1)

let run_anon_fun filename = input_files := filename :: !input_files

let list_minimizers = ref false

let inplace = ref false

let run_spec_list =
  [ "-c", Arg.Set_string command, "Set command";
    "-m", Arg.Set_string arg_minimizers, "Set minimizers";
    "-x", Arg.Set_string exclude_minimizers, "Exclude minimizers";
    "-e", Arg.Set_string Utils.error_str, "Set error to preserve";
    ( "-t",
      Arg.Set_string typing_command,
      "Set command to use to generate cmt file" );
    "-o", Arg.Set_string output_file, "Set output file/folder";
    "--test", Arg.Set_int test, "Run provided iteration of minimizer";
    "-l", Arg.Set list_minimizers, "List available minimizers";
    ( "--cmt",
      Arg.String (fun s -> cmt_files := s :: !cmt_files),
      "Set cmt files to use (incompatible with -t)" );
    ( "--inplace",
      Arg.Set inplace,
      "Minimize file in place (incompatible with -o); in that case, command \
       should include the input file" ) ]

(* `chamelon` *)

let chamelon_anon_fun s =
  match s with
  | "run" ->
    subcommand := Some Run;
    anon_fun := run_anon_fun
  | _ ->
    Format.eprintf "@[%a@ %s;@ %a@]@." Format.pp_print_text
      "warning: unknown subcommand" (Filename.quote s) Format.pp_print_text
      "parsing as file name in legacy mode.";
    !anon_fun "run";
    !anon_fun s

let () =
  spec_list := run_spec_list;
  anon_fun := chamelon_anon_fun;
  subcommand := Some Run

let anon_fun s = !anon_fun s
