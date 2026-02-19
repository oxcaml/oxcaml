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

type dune_subcommand = Build

type subcommand =
  | Run
  | Dune

let spec_list = ref []

let anon_fun = ref ignore

let subcommand = ref None

let dune_subcommand = ref None

let usage_msg =
  Format.asprintf
    "usage: %s <file1> [<file2>] ... -c \"<command>\" [-m <minimizers>] [-x \
     <minimizers>] [-e <error>] [[-t <typing command>] | [--cmt <cmt file>]] \
     [-i | [-o <output>]]"
    (Filename.basename Sys.executable_name)

(* common args *)

let common_spec_list =
  ["-e", Arg.Set_string Utils.error_str, "Set error to preserve"]

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
  @ common_spec_list

(* `chamelon dune` *)

let root = ref ""

let workspace = ref ""

let only_packages = ref None

let targets = ref []

let set_only_packages pkgs = only_packages := Some pkgs

let assume_failing = ref false

let extra_args_for_failing_target = ref []

let add_extra_arg_for_failing_target s =
  extra_args_for_failing_target := s :: !extra_args_for_failing_target

let dune_spec_list =
  [ "--root", Arg.Set_string root, "Passed through to dune.";
    "--workspace", Arg.Set_string workspace, "Set workspace file";
    "--only-packages", Arg.String set_only_packages, "Passed through to dune.";
    ( "--assume-failing",
      Arg.Set assume_failing,
      "Assume that provided targets are failing (skip failing target \
       inference)." );
    ( "--extra-arg-for-failing-target",
      Arg.String add_extra_arg_for_failing_target,
      "Extra argument to add to the compilation command of the failing target."
    ) ]
  @ common_spec_list

let dune_build_anon_fun target = targets := target :: !targets

let dune_anon_fun = function
  | "build" ->
    dune_subcommand := Some Build;
    anon_fun := dune_build_anon_fun
  | s -> raise (Arg.Bad (Format.asprintf "unknown dune subcommand: %s" s))

(* `chamelon` *)

let chamelon_anon_fun s =
  match s with
  | "run" ->
    subcommand := Some Run;
    anon_fun := run_anon_fun
  | "dune" ->
    subcommand := Some Dune;
    spec_list := dune_spec_list;
    anon_fun := dune_anon_fun
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
