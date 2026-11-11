(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
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

(** The JSIR backend of ocamlopt ([-target js_of_ocaml] and
    [-target wasm_of_ocaml]).

    The JSIR of a compilation unit is written to a JSIR file, [.cm-jsir-js] for
    JS or [.cm-jsir-wasm] for WASM. There are then two paths:

    - Separate compilation (the default): the [js_of_ocaml] executable produces
      JavaScript (or the [wasm_of_ocaml] executable produces WebAssembly) from
      the JSIR file of each unit when it is compiled, and links these when
      linking an executable. The JSIR file is intermediate, and is deleted
      unless [-S] is given.

    - Whole program: the JSIR files are the object files. Linking an executable
      links the JSIR of its units into one program, which [js_of_ocaml] or
      [wasm_of_ocaml] compiles at once. This makes compilation slower, but the
      program is optimised across units.

    The correspondence with the native backend's files is (with the
    wasm_of_ocaml extensions in parentheses):

    - [.cmx-js] ([.cmx-wasm]): unit metadata, in the same format as [.cmx];

    - [.cmo-js] ([.cmo-wasm]): the JavaScript (WebAssembly) for a unit,
      analogous to [.o];

    - [.cmxa-js] and [.cma-js] ([.cmxa-wasm] and [.cma-wasm]): libraries,
      analogous to [.cmxa] and [.a];

    - [.cm-jsir-js] and [.cma-jsir-js] ([.cm-jsir-wasm] and [.cma-jsir-wasm]):
      the JSIR of a unit, and of the units of a library. These take the place of
      [.cmo-js] and [.cma-js] with [-link-jsir]. As [-S] keeps the [.cm-jsir-js]
      of a unit compiled separately, such a unit can be linked either way;

    - [.js] files given on the command line (and [.wat], [.wast] and [.wasm]
      files with wasm_of_ocaml): runtime stubs, analogous to C stubs. They are
      recorded in library files and passed to [build-runtime] when linking an
      executable.

    [-jsoo-opt] passes options to every invocation, and [-jsoo-opt-compile],
    [-jsoo-opt-archive], [-jsoo-opt-runtime] and [-jsoo-opt-link] to one of them
    (see {!Clflags.Jsoo_phase}). *)

type error =
  | Program_not_found of
      { target : Clflags.Jsir_target.t;
        program : string
      }
  | Program_error of
      { target : Clflags.Jsir_target.t;
        subcommand : string;
        exit_code : int;
        input_left_in : string option
      }
  | Unsupported of
      { target : Clflags.Jsir_target.t;
        option : string
      }
  | Jsir_file_not_found of string
  | Unit_not_in_object_file of
      { compilation_unit : string;
        object_file : string
      }

exception Error of error

val report_error : error Format_doc.format_printer

val report_error_doc : error Format_doc.printer

(** The backend for the given JSIR target. *)
val make :
  Clflags.Jsir_target.t ->
  link_jsir:bool ->
  lambda_to_jsir:
    (ppf_dump:Format.formatter ->
    prefixname:string ->
    keep_symbol_tables:bool ->
    Lambda.program ->
    Optcomp_intf.jsir_program) ->
  jsir_linker:
    (register_globals:bool ->
    Jsoo_imports.Code.jsir_file list ->
    Jsoo_imports.Code.program) ->
  (module Optcompile.S)
