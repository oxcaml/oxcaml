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

    The JSIR of a compilation unit is written to a JSIR file, [.cmj] for JS or
    [.cmw] for WASM, from which the [js_of_ocaml] executable produces JavaScript
    or the [wasm_of_ocaml] executable produces WebAssembly, respectively. The
    file is intermediate, and is deleted unless [-S] is given.

    The correspondence with the native backend's files is (with the
    wasm_of_ocaml extensions in parentheses):

    - [.cmjx] ([.cmwx]): unit metadata, in the same format as [.cmx];

    - [.cmjo] ([.cmwo]): the JavaScript (WebAssembly) for a unit, analogous to
      [.o];

    - [.cmjxa] and [.cmja] ([.cmwxa] and [.cmwa]): libraries, analogous to
      [.cmxa] and [.a];

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

exception Error of error

val report_error : error Format_doc.format_printer

val report_error_doc : error Format_doc.printer

(** The backend for the given JSIR target. *)
val make :
  Clflags.Jsir_target.t ->
  lambda_to_jsir:
    (ppf_dump:Format.formatter ->
    prefixname:string ->
    keep_symbol_tables:bool ->
    Lambda.program ->
    Optcomp_intf.jsir_program) ->
  (module Optcompile.S)
