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

(** The JSIR backend of ocamlopt ([-target js_of_ocaml]).

    The JSIR of a compilation unit is written to a [.cmj] file, from which the
    [js_of_ocaml] executable produces JavaScript. The [.cmj] file is
    intermediate, and is deleted unless [-S] is given.

    The correspondence with the native backend's files is:

    - [.cmjx]: unit metadata, in the same format as [.cmx];

    - [.cmjo]: the JavaScript for a unit, analogous to [.o];

    - [.cmjxa] and [.cmja]: libraries, analogous to [.cmxa] and [.a];

    - [.js] files given on the command line: JavaScript stubs, analogous to C
      stubs. They are recorded in [.cmjxa] files and passed to
      [js_of_ocaml build-runtime] when linking an executable.

    [-jsoo-opt] passes options to every [js_of_ocaml] invocation, and
    [-jsoo-opt-compile], [-jsoo-opt-archive], [-jsoo-opt-runtime] and
    [-jsoo-opt-link] to one of them (see {!Clflags.Jsoo_phase}). *)

type error =
  | Js_of_ocaml_not_found of string
  | Js_of_ocaml_error of
      { subcommand : string;
        exit_code : int;
        input_left_in : string option
      }
  | Unsupported of string

exception Error of error

val report_error : error Format_doc.format_printer

val report_error_doc : error Format_doc.printer

val make :
  lambda_to_jsir:
    (ppf_dump:Format.formatter ->
    prefixname:string ->
    keep_symbol_tables:bool ->
    Lambda.program ->
    Optcomp_intf.jsir_program) ->
  (module Optcompile.S)
