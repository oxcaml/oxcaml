(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
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

(** The js_of_ocaml backend of ocamlopt, selected with [-target js_of_ocaml].

    It translates Flambda 2 to the js_of_ocaml IR and runs the [js_of_ocaml]
    executable (found in [Config.bindir], or in the PATH, unless the
    [OXCAML_JS_OF_OCAML] environment variable names it) to produce JavaScript:

    - compiling [foo.ml] produces [foo.cmjx] (the counterpart of [.cmx]) and
      [foo.cmjo] (the counterpart of [.o], containing JavaScript), via the
      intermediate [foo.cmj] (the counterpart of [.s], kept with [-S]);

    - [-a] produces a [.cmjxa] and a [.cmja] (the counterparts of [.cmxa] and
      [.a]);

    - linking produces a JavaScript file, [a.out.js] by default.

    [.js] files given on the command line are JavaScript stubs, in the role of C
    objects.

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
