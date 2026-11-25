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

open! Jsoo_imports.Import

(** Linking of the JSIR of several compilation units into the JSIR of a whole
    program, which js_of_ocaml (or wasm_of_ocaml) then compiles at once.

    The units must be given in initialisation order, each after the units it
    imports. The linked program runs the toplevel code of each unit, by
    converting the [Stop]s of the toplevel code of a unit branch to the entry
    block of the next one. Variables and block addresses are renumbered so that
    the units do not clash.

    The symbol table of the runtime ([caml_register_symbol] and
    [caml_get_symbol], as well as [caml_get_global] for module blocks) is not
    used. Instead, the variable a unit binds to an imported symbol is replaced
    with the variable of the exporting unit that holds its value, and the
    [caml_register_symbol] calls are removed. js_of_ocaml can then see through
    references across units. For this, the exporting unit must come before the
    importing one, and linking fails otherwise.

    The [caml_register_global] calls are removed too unless [register_globals],
    so that js_of_ocaml can remove the fields of module blocks that are not
    used.

    Units after one whose toplevel code never exits normally are left out, as
    they cannot run. *)

type error =
  | No_compilation_units
  | Multiple_definition of
      { symbol : Jsir.Linkage_name.t;
        first_unit : string;
        second_unit : string
      }
  | Undefined_symbol of
      { symbol : Jsir.Linkage_name.t;
        unit_ : string
      }
  | Symbol_defined_later of
      { symbol : Jsir.Linkage_name.t;
        unit_ : string;
        exporter : string
      }

exception Error of error

val link : register_globals:bool -> Jsir.jsir_file list -> Jsir.program
