(******************************************************************************
 *                                  OxCaml                                    *
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

[@@@ocaml.warning "+a-40-41-42"]

type directive = Oxcaml_colours.directive

let pop = Oxcaml_colours.pop

let none = Oxcaml_colours.none

let without_colours = Oxcaml_colours.without_colours

let terminator ppf = Oxcaml_colours.push ~fg:111 ppf

let block_label ppf = Oxcaml_colours.push ~fg:198 ppf

let block_label_exn ppf = Oxcaml_colours.push ~fg:198 ~bg:197 ppf

let instr_id ppf = Oxcaml_colours.push ~fg:43 ppf

let pred_succ ppf = Oxcaml_colours.push ~fg:243 ppf

let liveness ppf = Oxcaml_colours.push ~fg:243 ppf

let function_name ppf = Oxcaml_colours.push ~fg:1 ~bg:169 ppf

let basic ppf = Oxcaml_colours.push ~fg:65 ppf

let result ppf = Oxcaml_colours.push ~fg:220 ppf

let argument ppf = Oxcaml_colours.push ~fg:81 ppf
