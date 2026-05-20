(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type directive = Oxcaml_colours.directive

let pop = Oxcaml_colours.pop

let none = Oxcaml_colours.none

let without_colours = Oxcaml_colours.without_colours

let prim_constructive ppf = Oxcaml_colours.push ~fg:163 ppf

let prim_destructive ppf = Oxcaml_colours.push ~fg:62 ppf

let prim_neither ppf = Oxcaml_colours.push ~fg:130 ppf

let naked_number ppf = Oxcaml_colours.push ~fg:70 ppf

let unboxed_product ppf = Oxcaml_colours.push ~fg:198 ppf

let tagged_immediate ppf = Oxcaml_colours.push ~fg:70 ppf

let constructor ppf = Oxcaml_colours.push ~fg:69 ppf

let kind ppf = Oxcaml_colours.push ~fg:37 ppf

let subkind ppf = Oxcaml_colours.push ~fg:39 ppf

let top_or_bottom_type ppf = Oxcaml_colours.push ~fg:37 ppf

let debuginfo ppf = Oxcaml_colours.push ~fg:243 ppf

let discriminant ppf = Oxcaml_colours.push ~fg:111 ppf

let name ppf = Oxcaml_colours.push ~fg:111 ppf

let parameter ppf = Oxcaml_colours.push ~fg:198 ppf

let symbol ppf = Oxcaml_colours.push ~fg:98 ppf

let variable ppf = Oxcaml_colours.push ~fg:111 ppf

let function_slot ppf = Oxcaml_colours.push ~fg:31 ppf

let value_slot ppf = Oxcaml_colours.push ~fg:43 ppf

let code_id ppf = Oxcaml_colours.push ~fg:169 ppf

let expr_keyword ppf = Oxcaml_colours.push ~fg:51 ppf

let invalid_keyword ppf = Oxcaml_colours.push ~fg:255 ~bg:160 ppf

let static_keyword ppf = Oxcaml_colours.push ~fg:255 ~bg:240 ppf

let static_part ppf = Oxcaml_colours.push ~fg:255 ~bg:237 ppf

let continuation ppf = Oxcaml_colours.push ~fg:35 ppf

let continuation_definition ppf = Oxcaml_colours.push ~bg:237 ppf

let continuation_annotation ppf = Oxcaml_colours.push ~fg:202 ~bg:237 ppf

let name_abstraction ppf = Oxcaml_colours.push ~fg:172 ppf

let rec_info ppf = Oxcaml_colours.push ~fg:249 ppf

let coercion ppf = Oxcaml_colours.push ~fg:249 ppf

let depth_variable ppf = Oxcaml_colours.push ~fg:214 ppf

let error ppf = Oxcaml_colours.push ~fg:160 ppf

let elide ppf = Oxcaml_colours.push ~fg:243 ppf

let each_file ppf = Oxcaml_colours.push ~fg:51 ppf

let lambda = expr_keyword

let effect_ ppf = Oxcaml_colours.push ~fg:46 ppf
