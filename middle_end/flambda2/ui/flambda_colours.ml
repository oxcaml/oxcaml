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

type directive = Misc.Colours.directive

let pop = Misc.Colours.pop

let none = Misc.Colours.none

let without_colours = Misc.Colours.without_colours

let prim_constructive ppf = Misc.Colours.push ~fg:163 ppf

let prim_destructive ppf = Misc.Colours.push ~fg:62 ppf

let prim_neither ppf = Misc.Colours.push ~fg:130 ppf

let naked_number ppf = Misc.Colours.push ~fg:70 ppf

let unboxed_product ppf = Misc.Colours.push ~fg:198 ppf

let tagged_immediate ppf = Misc.Colours.push ~fg:70 ppf

let constructor ppf = Misc.Colours.push ~fg:69 ppf

let kind ppf = Misc.Colours.push ~fg:37 ppf

let subkind ppf = Misc.Colours.push ~fg:39 ppf

let top_or_bottom_type ppf = Misc.Colours.push ~fg:37 ppf

let debuginfo ppf = Misc.Colours.push ~fg:243 ppf

let discriminant ppf = Misc.Colours.push ~fg:111 ppf

let name ppf = Misc.Colours.push ~fg:111 ppf

let parameter ppf = Misc.Colours.push ~fg:198 ppf

let symbol ppf = Misc.Colours.push ~fg:98 ppf

let variable ppf = Misc.Colours.push ~fg:111 ppf

let function_slot ppf = Misc.Colours.push ~fg:31 ppf

let value_slot ppf = Misc.Colours.push ~fg:43 ppf

let code_id ppf = Misc.Colours.push ~fg:169 ppf

let expr_keyword ppf = Misc.Colours.push ~fg:51 ppf

let invalid_keyword ppf = Misc.Colours.push ~fg:255 ~bg:160 ppf

let static_keyword ppf = Misc.Colours.push ~fg:255 ~bg:240 ppf

let static_part ppf = Misc.Colours.push ~fg:255 ~bg:237 ppf

let continuation ppf = Misc.Colours.push ~fg:35 ppf

let continuation_definition ppf = Misc.Colours.push ~bg:237 ppf

let continuation_annotation ppf = Misc.Colours.push ~fg:202 ~bg:237 ppf

let name_abstraction ppf = Misc.Colours.push ~fg:172 ppf

let rec_info ppf = Misc.Colours.push ~fg:249 ppf

let coercion ppf = Misc.Colours.push ~fg:249 ppf

let depth_variable ppf = Misc.Colours.push ~fg:214 ppf

let error ppf = Misc.Colours.push ~fg:160 ppf

let elide ppf = Misc.Colours.push ~fg:243 ppf

let each_file ppf = Misc.Colours.push ~fg:51 ppf

let lambda = expr_keyword

let effect_ ppf = Misc.Colours.push ~fg:46 ppf
