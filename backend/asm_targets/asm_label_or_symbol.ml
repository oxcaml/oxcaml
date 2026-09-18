(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Label of Asm_label.t
  | Symbol of Asm_symbol.t

let compare t1 t2 =
  match t1, t2 with
  | Label lbl1, Label lbl2 -> Asm_label.compare lbl1 lbl2
  | Symbol sym1, Symbol sym2 -> Asm_symbol.compare sym1 sym2
  | Label _, Symbol _ -> -1
  | Symbol _, Label _ -> 1

let equal t1 t2 = compare t1 t2 = 0

let hash = function
  | Label lbl -> Hashtbl.hash (0, Asm_label.hash lbl)
  | Symbol sym -> Hashtbl.hash (1, Asm_symbol.hash sym)

let print ppf = function
  | Label lbl -> Asm_label.print ppf lbl
  | Symbol sym -> Asm_symbol.print ppf sym
