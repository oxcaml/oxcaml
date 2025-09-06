(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  James Rayman, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external of_int8_u : int8# -> char# = "%identity"
external to_int8_u : char# -> int8# = "%identity"

let to_char c = Char.chr (Int8_u.to_int (to_int8_u c))

let of_char c = of_int8_u (Int8_u.of_int (Char.code c))
