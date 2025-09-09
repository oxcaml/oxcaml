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

type t = char#

external of_int8_u : int8# -> char# @@ portable = "%identity"
external to_int8_u : char# -> int8# @@ portable = "%identity"

let code c = Int8_u.unsigned_to_int (to_int8_u c)

let to_char c = Char.chr (code c)

let of_char c = of_int8_u (Int8_u.of_int (Char.code c))

let chr i = of_char (Char.chr i)

let escaped c = Char.escaped (to_char c)

let lowercase_ascii c = of_char (Char.lowercase_ascii (to_char c))

let uppercase_ascii c = of_char (Char.uppercase_ascii (to_char c))

let equal c d = Int8_u.equal (to_int8_u c) (to_int8_u d)

let compare c d = Int8_u.unsigned_compare (to_int8_u c) (to_int8_u d)

let seeded_hash seed c = Int8_u.seeded_hash seed (to_int8_u c)

let hash c = Int8_u.hash (to_int8_u c)
