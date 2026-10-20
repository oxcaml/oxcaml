(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Kate Deplaix                              *)
(*                                                                        *)
(*   Copyright 2025 Kate Deplaix                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external phys_equal :
  ('a : value_or_null) . ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%eq"

external equal :
  ('a : value_or_null) . ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"
external compare :
  ('a : value_or_null) . ('a[@local_opt]) -> ('a[@local_opt]) -> int
  = "%compare"

let min = Stdlib.min
let max = Stdlib.max
