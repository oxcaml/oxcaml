(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The original code size model: estimates for individual Flambda terms, as
   plain integers that do not depend on the target architecture. The
   representation of sizes used by the rest of the compiler, and the arithmetic
   on them, are in [Code_size], which wraps this module. *)

val box_number :
  machine_width:Target_system.Machine_width.t ->
  Flambda_kind.Boxable_number.t ->
  int

val block : int -> int

val array : int -> int

val prim :
  machine_width:Target_system.Machine_width.t -> Flambda_primitive.t -> int

val simple : Simple.t -> int

val static_consts : unit -> int

val apply : Apply_expr.t -> int

val apply_cont : Apply_cont_expr.t -> int

val switch : Switch_expr.t -> int

val invalid : int

val alloc_size : int
