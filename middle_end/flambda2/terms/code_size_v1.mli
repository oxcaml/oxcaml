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

(** The original ("v1") code size model. Sizes are plain integers that do not
    depend on the target architecture. It is retained so that it can be selected
    with [-flambda2-code-size-model v1] and so that its estimates can be
    compared with those of the current model ([Code_size_v2]); see [Code_size]
    for the interface used by the rest of the compiler. *)

val alloc_size : int

val block : int -> int

val array : int -> int

val box_number :
  machine_width:Target_system.Machine_width.t ->
  Flambda_kind.Boxable_number.t ->
  int

val prim :
  machine_width:Target_system.Machine_width.t -> Flambda_primitive.t -> int

val simple : Simple.t -> int

val static_consts : unit -> int

val apply : Apply_expr.t -> int

val apply_cont : Apply_cont_expr.t -> int

val switch : Switch_expr.t -> int

val invalid : int
