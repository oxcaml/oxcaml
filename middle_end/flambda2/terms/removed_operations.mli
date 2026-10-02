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

type t = private
  { call : int;
    alloc : int;
    prim : int;
    branch : int;
    direct_call_of_indirect : int;
    specialized_poly_compare : int;
    requested_inline : int
  }

val zero : t

val call : t

val branch : t

val prim : Flambda_primitive.t -> t

val alloc : t

val direct_call_of_indirect : t

val specialized_poly_compare : t

val ( + ) : t -> t -> t

val print : Format.formatter -> t -> unit

val evaluate : args:Inlining_arguments.t -> t -> float

(** The bonus, in instructions, credited for the removed operations when a
    speculative inlining is judged by the ratio criterion (see the
    [speculative_inlining_bonus_*] flags in [Flambda_features.Inlining]). *)
val bonus : t -> float

val equal : t -> t -> bool
