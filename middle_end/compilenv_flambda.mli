(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Flambda2-specific compilation environment operations.

    Provides typed access to export info stored opaquely in [Compilenv]. *)

type unit_infos =
  (Lambda.main_module_block_format,
   Flambda2_cmx.Flambda_cmx_format.t option)
  Cmx_format.unit_infos_gen

type unit_infos_raw =
  Flambda2_cmx.Flambda_cmx_format.raw option Cmx_format.unit_infos_raw

val get_global_export_info :
  Compilation_unit.t -> Flambda2_cmx.Flambda_cmx_format.t option

val get_unit_export_info :
  Compilation_unit.t -> Flambda2_cmx.Flambda_cmx_format.t option

val set_export_info : Flambda2_cmx.Flambda_cmx_format.t -> unit

val read_unit_info : string -> unit_infos * Digest.t

val write_unit_info : unit_infos -> string -> unit

val cache_unit_info : unit_infos -> unit

val unpack_export_info :
  Obj.t -> Flambda2_cmx.Flambda_cmx_format.t
