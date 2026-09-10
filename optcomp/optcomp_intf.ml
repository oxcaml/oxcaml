(******************************************************************************
 *                                  OxCaml                                    *
 *                        Jacob Van Buren, Jane Street                        *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open Format
open Cmx_format
open Compilenv

type emit = Compile_common.info -> unit

(** Rebuild one reaped compilation unit of a batch from its reaped Flambda data.
    [paused_unit_infos] is the contents of the unit's paused .cmx file.
    [Compilenv.reset] must have been called for the unit first.

    The identifier tables are shared by the whole batch, so [keep_symbol_tables]
    must be [true] for all but the last unit of the batch. [may_reduce_heap]
    permits compacting the heap before running the external assembler; it should
    only be set for the last unit, since compaction is expensive and the shared
    state stays live until the batch is finished. *)
type rebuild_unit_from_reaped_flambda =
  keep_symbol_tables:bool ->
  may_reduce_heap:bool ->
  cmr_file:string ->
  paused_unit_infos:Cmx_format.unit_infos ->
  Compile_common.info ->
  unit

(** Create the state shared by a batch of reaped compilation unit rebuilds,
    resumed from the given .ltosol file. [batch_members] must be the compilation
    units of the batch, and the returned function must be called once per
    member, in dependency order (dependencies first). *)
type compile_from_reaped_flambda =
  ltosol_file:string ->
  batch_members:Compilation_unit.t list ->
  rebuild_unit_from_reaped_flambda

module type File_extensions = sig
  (** File extensions include exactly one dot, so they can be added with regular
      string append, and removed by Filename.strip_extension *)

  val ext_obj : string

  val ext_lib : string

  val ext_flambda_obj : string

  val ext_flambda_lib : string

  (** Name of executable produced by linking if none is given with -o, e.g.
      [a.out] under Unix. *)
  val default_executable_name : string
end

module type Backend = sig
  val backend : Compile_common.opt_backend

  val supports_metaprogramming : bool

  val link_shared :
    string list ->
    string ->
    genfns:Generic_fns.Tbl.t ->
    units_tolink:Linkenv.unit_link_info list ->
    ppf_dump:Format.formatter ->
    unit

  val link :
    Linkenv.t ->
    Linkenv.objfile_to_link list ->
    string ->
    cached_genfns_imports:Generic_fns.Partition.Set.t ->
    genfns:Generic_fns.Tbl.t ->
    units_tolink:Linkenv.unit_link_info list ->
    uses_eval:bool ->
    quoted_cmi:Compilation_unit.Name.Set.t ->
    quoted_cmx:Compilation_unit.Set.t ->
    ppf_dump:Format.formatter ->
    unit

  val link_partial : string -> string list -> unit

  val create_archive : string -> string list -> unit

  val compile_implementation :
    keep_symbol_tables:bool ->
    sourcefile:string option ->
    prefixname:string ->
    ppf_dump:Format.formatter ->
    Lambda.program ->
    unit

  val emit : emit option

  val compile_from_reaped_flambda : compile_from_reaped_flambda option

  (** This function is side-effect free. *)
  val support_files_for_eval : unit -> string list

  (** This function may have the side effect of updating the load path. *)
  val set_load_path_for_eval : unit -> unit

  include File_extensions
end
