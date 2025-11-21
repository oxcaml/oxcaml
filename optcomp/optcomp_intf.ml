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

module type File_extensions = sig
  (** File extensions include exactly one dot, so they can be added with regular string
      append, and removed by Filename.strip_extension *)

  val ext_obj : string

  val ext_lib : string

  val ext_flambda_obj : string

  val ext_flambda_lib : string

  (** Name of executable produced by linking if none is given with -o,
      e.g. [a.out] under Unix. *)
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
    string list ->
    string ->
    cached_genfns_imports:Generic_fns.Partition.Set.t ->
    genfns:Generic_fns.Tbl.t ->
    units_tolink:Linkenv.unit_link_info list ->
    uses_eval:bool ->
    quoted_globals:Compilation_unit.Name.Set.t ->
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

  (** This function is side-effect free. *)
  val support_files_for_eval : unit -> string list

  (** This function may have the side effect of updating the load path. *)
  val set_load_path_for_eval : unit -> unit

  include File_extensions
end
