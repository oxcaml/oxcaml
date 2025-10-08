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

module CU := Compilation_unit

type filepath := string

type unit_link_info =
  { name : Compilation_unit.t;
    defines : Compilation_unit.t list;
    file_name : string;
    crc : Digest.t;
    (* for shared libs *)
    dynunit : Cmxs_format.dynunit option
  }

val extract_crc_interfaces : unit -> Import_info.t list

val extract_crc_implementations : unit -> Import_info.t list

val lib_ccopts : unit -> string list

val lib_ccobjs : unit -> filepath list

val reset : unit -> unit

val make_globals_map :
  unit_link_info list ->
  (CU.t * Digest.t option * Digest.t option * Symbol.t list) list

val add_ccobjs : filepath -> Cmx_format.library_infos -> unit

val is_required : Compilation_unit.t -> bool

val add_required : filepath * CU.Name.t option -> Import_info.t -> unit

val remove_required : CU.t -> unit

val extract_missing_globals : unit -> (CU.t * filepath list) list

val check_cmi_consistency : filepath -> Import_info.t array -> unit

val check_cmx_consistency : filepath -> Import_info.t array -> unit

val check_consistency :
  unit:unit_link_info -> Import_info.t array -> Import_info.t array -> unit

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (Compilation_unit.t * string list) list
  | Inconsistent_interface of Compilation_unit.Name.t * filepath * filepath
  | Inconsistent_implementation of Compilation_unit.t * filepath * filepath
  | Multiple_definition of Compilation_unit.Name.t * filepath * filepath
  | Missing_cmx of filepath * Compilation_unit.t
  | Linking_error of int
  | Archiver_error of string

exception Error of error
