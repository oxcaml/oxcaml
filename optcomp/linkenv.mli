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
    imports_cmx : Import_info.t list;
    codegen_fingerprint : Codegen_variant.fingerprint;
    polled : bool;
    (* for shared libs *)
    dynunit : Cmxs_format.dynunit option
  }

(** An object file or archive to pass to the linker, together with the
    compilation units it contributes to the link (for an archive, only the units
    of its required members). *)
type objfile_to_link =
  { path : string;
    units : Compilation_unit.t list
  }

(** Values of type [t] are mutable structures. *)
type t

val create : unit -> t

val extract_crc_interfaces : t -> Import_info.t list

val extract_crc_implementations : t -> Import_info.t list

val lib_ccopts : t -> string list

val lib_ccobjs : t -> filepath list

val make_globals_map :
  t ->
  unit_link_info list ->
  (CU.t * Digest.t option * Digest.t option * Symbol.t list) list

val add_ccobjs : t -> filepath -> Cmx_format.library_infos -> unit

val is_required : t -> Compilation_unit.t -> bool

val add_required : t -> filepath * CU.Name.t option -> Import_info.t -> unit

val remove_required : t -> CU.t -> unit

val add_quoted_cmi : t -> CU.Name.t list -> unit

val add_quoted_cmx : t -> CU.t list -> unit

val get_quoted_cmi : t -> CU.Name.Set.t

val get_quoted_cmx : t -> CU.Set.t

val extract_missing_globals : t -> (CU.t * filepath list) list

(* Checks what an archive imports against what the files checked so far import
   and define. The units of the archive that the link uses are then checked one
   by one, with [check_consistency] (and no imports). *)
val check_library_consistency :
  t -> filepath -> Cmx_format.library_infos -> unit

val check_consistency :
  t -> unit:unit_link_info -> Import_info.t array -> Import_info.t array -> unit

(* Whether a file that the link has checked, an archive or the file of one unit,
   holds a unit of that name, which the link need not have taken from it. *)
val file_holds_unit : t -> filepath -> CU.t -> bool

type error =
  | File_not_found of filepath
    (* The link input [requested_filename] (a [.cmx] or [.cmxa]) was resolved
       through a manifest entry (see [-I-manifest]), but its accompanying object
       file [object_filename] has no manifest entry and was not found in the
       load path. *)
  | Companion_object_file_not_found_via_manifest of
      { object_filename : filepath;
        requested_filename : filepath
      }
  | Not_an_object_file of filepath
  | Missing_implementations of (Compilation_unit.t * string list) list
  | Inconsistent_interface of Compilation_unit.Name.t * filepath * filepath
  | Inconsistent_implementation of Compilation_unit.t * filepath * filepath
  | Multiple_definition of Compilation_unit.Name.t * filepath * filepath
  | Missing_cmx of filepath * Compilation_unit.t
  | Linking_error of int
  | Archiver_error of string
  | Metaprogramming_not_supported_by_backend of filepath
  | Requires_metaprogramming_without_flag of filepath
  | Inconsistent_codegen_variant of
      { unit_name : Compilation_unit.t;
        filename : filepath;
        saved : Codegen_variant.fingerprint;
        current : Codegen_variant.fingerprint
      }
    (* The runtime archive for a non-default codegen variant (e.g.
       libasmrun_fp.a) was not found in the load path. *)
  | Codegen_variant_runtime_not_found of
      { filename : filepath;
        variant : string
      }
    (* -use-cached-generic-functions was passed without an explicit path and the
       selected variant's standard-library root has no cached-generic-functions
       archive. *)
  | Cached_generic_functions_not_found of
      { filename : filepath;
        variant : string
      }
    (* The two errors below are for -use-cached-generic-functions without an
       explicit path as well, where the archive has no place: the one above is
       only for a file that can be generated where it is missing, in a library
       tree that serves the selected variant.

       The installation has no library tree that serves the selected variant. *)
  | Cached_generic_functions_tree_not_found of { variant : string }
    (* The library root, which stands for the tree of the selected variant, is
       built for another one: so is its archive, if it has one. *)
  | Cached_generic_functions_root_mismatch of
      { root : filepath;
        variant : string
      }
    (* The file that -cached-generic-functions-path names does not exist, and
       neither does [also_looked_up], its counterpart in the selected variant's
       tree, when the path lies inside the default library root. *)
  | Cached_generic_functions_file_not_found of
      { filename : filepath;
        also_looked_up : filepath option
      }

exception Error of error

(** Whether [Config.standard_library] is itself a library root that serves the
    given variant (OCAMLLIB naming a variant's tree, say), judged by the codegen
    fingerprint of the standard library there. *)
val standard_library_is_built_for : Codegen_variant.t -> bool

(** Whether that fingerprint is one of other settings than the variant's: false
    when the root has no standard library to read it from. *)
val standard_library_is_built_for_another : Codegen_variant.t -> bool
