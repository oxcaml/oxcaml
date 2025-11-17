(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Format of .cmxs files *)

(* Each .cmxs dynamically-loaded plugin contains a symbol
   "caml_plugin_header" containing the following info
   (as an externed record) *)

type dynunit = {
  dynu_name: Compilation_unit.t;
  dynu_crc: Digest.t;
  dynu_imports_cmi_bitmap: Misc.Bitmap.t; (** bitmap into [dynu_imports_cmi] *)
  dynu_imports_cmx_bitmap: Misc.Bitmap.t; (** bitmap into [dynu_imports_cmx] *)
  dynu_imports_cmx_self_index: int option;
    (** Index of this unit in [dynu_imports_cmx], if it appears there.
        Used to track initialization order. *)
  dynu_quoted_globals: Compilation_unit.Name.t array;
  (* CR sspies: Probably we want to lift the array here to the dynheader using a
     bitmap like in the two cases above. Do that when they are more stable. *)
  dynu_defines: Compilation_unit.t list;
}

type dynheader = {
  dynu_magic: string;
  dynu_units: dynunit list;
  dynu_imports_cmi: Import_info.t array;
  dynu_imports_cmx: Import_info.t array;
}
