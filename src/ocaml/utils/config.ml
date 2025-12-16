(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)

(* merlin-jst: All magic numbers ending in `500` (or higher) should not change when we
   upgrade the upstream Merlin version.  Otherwise, this should track
   `utils/config.mlp` from the compiler.  Also, the above warning comment about
   `config.mlbuild` is outdated and can be ignored. *)

(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

(* When bumping this number, be sure to also update ../typing/magic_numbers.ml *)
let cmi_magic_number = "Caml1999I571"

let ast_impl_magic_number = "Caml1999M571"
let ast_intf_magic_number = "Caml1999N571"
let cmt_magic_number = "Caml1999T571"
let cms_magic_number = "Caml1999S571"
let index_magic_number = "Merl2023I571"

let interface_suffix = ref ".mli"

let max_tag = 245
let flat_float_array = false

let reserved_header_bits = 8
let runtime5 = true
let syntax_quotations = true

let merlin = true

module Magic_numbers = struct
  type t =
    { cmi_magic_number : string;
      ast_intf_magic_number : string;
      ast_impl_magic_number : string;
      cmt_magic_number : string;
      cms_magic_number : string;
      index_magic_number : string
    }

  let current =
    { cmi_magic_number;
      ast_intf_magic_number;
      ast_impl_magic_number;
      cmt_magic_number;
      cms_magic_number;
      index_magic_number
    }

  let to_json t =
    let nums =
      [ ("cmi_magic_number", t.cmi_magic_number);
        ("ast_intf_magic_number", t.ast_intf_magic_number);
        ("ast_impl_magic_number", t.ast_impl_magic_number);
        ("cmt_magic_number", t.cmt_magic_number);
        ("cms_magic_number", t.cms_magic_number);
        ("index_magic_number", t.index_magic_number)
      ]
    in
    `Assoc
      (List.map
         (fun (key, value) -> (key, Merlin_utils.Std.Json.string value))
         nums)
end
