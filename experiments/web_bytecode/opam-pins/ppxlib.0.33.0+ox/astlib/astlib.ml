(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*$ open Astlib_cinaps_helpers
    open Printf $*)

(* Copy of OCaml parsetrees *)
(*$
  foreach_version (fun suffix _ ->
      printf "module Ast_%s = Ast_%s\n" suffix suffix)
*)
module Ast_414 = Ast_414
module Ast_500 = Ast_500
module Ast_999 = Ast_999
(*$*)

(* Manual migration between versions *)
(*$
  foreach_version_pair (fun x y ->
      printf "module Migrate_%s_%s = Migrate_%s_%s\n" x y x y;
      printf "module Migrate_%s_%s = Migrate_%s_%s\n" y x y x)
*)
module Migrate_414_500 = Migrate_414_500
module Migrate_500_414 = Migrate_500_414
module Migrate_500_999 = Migrate_500_999
module Migrate_999_500 = Migrate_999_500
(*$*)

(* Compiler modules *)
module Ast_metadata = Ast_metadata
module Config = Config
module Keyword = Keyword
module Location = Location
module Longident = Longident
module Parse = Parse
module Pprintast = Pprintast

let init_error_reporting_style_using_env_vars () =
  (*IF_AT_LEAST 408 Ocaml_common.Compmisc.read_clflags_from_env () *)
  (*IF_NOT_AT_LEAST 408 () *)
(** Adjust the reporting style of error messages to the environment variables OCAML_COLOR and OCAML_ERROR_STYLE. *)
