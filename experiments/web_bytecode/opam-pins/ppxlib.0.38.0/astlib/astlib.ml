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
module Ast_408 = Ast_408
module Ast_409 = Ast_409
module Ast_410 = Ast_410
module Ast_411 = Ast_411
module Ast_412 = Ast_412
module Ast_413 = Ast_413
module Ast_414 = Ast_414
module Ast_500 = Ast_500
module Ast_501 = Ast_501
module Ast_502 = Ast_502
module Ast_503 = Ast_503
module Ast_504 = Ast_504
module Ast_505 = Ast_505
(*$*)

(* Manual migration between versions *)
(*$
  foreach_version_pair (fun x y ->
      printf "module Migrate_%s_%s = Migrate_%s_%s\n" x y x y;
      printf "module Migrate_%s_%s = Migrate_%s_%s\n" y x y x)
*)
module Migrate_408_409 = Migrate_408_409
module Migrate_409_408 = Migrate_409_408
module Migrate_409_410 = Migrate_409_410
module Migrate_410_409 = Migrate_410_409
module Migrate_410_411 = Migrate_410_411
module Migrate_411_410 = Migrate_411_410
module Migrate_411_412 = Migrate_411_412
module Migrate_412_411 = Migrate_412_411
module Migrate_412_413 = Migrate_412_413
module Migrate_413_412 = Migrate_413_412
module Migrate_413_414 = Migrate_413_414
module Migrate_414_413 = Migrate_414_413
module Migrate_414_500 = Migrate_414_500
module Migrate_500_414 = Migrate_500_414
module Migrate_500_501 = Migrate_500_501
module Migrate_501_500 = Migrate_501_500
module Migrate_501_502 = Migrate_501_502
module Migrate_502_501 = Migrate_502_501
module Migrate_502_503 = Migrate_502_503
module Migrate_503_502 = Migrate_503_502
module Migrate_503_504 = Migrate_503_504
module Migrate_504_503 = Migrate_504_503
module Migrate_504_505 = Migrate_504_505
module Migrate_505_504 = Migrate_505_504
(*$*)

(* Compiler modules *)
module Ast_metadata = Ast_metadata
module Config = Config
module Keyword = Keyword
module Location = Location
module Longident = Longident
module Parse = Parse
module Pprintast = Pprintast
module Compiler_pprintast = struct
  include Ocaml_common.Pprintast

  let structure_item fmt t = structure fmt [t]
  let signature_item fmt t = signature fmt [t]

  exception Unavailable

  (*IF_NOT_AT_LEAST 414 let class_field _fmt _t = raise Unavailable *)
  (*IF_NOT_AT_LEAST 414 let class_type_field _fmt _t = raise Unavailable *)
  (*IF_NOT_AT_LEAST 414 let class_expr _fmt _t = raise Unavailable *)
  (*IF_NOT_AT_LEAST 414 let class_type _fmt _t = raise Unavailable *)
  (*IF_NOT_AT_LEAST 414 let module_type _fmt _t = raise Unavailable *)
  (*IF_NOT_AT_LEAST 414 let module_expr _fmt _t = raise Unavailable *)
end

module Clean = Clean

let init_error_reporting_style_using_env_vars () =
  Ocaml_common.Compmisc.read_clflags_from_env ()
(** Adjust the reporting style of error messages to the environment variables OCAML_COLOR and OCAML_ERROR_STYLE. *)
