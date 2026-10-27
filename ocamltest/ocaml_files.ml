(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Locations of files in the OCaml source tree *)

open Ocamltest_stdlib

type runtime_variant =
  | Normal
  | Debug
  | Instrumented

let runtime_variant() =
  let use_runtime = Sys.safe_getenv "USE_RUNTIME" in
  if use_runtime="d" then Debug
  else if use_runtime="i" then Instrumented
  else Normal

let srcdir_path = Ocaml_directories.srcdir_path

let ocamlrun =
  let runtime = match runtime_variant () with
    | Normal -> "ocamlrun"
    | Debug -> "ocamlrund"
    | Instrumented -> "ocamlruni" in
  let ocamlrunfile = Filename.mkexe runtime in
  srcdir_path ["runtime"; ocamlrunfile]

let ocamlrun_sys =
  Option.value (Options.file_override "ocamlrun_sys") ~default:ocamlrun

let ocamlc =
  srcdir_path [Filename.mkexe "ocamlc"]

let ocaml =
  srcdir_path [Filename.mkexe "ocaml"]

let ocamlc_dot_opt =
  srcdir_path [Filename.mkexe "ocamlc.opt"]

let ocamlopt =
  srcdir_path [Filename.mkexe "ocamlopt"]

let ocamlopt_dot_opt =
  srcdir_path [Filename.mkexe "ocamlopt.opt"]

let ocamlnat =
  srcdir_path [Filename.mkexe "ocamlnat"]

let cmpbyt =
  srcdir_path ["tools"; Filename.mkexe "cmpbyt"]

let expect =
  srcdir_path ["testsuite"; "tools";
     Filename.mkexe "expect"]

let expectnat =
  srcdir_path ["testsuite"; "tools";
     Filename.mkexe "expectnat"]

let ocamllex =
  srcdir_path ["lex"; Filename.mkexe "ocamllex"]

let ocamlyacc =
  srcdir_path ["yacc"; Filename.mkexe "ocamlyacc"]

let ocamldoc =
  srcdir_path ["ocamldoc"; Filename.mkexe "ocamldoc"]

let ocamldebug =
  srcdir_path ["debugger"; Filename.mkexe "ocamldebug"]

let ocamlobjinfo =
  srcdir_path ["tools"; Filename.mkexe "ocamlobjinfo"]

let ocamlmklib =
  srcdir_path ["tools"; Filename.mkexe "ocamlmklib"]

let ocamlfilt =
  srcdir_path ["tools"; Filename.mkexe "ocamlfilt"]

let codegen =
  srcdir_path ["testsuite"; "tools"; Filename.mkexe "codegen"]

let fexprc =
  srcdir_path ["testsuite"; "tools"; Filename.mkexe "fexprc"]

let asmgen_archmod =
  let objname =
    "asmgen_" ^ Ocamltest_config.arch ^ "." ^ Ocamltest_config.objext
  in
  srcdir_path ["testsuite"; "tools"; objname]
