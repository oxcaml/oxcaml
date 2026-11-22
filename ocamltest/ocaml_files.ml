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

let js_of_ocaml =
  Filename.make_path
    [Ocaml_directories.srcdir; Filename.mkexe "js_of_ocaml"]

let wasm_of_ocaml =
  Filename.make_path
    [Ocaml_directories.srcdir; Filename.mkexe "wasm_of_ocaml"]

let jsir_js_available = Sys.file_exists js_of_ocaml

let jsir_wasm_available = Sys.file_exists wasm_of_ocaml

let node = Sys.getenv_with_default_value "NODE" "node"

let node_wasm_flags =
  Sys.getenv_with_default_value "NODE_WASM_FLAGS"
    "--stack-size=10000 --experimental-wasm-jspi"

let cmpbyt =
  srcdir_path ["tools"; Filename.mkexe "cmpbyt"]

let expect =
  srcdir_path ["testsuite"; "tools";
     Filename.mkexe "expect"]

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

type native_host =
  | Expectnat
  | Ocamlnat
  | Codegen

let native_host_name = function
  | Expectnat -> "expect.opt"
  | Ocamlnat -> "ocamlnat"
  | Codegen -> "codegen"

let native_host ~variant host =
  let file =
    match host with
    | Expectnat -> "expectnat"
    | Ocamlnat -> "ocamlnat"
    | Codegen -> "codegen"
  in
  match variant, host with
  | None, Ocamlnat -> srcdir_path [Filename.mkexe file]
  | None, (Expectnat | Codegen) ->
    srcdir_path ["testsuite"; "tools"; Filename.mkexe file]
  | Some variant, (Expectnat | Ocamlnat | Codegen) ->
    srcdir_path
      ["testsuite"; "tools"; "variants"; variant; Filename.mkexe file]

let fexprc =
  srcdir_path ["testsuite"; "tools"; Filename.mkexe "fexprc"]

let asmgen_archmod =
  let objname =
    "asmgen_" ^ Ocamltest_config.arch ^ "." ^ Ocamltest_config.objext
  in
  srcdir_path ["testsuite"; "tools"; objname]
