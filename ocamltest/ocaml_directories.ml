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

(* Locations of directories in the OCaml source tree *)

open Ocamltest_stdlib

let srcdir =
  if Options.no_ocamlsrcdir then "@OCAML@"
  else Sys.getenv_with_default_value "OCAMLSRCDIR" Ocamltest_config.ocamlsrcdir

let startup_directory = Sys.getcwd ()

let srcdir_path p =
  match Options.file_override (String.concat "/" p) with
  | Some p ->
     if Filename.is_relative p then
       Filename.concat startup_directory p
     else
       p
  | None ->
     Filename.make_path (srcdir :: p)

let stdlib = srcdir_path ["stdlib"]

let libunix = srcdir_path ["otherlibs"; "unix"]

let toplevel = srcdir_path ["toplevel"]

let runtime = srcdir_path ["runtime"]

let tools = srcdir_path ["tools"]
