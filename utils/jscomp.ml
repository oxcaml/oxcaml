(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for invoking js_of_ocaml tooling. *)

let run_jsoo_exn ~args =
  let prog =
    match Sys.ocaml_release with
    | { Sys.extra = Some (Plus, "ox"); _ } ->
        Filename.concat Config.bindir "js_of_oxcaml"
    | _ ->
      (* during bootstrapping, js_of_oxcaml is put into our PATH by dune *)
      "js_of_oxcaml"
  in
  let cmdline = Filename.quote_command prog args in
  match Ccomp.command cmdline with
  | 0 -> ()
  | _ -> raise (Sys_error cmdline)
