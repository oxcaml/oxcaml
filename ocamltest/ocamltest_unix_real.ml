(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 David Allsopp, OCaml Labs, Cambridge.                  *)
(*                                                                        *)
(*   Copyright 2020 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Unix.gettimeofday and Unix.has_symlink never raise *)
let has_symlink = Unix.has_symlink
let gettimeofday = Unix.gettimeofday

(* Convert Unix_error to Sys_error *)
let wrap f x =
  try f x
  with Unix.Unix_error(err, fn_name, arg) ->
    let msg =
      Printf.sprintf "%s failed on %S with %s"
                     fn_name arg (Unix.error_message err)
    in
      raise (Sys_error msg)

(* These must be eta-expanded otherwise we get the following error when using
   OxCaml's stdlib:

   Error: This value is local
       but is expected to be local to the parent region or global
       because it is an argument in a tail call.
   Hint: This is a partial application
         Adding 1 more argument will make the value non-local
 *)
let symlink ?to_dir source =
  wrap (fun dest -> Unix.symlink ?to_dir source dest)
let chmod file =
  wrap (fun perms -> Unix.chmod file perms)
