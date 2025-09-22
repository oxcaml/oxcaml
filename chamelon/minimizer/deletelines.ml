(**************************************************************************)
(*                                                                        *)
(*                                Chamelon                                *)
(*                                                                        *)
(*                       Milla Valnet, OCamlPro                           *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Minimizer del-line: delete the last definitions *)

open Typedtree
open Utils

let rec remove_last = function
  | [] -> []
  | [ _ ] -> []
  | h :: q -> h :: remove_last q

let minimize should_remove map cur_file =
  let str = Smap.find cur_file map in
  let nstr =
    {
      str with
      str_items =
        List.rev
          (List.filter
             (fun _ -> not (should_remove ()))
             (List.rev str.str_items));
    }
  in
  Smap.add cur_file nstr map

let minimizer = { minimizer_name = "delete-lines"; minimizer_func = minimize }
