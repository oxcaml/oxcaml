(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Text-mode driver for ppx_optcomp_light and sedlex.ppx: reads and prints
   source, so it does not need to be built with the compiler consuming its
   output. *)

(* Menhir's inference round preprocesses [*.ml.mock] files, an extension
   ppxlib rejects; re-exec with [-impl] added for them. *)
let () =
  match Sys.getenv_opt "JSOO_PPX_INNER" with
  | Some _ -> Ppxlib.Driver.standalone ()
  | None ->
      let args =
        Array.to_list Sys.argv
        |> List.tl
        |> List.concat_map (fun arg ->
               if Filename.check_suffix arg ".ml.mock" then [ "-impl"; arg ] else [ arg ])
      in
      Unix.putenv "JSOO_PPX_INNER" "1";
      Unix.execv Sys.executable_name (Array.of_list (Sys.executable_name :: args))
