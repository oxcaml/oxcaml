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

(* Neither ppxlib's nor the compiler's pretty-printer parenthesizes class-type
   arguments, and the output does not always reparse: OxCaml lexes [#] glued
   to a preceding type variable as an unboxed-type hash (['a#js_array t]
   errors, ['a #js_array t] is fine), and [(< .. > as 'a) #event t] comes out
   as the unparseable [< .. > as 'a#event t]. Wrapping every class-type
   argument in a no-op attribute forces both printers to parenthesize it,
   which reparses unambiguously ([('a[@jsoo_ppx.parens ])#js_array t]). *)
let paren_class_args =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! core_type t =
      let t = super#core_type t in
      match t.ptyp_desc with
      | Ptyp_class (name, args) ->
          let parenthesize (a : Ppxlib.core_type) =
            let loc = { a.ptyp_loc with loc_ghost = true } in
            let attr : Ppxlib.attribute =
              { attr_name = { txt = "jsoo_ppx.parens"; loc }
              ; attr_payload = PStr []
              ; attr_loc = loc
              }
            in
            { a with ptyp_attributes = attr :: a.ptyp_attributes }
          in
          { t with ptyp_desc = Ptyp_class (name, List.map parenthesize args) }
      | _ -> t
  end

let () =
  Ppxlib.Driver.register_transformation
    "jsoo_ppx_paren_class_args"
    ~impl:paren_class_args#structure
    ~intf:paren_class_args#signature

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
