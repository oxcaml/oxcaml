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

(* The [Ocaml_or_oxcaml] module for a stock OCaml compiler. See
   ocaml_or_oxcaml.oxcaml.ml for the OxCaml one, and the dune file for how
   the build chooses between them. *)

module Float32 = struct
  type t

  let of_float _ = assert false

  let to_float _ = assert false

  let of_string _ = assert false
end

let with_async_exns f = f ()
