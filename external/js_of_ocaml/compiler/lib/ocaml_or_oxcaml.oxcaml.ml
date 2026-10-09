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

(* The [Ocaml_or_oxcaml] module for an OxCaml compiler. See
   ocaml_or_oxcaml.ocaml.ml for the stock OCaml one, and the dune file for
   how the build chooses between them. *)

module Float32 = struct
  type t = float32

  external of_float : float -> t = "%float32offloat"

  external to_float : t -> float = "%floatoffloat32"

  (* In javascript/wasm, we define float32 parsing as rounding the 64-bit result.
     This is not equivalent to native code, which parses to 32 bits directly. *)
  let of_string s = float_of_string s |> of_float
end

let with_async_exns = Sys.with_async_exns
