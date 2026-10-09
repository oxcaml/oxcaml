(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

open! Stdlib
module J = Javascript

let zero = J.ENum (J.Num.of_targetint Targetint.zero)

let one = J.ENum (J.Num.of_targetint Targetint.one)

(* JavaScript engines recognize the pattern 'typeof x==="number"'; if the string is
   shared, less efficient code is generated. *)
let type_of_is_number binop e =
  J.EBin (binop, J.EUn (J.Typeof, e), J.EStr (Utf8_string.of_string_exn "number"))

let is_block e = type_of_is_number J.NotEqEq e

let is_immediate e = type_of_is_number J.EqEqEq e

module Block = struct
  let make ~tag ~args =
    let tag_elt = J.Element (J.ENum (J.Num.of_targetint (Targetint.of_int_exn tag))) in
    J.EArr (tag_elt :: args)

  (* CR-someday selee: We use optional chaining because issues can arise when
     [Js.Unsafe.*] functions are used with code produced by OxCaml's ocamlj.
     When constructing an [any array], Flambda 2 first uses this primitive to
     check at runtime whether [any] is actually a boxed float, to determine
     whether it should create a float array or a normal array. Unfortunately
     [any] can be [undefined] (in JS), so accessing field 0 raises an error.

     A more principled fix would probably be to refactor [Js.Unsafe], but this
     is a much larger change. *)
  let tag e = J.EAccess (e, ANullish, zero)

  let field e idx =
    let adjusted = J.ENum (J.Num.of_targetint (Targetint.of_int_exn (idx + 1))) in
    J.EAccess (e, ANormal, adjusted)
end

module Array = struct
  let make = Block.make

  let length e =
    let underlying = J.EDot (e, ANormal, Utf8_string.of_string_exn "length") in
    J.EBin (J.Minus, underlying, one)

  let field e i =
    match i with
    | J.ENum n ->
        let idx = J.Num.to_targetint n in
        let adjusted = J.ENum (J.Num.of_targetint (Targetint.succ idx)) in
        J.EAccess (e, ANormal, adjusted)
    | J.EUn (J.Neg, _) -> failwith "Negative field indexes are not allowed"
    | _ ->
        let adjusted = J.EBin (J.Plus, i, one) in
        J.EAccess (e, ANormal, adjusted)
end
