(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Edge = Fdo_counter_identity.Edge
module Hash = Fdo_counter_identity.Hash
module Function_body_hash = Fdo_counter_identity.Function_body_hash
module Definition_id = Fdo_counter_identity.Definition_id
module Position = Fdo_counter_identity.Position

module Hashed = struct
  type t = Hash.t list
end

type t =
  { position : Position.t;
    inlining_stack : Position.t list
  }

let inline t ~at =
  { t with
    inlining_stack = t.inlining_stack @ (at.position :: at.inlining_stack)
  }

let equal a b =
  Position.equal a.position b.position
  && List.equal Position.equal a.inlining_stack b.inlining_stack

let specialize t ~at =
  let specialize_position position =
    Position.specialize position ~at:(at.position :: at.inlining_stack)
  in
  (* Only the outermost position belongs to the copied function. Inner positions
     belong to callees inlined into it and retain their own identities. *)
  let rec outermost = function
    | [] -> []
    | [p] -> [specialize_position p]
    | p :: rest -> p :: outermost rest
  in
  match t.inlining_stack with
  | [] -> { t with position = specialize_position t.position }
  | stack -> { t with inlining_stack = outermost stack }

let to_string { position; inlining_stack } =
  String.concat " <- "
    (List.map Position.to_string (position :: inlining_stack))

let hash { position; inlining_stack } =
  List.map Position.hash (position :: inlining_stack)

let add_all existing counters =
  List.rev
    (List.fold_left
       (fun acc counter ->
         if List.exists (equal counter) acc then acc else counter :: acc)
       (List.rev existing) counters)
