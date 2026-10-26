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

module Position_hash = Fdo_counter_identity.Position_hash
module Body_hash = Fdo_counter_identity.Body_hash
module Name = Fdo_counter_identity.Name
module Function = Fdo_counter_identity.Function
module Position = Fdo_counter_identity.Position

module Hashed = struct
  type t = Position_hash.t list
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

(* Only the outermost position belongs to the copied function. Inner positions
   belong to callees inlined into it and keep their identities. *)
let specialize t ~at =
  let specialize_position position =
    Position.specialize position ~at:(at.position :: at.inlining_stack)
  in
  match List.rev t.inlining_stack with
  | [] -> { t with position = specialize_position t.position }
  | outermost :: inner ->
    { t with
      inlining_stack = List.rev (specialize_position outermost :: inner)
    }

let to_string { position; inlining_stack } =
  position :: inlining_stack
  |> List.map Position.to_string
  |> String.concat " <- "

let hash { position; inlining_stack } =
  List.map Position.hash (position :: inlining_stack)

let add_all existing counters =
  List.fold_left
    (fun acc counter ->
      if List.exists (equal counter) acc then acc else acc @ [counter])
    existing counters
