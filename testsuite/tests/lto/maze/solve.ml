(******************************************************************************
 *                                  OxCaml                                    *
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

module type GRAPH = sig
  module Vertex : Map.OrderedType
  type t
  val iter_successors : t -> Vertex.t -> (Vertex.t -> unit) -> unit
end

module Make (Graph : GRAPH) = struct
  module Visited = Map.Make (Graph.Vertex)

  type entry = { parent : Graph.Vertex.t option; distance : int }

  type solution =
    { path : Graph.Vertex.t list;
      distance : int;
      explored : int }

  let[@inline never] run graph ~start ~goal =
    let pending = Queue.create () in
    let visited =
      ref (Visited.singleton start { parent = None; distance = 0 })
    in
    Queue.add start pending;
    let rec reconstruct vertex path =
      let entry = Visited.find vertex !visited in
      match entry.parent with
      | None -> vertex :: path
      | Some parent -> reconstruct parent (vertex :: path)
    in
    let rec search () =
      match Queue.take_opt pending with
      | None -> None
      | Some vertex ->
        let entry = Visited.find vertex !visited in
        if Graph.Vertex.compare vertex goal = 0 then
          Some
            { path = reconstruct vertex [];
              distance = entry.distance;
              explored = Visited.cardinal !visited }
        else begin
          Graph.iter_successors graph vertex (fun next ->
            if not (Visited.mem next !visited) then begin
              let entry =
                { parent = Some vertex; distance = entry.distance + 1 }
              in
              visited := Visited.add next entry !visited;
              Queue.add next pending
            end);
          search ()
        end
    in
    search ()
end
