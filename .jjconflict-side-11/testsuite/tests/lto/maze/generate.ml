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

module type RNG = sig
  type t
  val int : t -> int -> int
end

module Make (Rng : RNG) = struct
  let shuffle rng items =
    for i = Array.length items - 1 downto 1 do
      let j = Rng.int rng (i + 1) in
      let item = items.(i) in
      items.(i) <- items.(j);
      items.(j) <- item
    done

  let[@inline never] run rng ~width ~height =
    let maze = Maze.create ~width ~height in
    let visited = Array.make (width * height) false in
    let index = Position.index ~width in
    let rec visit p =
      visited.(index p) <- true;
      let neighbours = ref [] in
      Maze.iter_neighbours maze p (fun direction next ->
        neighbours := (direction, next) :: !neighbours);
      let choices = Array.of_list (List.rev !neighbours) in
      shuffle rng choices;
      Array.iter
        (fun (direction, next) ->
          if not visited.(index next) then begin
            Maze.carve maze p direction;
            visit next
          end)
        choices
    in
    visit { Position.x = 0; y = 0 };
    maze

  let remove_walls rng maze ~one_in =
    Maze.iter_cells maze (fun p ->
      Maze.iter_neighbours maze p (fun direction _ ->
        (* Consider each interior wall once. *)
        match direction with
        | Position.North | Position.West -> ()
        | Position.East | Position.South ->
          if Maze.has_wall maze p direction && Rng.int rng one_in = 0 then
            Maze.carve maze p direction))
end
