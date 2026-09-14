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

module Positions = Set.Make (Position)

type tile = Wall of int | Empty | Start | Goal | Route

type palette =
  { wall : int -> Uchar.t;
    empty : Uchar.t;
    start : Uchar.t;
    goal : Uchar.t;
    route : Uchar.t }

let unicode =
  let uchar s = Uchar.utf_decode_uchar (String.get_utf_8_uchar s 0) in
  (* Connection bits, as in Position.wall_bit: north, east, south, west. *)
  let walls =
    Array.map uchar
      [| " "; "╵"; "╶"; "└"; "╷"; "│"; "┌"; "├";
         "╴"; "┘"; "─"; "┴"; "┐"; "┤"; "┬"; "┼" |]
  in
  { wall = Array.get walls;
    empty = uchar " ";
    start = uchar "S";
    goal = uchar "E";
    route = uchar "·" }

let glyph palette = function
  | Wall connections -> palette.wall connections
  | Empty -> palette.empty
  | Start -> palette.start
  | Goal -> palette.goal
  | Route -> palette.route

let wall_canvas (maze : Maze.t) palette =
  let width = 2 * maze.width + 1 in
  let height = 2 * maze.height + 1 in
  let connections = Array.init height (fun _ -> Array.make width 0) in
  let connect (p : Position.t) direction =
    let next = Position.move p direction in
    connections.(p.y).(p.x) <-
      connections.(p.y).(p.x) lor Position.wall_bit direction;
    connections.(next.y).(next.x) <-
      connections.(next.y).(next.x)
      lor Position.wall_bit (Position.opposite direction)
  in
  Maze.iter_cells maze (fun p ->
    let x = 2 * p.Position.x and y = 2 * p.Position.y in
    Array.iter
      (fun direction ->
        if Maze.has_wall maze p direction then begin
          let origin, along =
            match direction with
            | Position.North -> { Position.x = x; y }, Position.East
            | Position.East -> { Position.x = x + 2; y }, Position.South
            | Position.South -> { Position.x = x; y = y + 2 }, Position.East
            | Position.West -> { Position.x = x; y }, Position.South
          in
          connect origin along;
          connect (Position.move origin along) along
        end)
      Position.directions);
  Array.map
    (Array.map (fun mask ->
       glyph palette (if mask = 0 then Empty else Wall mask)))
    connections

let[@inline never] make (maze : Maze.t) ~start ~goal palette =
  let glyph = glyph palette in
  fun path ->
    let on_path =
      let positions = Positions.of_list path in
      fun p -> Positions.mem p positions
    in
    let canvas = wall_canvas maze palette in
    Maze.iter_cells maze (fun p ->
      let x = 2 * p.Position.x + 1 in
      let y = 2 * p.Position.y + 1 in
      let tile =
        if Position.equal p start then Start
        else if Position.equal p goal then Goal
        else if on_path p then Route
        else Empty
      in
      canvas.(y).(x) <- glyph tile;
      List.iter
        (fun direction ->
          if not (Maze.has_wall maze p direction) then begin
            let next = Position.move p direction in
            let tile = if on_path p && on_path next then Route else Empty in
            let wall_x = p.x + next.x + 1 in
            let wall_y = p.y + next.y + 1 in
            canvas.(wall_y).(wall_x) <- glyph tile
          end)
        [Position.East; Position.South]);
    let output = Buffer.create (Array.length canvas * (2 * maze.width + 2)) in
    let add_glyph = Buffer.add_utf_8_uchar output in
    Array.iter
      (fun row ->
        Array.iter add_glyph row;
        Buffer.add_char output '\n')
      canvas;
    Buffer.contents output
