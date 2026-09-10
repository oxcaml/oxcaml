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

type cell = { mutable walls : int }

type t = { width : int; height : int; cells : cell array }

let create ~width ~height =
  { width;
    height;
    cells = Array.init (width * height) (fun _ -> { walls = 15 }) }

let contains t (p : Position.t) =
  p.x >= 0 && p.x < t.width && p.y >= 0 && p.y < t.height

let cell t p = t.cells.(Position.index p ~width:t.width)

let has_wall t p direction =
  (cell t p).walls land Position.wall_bit direction <> 0

let remove_wall t p direction =
  let cell = cell t p in
  cell.walls <- cell.walls land lnot (Position.wall_bit direction)

let[@inline never] carve t p direction =
  let next = Position.move p direction in
  remove_wall t p direction;
  remove_wall t next (Position.opposite direction)

let iter_neighbours t p f =
  Array.iter
    (fun direction ->
      let next = Position.move p direction in
      if contains t next then f direction next)
    Position.directions

let iter_cells t f =
  Array.iteri (fun i _ -> f (Position.of_index ~width:t.width i)) t.cells
