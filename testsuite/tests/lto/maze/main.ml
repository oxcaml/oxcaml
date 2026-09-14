(* TEST
 modules = "position.ml maze.ml generate.ml solve.ml render.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-O3 -flambda2-reaper -support-lto -flambda2-result-types-all-functions";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve position.cmr maze.cmr generate.cmr solve.cmr render.cmr main.cmr";
 last_flags = "-o maze.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild position.cmr maze.cmr generate.cmr maze.ltosol";
 last_flags = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild solve.cmr render.cmr main.cmr maze.ltosol";
 last_flags = "";
 ocamlopt.opt;

 flags = "";
 all_modules = "position.reaped.cmx maze.reaped.cmx generate.reaped.cmx solve.reaped.cmx render.reaped.cmx main.reaped.cmx";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
 run;
 check-program-output;
*)

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

(* Keep calls across units as well as inlinable helpers: the LTO rebuild must
   agree on record and closure representations. Only the path is used from
   the solver's result; its statistics are deliberately left unused. *)

module Generator = Generate.Make (struct
  type t = Random.State.t
  let int : t -> int -> int = fun state bound -> Random.State.int state bound
end)

module Graph = struct
  module Vertex = Position
  type t = Maze.t

  let iter_successors maze p f =
    Maze.iter_neighbours maze p (fun direction next ->
      if not (Maze.has_wall maze p direction) then f next)
end

module Solver = Solve.Make (Graph)

let () =
  let width = 39 and height = 24 in
  let rng = Random.State.make [|42|] in
  let maze = Generator.run rng ~width ~height in
  Generator.remove_walls rng maze ~one_in:10;
  let start = { Position.x = 0; y = 0 } in
  let goal = { Position.x = width - 1; y = height - 1 } in
  let render = Render.make maze ~start ~goal Render.unicode in
  match Solver.run maze ~start ~goal with
  | None -> failwith "No route through the maze"
  | Some solution -> print_string (render solution.path)
