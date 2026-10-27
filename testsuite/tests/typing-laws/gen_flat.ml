(* TEST
 flags = "-extension laws";
 {
   setup-ocamlopt.opt-build-env;
   module = "gen_flat.ml";
   ocamlopt.opt;
   flags = "-generate-laws-implementation -o gen_flat_laws.ml";
   module = "gen_flat.cmx";
   ocamlopt.opt;
   output = "${test_build_directory}/gen_flat_laws.ml";
   reference = "${test_source_directory}/gen_flat_laws.ml.reference";
   check-program-output;
   flags = "-extension laws";
   module = "gen_flat_laws.ml";
   ocamlopt.opt;
 }{
   setup-ocamlc.byte-build-env;
   module = "gen_flat.ml";
   ocamlc.byte;
   flags = "-generate-laws-implementation -o gen_flat_laws.ml";
   module = "gen_flat.cmo";
   ocamlc.byte;
   output = "${test_build_directory}/gen_flat_laws.ml";
   reference = "${test_source_directory}/gen_flat_laws.ml.reference";
   check-program-output;
 }
*)

(* The laws file of a unit without functors, from native code and from
   bytecode. The laws exercise the corners of the generated file: type
   variables, modes, unboxed types, keywords as names, type aliases and
   assumptions. *)

let apply (g : local_ int -> int) = g 1
external of_unboxed : float# -> float = "%box_float"

law? poly (xs : 'a list) (f : 'a -> 'b) :
  List.length (List.map f xs) = List.length xs
law? modes (g : local_ int -> int) : apply g = apply g
law? unboxed (x : float#) : of_unboxed x = of_unboxed x
law? \#type (\#method : int) : \#method = \#method
law? alias (x : (int as 'a)) : x = x
law? assumption (x : int) : x > 0 ===> x <> 0
law? constants (c : char) : c = 'a' ===> 1.5 >= -0.5 && {|s|} <> "t"

type shape = Circle of float | Square of float
type other = Circle of int
law? constructors (s : shape) :
  s = (Circle 1. : shape) ===> s <> Square 1. && Some s <> None
law? tuples (x : int) (y : string) : (x, y) = (x, y) && `A x <> `B

type point = { x : float; y : float }
type other_point = { x : int }
let g () = ()
law? control (x : int) :
  (if x > 0 then x else 0) >= 0 && (g (); x) = x
  && Lazy.force (lazy x) = x && (assert (x = x); true)
law? binding (s : shape) :
  let a = (match s with Circle r -> r | Square w -> w) in
  (try a >= 0. with Not_found -> false) || a < 0.
law? functions (xs : int list) :
  (fun ?(min = 0) x -> x >= min) 1 && List.for_all (fun x -> x >= 0) xs
  || List.exists (function 0 -> true | _ -> false) xs
module Ops = struct let add = ( + ) end
law? local (x : int) : Ops.(add x 1 = x + 1)
law? records (p : point) :
  ({ x = 0.; y = p.y } : point) = { p with x = 0. } && (p : point).x >= 0.
