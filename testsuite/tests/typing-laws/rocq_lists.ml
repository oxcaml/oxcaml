(* TEST
 flags = "-extension laws -laws-rocq rocq_lists.v";
 setup-ocamlc.byte-build-env;
 module = "rocq_lists.ml";
 ocamlc.byte;
 output = "${test_build_directory}/rocq_lists.v";
 reference = "${test_source_directory}/rocq_lists.v.reference";
 check-program-output;
*)

(* The Rocq file of a list module: recursive functions, matches on lists,
   references to the standard library, and laws over type variables. *)

let is_empty = function [] -> true | _ :: _ -> false

let rec rev_append l1 l2 =
  match l1 with [] -> l2 | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l []

let rec length = function [] -> 0 | _ :: l -> 1 + length l

let rec map f = function [] -> [] | x :: l -> f x :: map f l

let singleton x = [ x ]
let digits = [ 1; 2; 3 ]
let length_of_digits = length digits

law? rev_rev (xs : 'a list) : rev (rev xs) =:= xs

law? rev_nonempty (xs : 'a list) :
  not (is_empty xs) ===> not (is_empty (rev xs))

law? rev_stdlib (xs : 'a list) : rev xs =:= List.rev xs

law? length_map (f : 'a -> 'b) (xs : 'a list) :
  length (map f xs) = length xs

law? length_nonneg (xs : int list) : length xs >= 0 && List.length xs >= 0

law? map_not (bs : bool list) : map not (map not bs) =:= bs
