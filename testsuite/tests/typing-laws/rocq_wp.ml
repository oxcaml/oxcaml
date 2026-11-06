(* TEST
 flags = "-extension laws -laws-rocq rocq_wp.v";
 setup-ocamlc.byte-build-env;
 module = "rocq_wp.ml";
 ocamlc.byte;
 output = "${test_build_directory}/rocq_wp.v";
 reference = "${test_source_directory}/rocq_wp.v.reference";
 check-program-output;
*)

(* The Rocq file of laws with [let%wp] (Phase 2, weakest preconditions) and
   with modes (Phase 3, separation logic): the logic of each law follows
   from its syntax and parameters. *)

let rec first p n = if p n then n else first p (n + 1)

let rec all_in_range n i f =
  if n < i then f n && all_in_range (n + 1) i f else true

let rec length = function [] -> 0 | _ :: l -> 1 + length l

(* Phase 1 *)
law? first_now (p : int -> bool) (n : int) : p n ===> first p n =:= n

(* Phase 2 *)
law? first_correct (p : int -> bool) (n : int) :
  let%wp i = first p n in
  n <= i && p i && all_in_range n i (fun j -> not (p j))

law? first_premise (p : int -> bool) (n : int) (m : int) :
  n =:= m ===> p n ===> let%wp i = first p m in i =:= n && p i

law? first_twice (p : int -> bool) (n : int) :
  let%wp i = first p n in let%wp j = first p i in i =:= j

(* Phase 3: modes on parameters and results *)
law? local_list (xs : int list @ local) (f : int -> int) :
  f 0 = f 0 ===>
  length xs >= 0 ===> let%wp () = () in length xs =:= length xs

law? shared_result (p : (int -> bool) [@total]) (n : int @ shared) :
  let%wp i @ local portable = first p n in p i
