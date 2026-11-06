(* TEST
 flags = "-extension laws";
 setup-ocamlc.byte-build-env;
 module = "gen_wp.ml";
 ocamlc.byte;
 flags = "-generate-laws-implementation -o gen_wp_laws.ml";
 module = "gen_wp.cmo";
 ocamlc.byte;
 output = "${test_build_directory}/gen_wp_laws.ml";
 reference = "${test_source_directory}/gen_wp_laws.ml.reference";
 check-program-output;
 flags = "-extension laws";
 module = "gen_wp_laws.ml";
 ocamlc.byte;
*)

(* The laws file tests [=:=] with polymorphic equality, and skips the laws
   that it cannot test: [let%wp], and [=:=] on functions. *)

let rev = List.rev
let rec first p n = if p n then n else first p (n + 1)

law? rev_rev (xs : int list) : rev (rev xs) =:= xs
law? both (xs : int list) : rev [] =:= [] && rev (rev xs) =:= xs
law? local_param (xs : int list @ local) : xs =:= xs
law? first_correct (p : int -> bool) (n : int) :
  let%wp i = first p n in n <= i
law? functions (f : int -> int) : (fun x -> f x) =:= f
