(* TEST
 readonly_files = "gen_kinded_inputs.ml";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_kinded.ml";
 ocamlopt.opt;
 flags = "-generate-laws-implementation -o gen_kinded_laws.ml";
 module = "gen_kinded.cmx";
 ocamlopt.opt;
 output = "${test_build_directory}/gen_kinded_laws.ml";
 reference = "${test_source_directory}/gen_kinded_laws.ml.reference";
 check-program-output;
 flags = "-extension laws";
 module = "gen_kinded_laws.ml gen_kinded_inputs.ml";
 ocamlopt.opt;
*)

(* The input types of laws quantify their type variables with their kinds,
   so that gen_kinded_inputs.ml constructs the inputs the laws admit,
   nullable ones included. *)

let f (_ : ('a : immutable_data)) = true
law? kinded (x : ('a : immutable_data)) : f x
(* The kind may come from the clauses; it holds for every occurrence. *)
law? narrowed (x : 'a) (y : 'a) : f x && f y
law? nullable (type (a : value_or_null)) (x : a) : 1 = 1
law? plain (x : 'a) : 1 = 1
(* The clauses refer to the types of a law, which matches can refine. *)
type _ ty = Int : int ty | Bool : bool ty
law? refined (type a) (w : a ty) (x : a) :
  (match w with Int -> x >= 0 | Bool -> x || true) && x == x
