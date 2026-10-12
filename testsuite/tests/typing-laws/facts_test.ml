(* TEST
 readonly_files = "fact_a.ml fact_b.ml";
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "fact_a.ml fact_b.ml facts_test.ml";
 ocamlopt.opt;
*)

(* The inferred interface of [Fact_b], a unit without interface including
   [Fact_a], records that its values and exceptions are those of [Fact_a]:
   the laws of a consumer may refer to either. *)

module type L = sig
  law? value : Fact_a.x = 0
  law? exception_ : Fact_a.E = Fact_a.E
end

module Through_b : L = struct
  law? value : Fact_b.x = 0
  law? exception_ : Fact_b.E = Fact_a.E
end
