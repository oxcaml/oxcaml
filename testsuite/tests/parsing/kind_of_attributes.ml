(* TEST
 flags = "-stop-after parsing -dparsetree";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

module type K = sig kind_ k end

(* Bare [kind_of_] appears here without parentheses. These three forms pin down
   where [@foo] binds:
   - to the inner [core_type] in [kind_of_ int [@foo]]
   - to the outer [module_type] in [(K with kind_ k = kind_of_ int) [@foo]]
   - to the outer [module_expr] in [((Int : K with kind_ k = kind_of_ int) [@foo])] *)

module _ = (Int : K with kind_ k = kind_of_ int [@foo])

module _ = (Int : (K with kind_ k = kind_of_ int) [@foo])

module _ = ((Int : K with kind_ k = kind_of_ int) [@foo])
