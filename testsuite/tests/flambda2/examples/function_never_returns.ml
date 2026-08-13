(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-flambda2-result-types-all-functions";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* compiled with -flambda2-result-types-all-functions this used to trigger a
   compilation error related to [Never_returns] functions and return arity. *)

let[@inline never] f () : unit = raise Exit

let foo x =
  let () = f () in
  x + 1

let bar x = (foo [@inlined]) x
