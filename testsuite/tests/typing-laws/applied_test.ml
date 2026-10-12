(* TEST
 readonly_files = "applied_provider.ml";
 setup-ocamlopt.opt-build-env;
 module = "applied_provider.ml";
 ocamlopt.opt;
 flags = "-extension laws";
 module = "applied_test.ml";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* The inferred interface of [Applied_provider], a unit compiled without
   the extension, records that the values of [P] and [Q] are those of
   [Bound], and nothing about those of [M] and [N], the results of
   applying [Id] to an application. *)

open Applied_provider

module Same_instance : sig
  law? p : P.x == Q.x
  law? e : P.E = Q.E
end = struct
  law? p : Bound.x == Bound.x
  law? e : Bound.E = Bound.E
end

module Different_instances : sig
  law? p : M.x == N.x
end = struct
  law? p : M.x == M.x
end
