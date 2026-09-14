(* TEST
   modules = "specialise_lifted_functor_cost_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   flags = "-O3 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   module = "specialise_lifted_functor_cost_lib.ml";
   ocamlopt.opt;
   flags = "-O3 -no-flambda2-reaper -dflambda-invariants";
   flags += " -zero-alloc-check all";
   all_modules = "specialise_lifted_functor_cost.ml";
   binary_modules = "specialise_lifted_functor_cost_lib";
   module = "";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

(* A functor body containing sites must inline speculatively just as it would
   with ordinary closed sets of closures. *)

module Interval = Specialise_lifted_functor_cost_lib.Make (struct
  type t = float
  let ( < ) (x : float) y = x < y
  let ( > ) (x : float) y = x > y
end)

let[@zero_alloc] [@inline never] check bounds raw =
  Interval.contains bounds raw

let () =
  let bounds = Specialise_lifted_functor_cost_lib.Interval (0., 1.) in
  assert (check bounds 0.5);
  assert (not (check bounds 2.));
  assert (not (check Specialise_lifted_functor_cost_lib.Empty 0.5))
