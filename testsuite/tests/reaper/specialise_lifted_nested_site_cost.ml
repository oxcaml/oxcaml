(* TEST
   modules = "specialise_lifted_nested_site_cost_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   flags = "-O3 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   module = "specialise_lifted_nested_site_cost_lib.ml";
   ocamlopt.opt;
   flags = "-O3 -no-flambda2-reaper -dflambda-invariants";
   flags += " -zero-alloc-check all";
   all_modules = "specialise_lifted_nested_site_cost.ml";
   binary_modules = "specialise_lifted_nested_site_cost_lib";
   module = "";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

(* Sites inside [fold] must contribute to its cost, not the functor's. *)

module M = Specialise_lifted_nested_site_cost_lib.Make (struct
  type t = int
  let of_int x = x land 0xffff
  let to_int x = x
end)

let[@zero_alloc] [@inline never] fold xs init =
  M.fold xs ~init ~f:(fun x acc -> x + acc)

let[@zero_alloc] [@inline never] fold2 xs init =
  M.fold2 xs ~init ~f:(fun x acc -> x + acc)

let () =
  assert (fold [] 7 = 7);
  assert (fold2 [] 9 = 9);
  assert (fold [1] 0 = 4);
  assert (fold2 [1] 0 = 11)
