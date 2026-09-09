(* TEST
   modules = "specialise_lifted_functor_helper_lib.ml specialise_lifted_functor_body_lib.ml specialise_lifted_functor_chain_mid.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   setup-ocamlopt.opt-build-env;
   module = "specialise_lifted_functor_helper_lib.ml";
   ocamlopt.opt;
   module = "specialise_lifted_functor_body_lib.ml";
   ocamlopt.opt;
   module = "specialise_lifted_functor_chain_mid.ml";
   ocamlopt.opt;
   module = "";
   all_modules = "specialise_lifted_functor_chain.ml";
   binary_modules = "specialise_lifted_functor_helper_lib specialise_lifted_functor_body_lib specialise_lifted_functor_chain_mid";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

module Helper = Specialise_lifted_functor_chain_mid.Helper (struct
  let f x = x * x
end)

module Body = Specialise_lifted_functor_chain_mid.Body (struct
  let f x = x * x
end)

module Wrapped = Specialise_lifted_functor_chain_mid.Wrapped (struct
  let f x = x * x
end)

let[@zero_alloc] fourth_power x = Helper.apply_twice x
let[@zero_alloc] wrapped_fourth_power x = Wrapped.apply_twice x

let[@zero_alloc] sum_squares (l @ local) =
  let squares = Body.map l in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () =
  Printf.printf "%d %d; %d %d\n" (fourth_power 0) (fourth_power 3)
    (wrapped_fourth_power 0) (wrapped_fourth_power 3);
  Printf.printf "%d %d\n" (sum_squares []) (sum_squares [1; 2; 3; 4])
