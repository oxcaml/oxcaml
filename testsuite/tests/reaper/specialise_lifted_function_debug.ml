(* TEST
   modules = "specialise_lifted_function_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   flags = "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   module = "specialise_lifted_function_lib.ml";
   ocamlopt.opt;
   flags = "-O3 -no-flambda2-reaper -g -flambda2-expert-phantom-lets -dflambda-invariants";
   module = "";
   all_modules = "specialise_lifted_function_debug.ml";
   binary_modules = "specialise_lifted_function_lib";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

[@@@ocaml.flambda_o3]

(* Imported specialisation sites under phantom lets, in a consumer that does
   not run the reaper. *)

let[@zero_alloc] sum_squares (l @ local) =
  let squares = Specialise_lifted_function_lib.map_stack (fun x -> x * x) l in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3; 4])
