(* TEST
   modules = "specialise_lifted_function_lib.ml specialise_lifted_function_chain_mid.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   compiler_output2 = "producer.cmm";
   flags = "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants -dcmm";
   module = "specialise_lifted_function_lib.ml";
   ocamlopt.opt;
   script = "sh ${test_source_directory}/check-no-specialisation-site-data.sh producer.cmm";
   script;
   compiler_output2 = "compiler.output";
   flags = "-O3 -no-flambda2-reaper -dflambda-invariants";
   module = "specialise_lifted_function_chain_mid.ml";
   ocamlopt.opt;
   module = "";
   all_modules = "specialise_lifted_function_o3.ml";
   binary_modules = "specialise_lifted_function_lib specialise_lifted_function_chain_mid";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

[@@@ocaml.flambda_o3]

(* Only the producer runs the reaper. Its specialisation site must survive
   the -O3 intermediate unit and specialise the loop here. *)

let[@zero_alloc] sum_squares (l @ local) =
  let squares =
    Specialise_lifted_function_chain_mid.my_map (fun x -> x * x) l
  in
  let rec total acc (l @ local) =
    match l with
    | [] -> acc
    | x :: xs -> total (acc + x) xs
  in
  total 0 squares [@nontail]

let () = Printf.printf "%d\n" (sum_squares [1; 2; 3; 4])
