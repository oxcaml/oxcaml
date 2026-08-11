(* TEST
 modules = "layout_any_return_cross_unit_dep.ml";
 flags = "-extension layouts_beta";
 flambda2;
 {
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   native;
 }{
   ocamlopt_flags = "-O3";
   compiler_directory_suffix = ".O3";
   native;
 }{
   native;
 }
*)

(* Never-returning functions are compiled with a bottom result arity, which
   is stored in their unit's cmx.  Calling them from another unit exercises
   the cmx round-trip of non-concrete result arities. *)

let () =
  match Layout_any_return_cross_unit_dep.f () with
  | n -> Printf.printf "returned: %d\n" n
  | exception Assert_failure _ -> print_string "raised: int\n"

let () =
  match Layout_any_return_cross_unit_dep.g () with
  | _ -> print_string "returned: float64\n"
  | exception Assert_failure _ -> print_string "raised: float64\n"
