(* TEST
  modules = "alias_lib.ml";
  include eval;
  flags = "-extension runtime_metaprogramming -no-alias-deps";
  native;
*)

#syntax quotations on

(* [alias_lib] declares [module Aliased = Alias_target] but no
   [alias_target.cmi] exists. The alias is recorded in [alias_lib.cmi]'s
   cmi_crcs as an alias-only entry. *)

let () =
  Printf.printf "Test alias to missing cmi\n";
  let result : int = Eval.eval <[ Alias_lib.useful_value + 1 ]> in
  Printf.printf "Output: %d\n" result;
;;
