(* TEST
 flags = "-no-alias-deps";
 expect;
*)

module A = Missing_alias_target
module B = A
[%%expect{|
Line 1, characters 11-31:
1 | module A = Missing_alias_target
               ^^^^^^^^^^^^^^^^^^^^
Warning 49 [no-cmi-file]: no cmi file was found
  in path for module "Missing_alias_target"

module A = Missing_alias_target
module B = A
|}]

open B
[%%expect{|
Line 1, characters 5-6:
1 | open B
         ^
Error: The module "B" is an alias for module "Missing_alias_target", which is missing
|}]
