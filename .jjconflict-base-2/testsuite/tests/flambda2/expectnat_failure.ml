(* TEST
   compile_only = "true";
   flambda2;
   expect.opt with dump-simplify;
 *)

(* Makes sure frontend errors are properly rendered *)
let c = 1 + 2.

[%%expect_fexpr Simplify{|
Line 1, characters 12-14:
1 | let c = 1 + 2.
                ^^
Error: The constant "2." has type "float" but an expression was expected of type
         "int"
|}]
[%%expect_asm X86_64{|
Line 1, characters 12-14:
1 | let c = 1 + 2.
                ^^
Error: The constant "2." has type "float" but an expression was expected of type
         "int"
|}]
