(* TEST
 flags += " -O3 -extension layouts_beta";
 only-default-codegen;
 expect.opt;
*)

let rebuild_mixed (x, #(y, z)) = (x, #(y, z))
[%%expect_asm X86_64{|
rebuild_mixed:
  ret
|}]


let rebuild_mixed_annotated (x, (#(y, z) : (_ : float64 & (value & void)))) =
  (x, #(y, z))
[%%expect_asm X86_64{|
rebuild_mixed_annotated:
  ret
|}]

type ('a : any) t = 'a * int * bool#

let rebuild_any ((x, y, z) : int t) = (x, y, z)
[%%expect_asm X86_64{|
rebuild_any:
  ret
|}]
