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

external is_int : 'a -> bool = "%obj_is_int"

type ('a : any) any_pair = 'a * int
type ('b : value) value_pair = 'b * int

let pair_is_int_any (x : _ any_pair) = is_int x
[%%expect_asm X86_64{|
pair_is_int_any:
  movl  $1, %eax
  ret
|}]

let pair_is_int_value (x : _ value_pair) = is_int x
[%%expect_asm X86_64{|
pair_is_int_value:
  movl  $1, %eax
  ret
|}]

type ('a : any) any_nested = ('a * int) * string
type ('b : value) value_nested = ('b * int) * string

let nested_is_int_any (x : _ any_nested) =
  let (y, _) = x in
  is_int y
[%%expect_asm X86_64{|
nested_is_int_any:
  movl  $1, %eax
  ret
|}]

let nested_is_int_value (x : _ value_nested) =
  let (y, _) = x in
  is_int y
[%%expect_asm X86_64{|
nested_is_int_value:
  movl  $1, %eax
  ret
|}]
