(* TEST
 flags += " -O3 -extension layouts_beta";
 only-default-codegen;
 flat-float-array;
 expect.opt;
*)

type ('a : any) t = { field : 'a }

external box_float : float# -> float = "%box_float"

let rebuild_int (src : int t) : int t = { field = src.field }
[%%expect_asm X86_64{|
rebuild_int:
  ret
|}]

let rebuild_float (src : float# t) : float# t = { field = src.field }
[%%expect_asm X86_64{|
rebuild_float:
  ret
|}]

(* Keep the array access polymorphic until inlining. *)
let first (r : int array t) =
  let[@inline always] first (a : _ array) = Array.unsafe_get a 0 in
  first r.field
[%%expect_asm X86_64{|
first:
  movq  (%rax), %rax
  movq  (%rax), %rax
  ret
|}]

let () =
  assert ((rebuild_int { field = 42 }).field = 42);
  assert (box_float (rebuild_float { field = #42.0 }).field = 42.0);
  assert (first { field = [| 42 |] } = 42)
[%%expect{|
|}]
