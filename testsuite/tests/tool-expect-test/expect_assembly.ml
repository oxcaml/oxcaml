(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)

let unused_function () = 0;;

let helper2 x = if x > 3 then x + 7 else x * 7
[%%expect{|
val unused_function : unit -> int = <fun>
val helper2 : int -> int = <fun>
|}]
[%%expect_asm X86_64{|
helper2:
  cmpq  $7, %rax
  jle   .L0
  addq  $14, %rax
  ret
.L0:
  imulq $7, %rax
  addq  $-6, %rax
  ret
|}]

let helper x = if x > 3 then x + 7 else x * 7
[%%expect_asm X86_64{|
helper:
  cmpq  $7, %rax
  jle   .L0
  addq  $14, %rax
  ret
.L0:
  imulq $7, %rax
  addq  $-6, %rax
  ret
|}]
[%%expect{|
val helper : int -> int = <fun>
|}]


(* CR ttebbi: We should try to normalize mangled label names, at least remove the
    camlTOP part. *)
let f l = List.map (fun x -> helper (x + 11)) l |> List.map (fun x -> helper (x + 12));;
[%%expect_asm X86_64{|
f.(fun):
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  16(%rbx), %rbx
  addq  $22, %rax
  movq  (%rbx), %rdi
  jmp   *%rdi

f:
  subq  $8, %rsp
  movq  %rax, %rbx
  movq  <hidden PC-relative offset>(%rip), %rax
  call  camlStdlib__List__map_16_124_code@PLT
.L0:
  movq  %rax, %rbx
  movq  <hidden PC-relative offset>(%rip), %rax
  addq  $8, %rsp
  jmp   camlStdlib__List__map_16_124_code@PLT

f.(fun):
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  16(%rbx), %rbx
  addq  $24, %rax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]
