(* TEST
flags += " -O3";
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
  jle   .L106
  addq  $14, %rax
  ret
  .align 4
.L106:
  imulq $7, %rax
  addq  $-6, %rax
  ret
|}]

let helper x = if x > 3 then x + 7 else x * 7
[%%expect_asm X86_64{|
helper:
  cmpq  $7, %rax
  jle   .L106
  addq  $14, %rax
  ret
  .align 4
.L106:
  imulq $7, %rax
  addq  $-6, %rax
  ret
|}]
[%%expect{|
val helper : int -> int = <fun>
|}]

let f l = List.map (fun x -> helper (x + 11)) l |> List.map (fun x -> helper (x + 12));;

[%%expect_asm X86_64{|
f.(fun):
  movq  camlTOP4__fn$5b$3a1$2c19$2d$2d45$5d_10@GOTPCREL(%rip), %rbx
  movq  16(%rbx), %rbx
  addq  $22, %rax
  movq  (%rbx), %rdi
  jmp   *%rdi

f:
  subq  $8, %rsp
  .cfi_adjust_cfa_offset 8
  movq  %rax, %rbx
  movq  camlTOP4__fn$5b$3a1$2c19$2d$2d45$5d_10@GOTPCREL(%rip), %rax
  call  camlStdlib__List__map_15_113_code@PLT
.L113:
  movq  %rax, %rbx
  movq  camlTOP4__fn$5b$3a1$2c60$2d$2d86$5d_11@GOTPCREL(%rip), %rax
  addq  $8, %rsp
  .cfi_adjust_cfa_offset -8
  jmp   camlStdlib__List__map_15_113_code@PLT
  .cfi_adjust_cfa_offset 8

f.(fun):
  movq  camlTOP4__fn$5b$3a1$2c60$2d$2d86$5d_11@GOTPCREL(%rip), %rbx
  movq  16(%rbx), %rbx
  addq  $24, %rax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]
