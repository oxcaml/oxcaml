(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)


(* CR ttebbi: The second check is is duplicated but not folded away. *)
let unwrap_twice o = Option.value ~default:7 o + Option.value ~default:7 o
[%%expect_asm X86_64{|
unwrap_twice:
  testb $1, %al
  je    .L108
  movl  $15, %ebx
  testb $1, %al
  je    .L119
  jmp   .L117
.L108:
  movq  (%rax), %rbx
  testb $1, %al
  je    .L119
.L117:
  movl  $15, %eax
  jmp   .L123
.L119:
  movq  (%rax), %rax
.L123:
  leaq  -1(%rax,%rbx), %rax
  ret
|}]


(* CR ttebbi: Array bounds checks are not eliminated. *)
let arr_sum arr =
  let sum = ref 0 in
  for i = 0 to Array.length arr - 1 do
    sum := !sum + arr.(i)
  done;
  !sum
;;
[%%expect_asm X86_64{|
arr_sum:
  movq  %rax, %rbx
  movq  -8(%rbx), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  orq   $1, %rdi
  movq  %rdi, %rsi
  addq  $-2, %rsi
  cmpq  $1, %rsi
  jl    .L135
  movl  $1, %eax
  movl  $1, %edx
  cmpq  %rdi, %rdx
  jae   .L131
.L118:
  movq  -4(%rbx,%rdx,4), %rcx
  leaq  -1(%rax,%rcx), %rax
  cmpq  %rsi, %rdx
  je    .L125
  addq  $2, %rdx
  cmpq  %rdi, %rdx
  jae   .L131
  jmp   .L118
.L125:
  ret
.L131:
  movq  camlTOP2__block95@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L135:
  movl  $1, %eax
  ret
|}]
