(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)


(* CR ttebbi: The second check is is duplicated but not folded away. *)
let unwrap_twice o = Option.value ~default:7 o + Option.value ~default:7 o
[%%expect_asm X86_64{|
unwrap_twice:
  testb $1, %al
  je    .L0
  movl  $15, %ebx
  testb $1, %al
  je    .L2
  jmp   .L1
.L0:
  movq  (%rax), %rbx
  testb $1, %al
  je    .L2
.L1:
  movl  $15, %eax
  jmp   .L3
.L2:
  movq  (%rax), %rax
.L3:
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
  leaq  -2(%rdi), %rsi
  cmpq  $1, %rsi
  jl    .L2
  sarq  $1, %rsi
  movl  $1, %eax
  xorl  %edx, %edx
.L0:
  leaq  1(%rdx,%rdx), %rcx
  cmpq  %rdi, %rcx
  jae   .L1
  movq  -4(%rbx,%rcx,4), %rcx
  leaq  -1(%rax,%rcx), %rax
  incq  %rdx
  cmpq  %rsi, %rdx
  jle   .L0
  ret
.L1:
  movq  camlTOP2__block101@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L2:
  movl  $1, %eax
  ret
|}]

(* CR ttebbi: The generated control flow branches three (!) times on
   should_continue. In block 123, we can even statically know that the bit is 1.
   Additionally, we materialise the should_continue bit. *)
let search ~target (start : int list) =
  let node = ref start in
  while
    match !node with
    | [] -> false
    | x :: xs ->
      let should_continue = target < x in
      if should_continue then node := xs;
      should_continue
  do () done;
  !node
;;
[%%expect_asm X86_64{|
search:
  movq  %rax, %rdi
  testb $1, %bl
  je    .L1
.L0:
  xorl  %esi, %esi
  movq  %rbx, %rax
  jmp   .L4
.L1:
  movq  (%rbx), %rax
  xorl  %esi, %esi
  cmpq  %rax, %rdi
  setl  %sil
  testq %rsi, %rsi
  je    .L2
  movq  8(%rbx), %rax
  testq %rsi, %rsi
  jne   .L3
  jmp   .L4
.L2:
  movq  %rbx, %rax
  testq %rsi, %rsi
  je    .L4
.L3:
  movq  %rax, %rbx
  testb $1, %bl
  je    .L1
  jmp   .L0
.L4:
  ret
|}]

(* CR ttebbi: The second branch is always true. *)
let redundant_compare (x: int) = if x > 0 && x > 5 then 100 else 200
[%%expect_asm X86_64{|
redundant_compare:
  cmpq  $1, %rax
  jle   .L0
  cmpq  $11, %rax
  jle   .L0
  movl  $201, %eax
  ret
.L0:
  movl  $401, %eax
  ret
|}]

(* CR ttebbi: We don't learn that x is 3 in the first case. *)
let learn_from_branch (x : int) : int =
  match x with
  | 3 -> x * 2
  | _ -> 100
[%%expect_asm X86_64{|
learn_from_branch:
  cmpq  $7, %rax
  je    .L0
  movl  $201, %eax
  ret
.L0:
  leaq  -1(%rax,%rax), %rax
  ret
|}]
