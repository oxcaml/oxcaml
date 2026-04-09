(* TEST
 flags += " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt;
*)


(* CR ttebbi: The second check is is duplicated but not folded away. *)
let unwrap_twice o = Option.value ~default:7 o + Option.value ~default:7 o
[%%expect_asm X86_64{|
unwrap_twice:
  testb $1, %al
  je    .L104
  movl  $15, %ebx
  testb $1, %al
  je    .L109
  jmp   .L108
.L104:
  movq  (%rax), %rbx
  testb $1, %al
  je    .L109
.L108:
  movl  $15, %eax
  jmp   .L106
.L109:
  movq  (%rax), %rax
.L106:
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
  movq  %rax, %rdi
  movq  -8(%rdi), %rbx
  salq  $8, %rbx
  shrq  $17, %rbx
  orq   $1, %rbx
  leaq  -2(%rbx), %rsi
  cmpq  $1, %rsi
  jl    .L107
  subq  $8, %rsp
  sarq  $1, %rsi
  movl  $1, %eax
  xorl  %edx, %edx
.L109:
  leaq  1(%rdx,%rdx), %rcx
  cmpq  %rbx, %rcx
  jae   .L112
  movq  -4(%rdi,%rcx,4), %rcx
  leaq  -1(%rax,%rcx), %rax
  incq  %rdx
  cmpq  %rsi, %rdx
  jle   .L109
  addq  $8, %rsp
  ret
.L112:
  movq  camlTOP2__block101@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L107:
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
  subq  $8, %rsp
  movq  %rax, %rdi
  testb $1, %bl
  je    .L108
.L107:
  xorl  %esi, %esi
  movq  %rbx, %rax
  jmp   .L115
.L108:
  movq  (%rbx), %rax
  cmpq  %rax, %rdi
  setl  %al
  movzbq %al, %rsi
  testq %rsi, %rsi
  je    .L112
  movq  8(%rbx), %rax
  testq %rsi, %rsi
  jne   .L114
  jmp   .L115
.L112:
  movq  %rbx, %rax
  testq %rsi, %rsi
  je    .L115
.L114:
  movq  %rax, %rbx
  testb $1, %bl
  je    .L108
  jmp   .L107
.L115:
  addq  $8, %rsp
  ret
|}]

(* CR ttebbi: The second branch is always true. *)
let redundant_compare (x: int) = if x > 0 && x > 5 then 100 else 200
[%%expect_asm X86_64{|
redundant_compare:
  cmpq  $1, %rax
  jle   .L104
  cmpq  $11, %rax
  jle   .L104
  movl  $201, %eax
  ret
.L104:
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
  je    .L102
  movl  $201, %eax
  ret
.L102:
  leaq  -1(%rax,%rax), %rax
  ret
|}]
