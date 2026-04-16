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
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  orq   $1, %rdi
  leaq  -2(%rdi), %rdx
  cmpq  $1, %rdx
  jl    .L137
  sarq  $1, %rdx
  movl  $1, %esi
  xorl  %ebx, %ebx
.L114:
  leaq  1(%rbx,%rbx), %rcx
  cmpq  %rdi, %rcx
  jae   .L133
  movq  -4(%rax,%rcx,4), %rcx
  leaq  -1(%rsi,%rcx), %rsi
  incq  %rbx
  cmpq  %rdx, %rbx
  jle   .L114
  movq  %rsi, %rax
  ret
.L133:
  movq  camlTOP2__block101@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L137:
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
  je    .L116
  jmp   .L114
.L112:
  testb $1, %bl
  je    .L116
.L114:
  xorl  %eax, %eax
  jmp   .L131
.L116:
  movq  (%rbx), %rax
  cmpq  %rax, %rdi
  setl  %al
  movzbq %al, %rax
  testq %rax, %rax
  je    .L127
  movq  8(%rbx), %rbx
  testq %rax, %rax
  jne   .L112
  jmp   .L131
.L127:
  testq %rax, %rax
  jne   .L112
.L131:
  movq  %rbx, %rax
  ret
|}]

(* CR ttebbi: The second branch is always true. *)
let redundant_compare (x: int) = if x > 0 && x > 5 then 100 else 200
[%%expect_asm X86_64{|
redundant_compare:
  cmpq  $1, %rax
  jle   .L110
  cmpq  $11, %rax
  jle   .L110
  movl  $201, %eax
  ret
.L110:
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
  je    .L105
  movl  $201, %eax
  ret
.L105:
  leaq  -1(%rax,%rax), %rax
  ret
|}]
