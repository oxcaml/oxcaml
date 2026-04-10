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
<<<<<<< HEAD
  je    .L0
||||||| parent of 42782c097b (passes testsuite)
  je    .L108
=======
  je    .L104
>>>>>>> 42782c097b (passes testsuite)
  movl  $15, %ebx
  testb $1, %al
<<<<<<< HEAD
  je    .L2
  jmp   .L1
.L0:
||||||| parent of 42782c097b (passes testsuite)
  je    .L119
  jmp   .L117
.L108:
=======
  je    .L109
  jmp   .L108
.L104:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rax), %rbx
  testb $1, %al
<<<<<<< HEAD
  je    .L2
.L1:
||||||| parent of 42782c097b (passes testsuite)
  je    .L119
.L117:
=======
  je    .L109
.L108:
>>>>>>> 42782c097b (passes testsuite)
  movl  $15, %eax
<<<<<<< HEAD
  jmp   .L3
.L2:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L123
.L119:
=======
  jmp   .L106
.L109:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rax), %rax
<<<<<<< HEAD
.L3:
||||||| parent of 42782c097b (passes testsuite)
.L123:
=======
.L106:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  jl    .L2
||||||| parent of 42782c097b (passes testsuite)
  jl    .L137
=======
  jl    .L107
<<<<<<< HEAD
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  subq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
  sarq  $1, %rsi
  movl  $1, %eax
  xorl  %edx, %edx
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L114:
=======
.L109:
>>>>>>> 42782c097b (passes testsuite)
  leaq  1(%rdx,%rdx), %rcx
<<<<<<< HEAD
  cmpq  %rdi, %rcx
  jae   .L1
  movq  -4(%rbx,%rcx,4), %rcx
||||||| parent of 42782c097b (passes testsuite)
  cmpq  %rdi, %rcx
  jae   .L133
  movq  -4(%rbx,%rcx,4), %rcx
=======
  cmpq  %rbx, %rcx
  jae   .L112
  movq  -4(%rdi,%rcx,4), %rcx
>>>>>>> 42782c097b (passes testsuite)
  leaq  -1(%rax,%rcx), %rax
  incq  %rdx
  cmpq  %rsi, %rdx
<<<<<<< HEAD
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  jle   .L114
=======
  jle   .L109
<<<<<<< HEAD
  addq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  addq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L133:
=======
.L112:
>>>>>>> 42782c097b (passes testsuite)
  movq  camlTOP2__block101@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
<<<<<<< HEAD
.L2:
||||||| parent of 42782c097b (passes testsuite)
.L137:
=======
.L107:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  je    .L1
.L0:
||||||| parent of 42782c097b (passes testsuite)
  je    .L116
.L114:
=======
  je    .L108
.L107:
>>>>>>> 42782c097b (passes testsuite)
  xorl  %esi, %esi
  movq  %rbx, %rax
<<<<<<< HEAD
  jmp   .L4
.L1:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L131
.L116:
=======
  jmp   .L115
.L108:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rbx), %rax
  xorl  %esi, %esi
  cmpq  %rax, %rdi
  setl  %sil
  testq %rsi, %rsi
<<<<<<< HEAD
  je    .L2
||||||| parent of 42782c097b (passes testsuite)
  je    .L123
=======
  je    .L112
>>>>>>> 42782c097b (passes testsuite)
  movq  8(%rbx), %rax
  testq %rsi, %rsi
<<<<<<< HEAD
  jne   .L3
  jmp   .L4
.L2:
||||||| parent of 42782c097b (passes testsuite)
  jne   .L129
  jmp   .L131
.L123:
=======
  jne   .L114
  jmp   .L115
.L112:
>>>>>>> 42782c097b (passes testsuite)
  movq  %rbx, %rax
  testq %rsi, %rsi
<<<<<<< HEAD
  je    .L4
.L3:
||||||| parent of 42782c097b (passes testsuite)
  je    .L131
.L129:
=======
  je    .L115
.L114:
>>>>>>> 42782c097b (passes testsuite)
  movq  %rax, %rbx
  testb $1, %bl
<<<<<<< HEAD
  je    .L1
  jmp   .L0
.L4:
||||||| parent of 42782c097b (passes testsuite)
  je    .L116
  jmp   .L114
.L131:
=======
  je    .L108
  jmp   .L107
.L115:
<<<<<<< HEAD
  addq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  addq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
  ret
|}]

(* CR ttebbi: The second branch is always true. *)
let redundant_compare (x: int) = if x > 0 && x > 5 then 100 else 200
[%%expect_asm X86_64{|
redundant_compare:
  cmpq  $1, %rax
<<<<<<< HEAD
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  jle   .L110
=======
  jle   .L104
>>>>>>> 42782c097b (passes testsuite)
  cmpq  $11, %rax
<<<<<<< HEAD
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  jle   .L110
=======
  jle   .L104
>>>>>>> 42782c097b (passes testsuite)
  movl  $201, %eax
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L110:
=======
.L104:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  je    .L0
||||||| parent of 42782c097b (passes testsuite)
  je    .L105
=======
  je    .L102
>>>>>>> 42782c097b (passes testsuite)
  movl  $201, %eax
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L105:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
  leaq  -1(%rax,%rax), %rax
  ret
|}]
