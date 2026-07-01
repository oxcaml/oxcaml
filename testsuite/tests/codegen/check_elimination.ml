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
  movq  %rax, %rbx
  testb $1, %bl
  je    .L0
  movl  $15, %eax
  testb $1, %bl
  je    .L2
  jmp   .L1
.L0:
  movq  (%rbx), %rax
  testb $1, %bl
  je    .L2
.L1:
  movl  $15, %ebx
  jmp   .L3
.L2:
  movq  (%rbx), %rbx
.L3:
  leaq  -1(%rbx,%rax), %rax
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
  movq  %rax, %rdx
  movq  -8(%rdx), %rbx
  salq  $8, %rbx
  shrq  $17, %rbx
  orq   $1, %rbx
  leaq  -2(%rbx), %rdi
  cmpq  $1, %rdi
  jl    .L3
  sarq  $1, %rdi
  movl  $1, %eax
  xorl  %esi, %esi
  movq  %rax, %rcx
.L0:
  leaq  1(%rsi,%rsi), %rax
  cmpq  %rbx, %rax
  jae   .L2
  movq  -4(%rdx,%rax,4), %rax
  leaq  -1(%rcx,%rax), %rax
  incq  %rsi
  cmpq  %rdi, %rsi
  jg    .L1
  movq  %rax, %rcx
  jmp   .L0
.L1:
  ret
.L2:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L3:
  movl  $1, %eax
  ret
|}]

(* CR ttebbi: The generated control flow branches two times on
   should_continue. Additionally, we materialise the should_continue bit. *)
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
  je    .L2
  jmp   .L1
.L0:
  testb $1, %bl
  je    .L2
.L1:
  xorl  %esi, %esi
  movl  $1, %eax
  movq  %rax, %rbx
  jmp   .L4
.L2:
  movq  (%rbx), %rax
  xorl  %esi, %esi
  cmpq  %rax, %rdi
  setl  %sil
  testq %rsi, %rsi
  je    .L3
  movq  8(%rbx), %rbx
  testq %rsi, %rsi
  jne   .L0
  jmp   .L4
.L3:
  testq %rsi, %rsi
  jne   .L0
.L4:
  movq  %rbx, %rax
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


(* CR ttebbi: We shouldn't materialize the boolean and some branches are
   imposssible to take. *)
let complex_branching_on_two_comparisons (x: int) (y: int) c1 c2 c3 =
 match x = 2, y = 2 with
 | true, true -> c1 ()
 | _, false -> c2 ()
 | false, _ -> c3 ()
[%%expect_asm X86_64{|
complex_branching_on_two_comparisons:
  movq  %rax, %rcx
  xorl  %eax, %eax
  cmpq  $5, %rbx
  sete  %al
  cmpq  $5, %rcx
  jne   .L0
  testq %rax, %rax
  je    .L0
  movl  $1, %eax
  movq  (%rdi), %rsi
  movq  %rdi, %rbx
  jmp   *%rsi
.L0:
  testq %rax, %rax
  je    .L1
  movl  $1, %eax
  movq  (%rdx), %rdi
  movq  %rdx, %rbx
  jmp   *%rdi
.L1:
  movl  $1, %eax
  movq  (%rsi), %rdi
  movq  %rsi, %rbx
  jmp   *%rdi
|}]
