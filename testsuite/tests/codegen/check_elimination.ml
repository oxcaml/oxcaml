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
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L2:
  movl  $1, %eax
  ret
|}]

(* CR ttebbi: We check [target < x] twice, even though the second branch is
   redundant. *)
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
  movq  %rbx, %rax
  testb $1, %al
  je    .L2
  jmp   .L1
.L0:
  testb $1, %al
  je    .L2
.L1:
  movl  $1, %eax
  jmp   .L4
.L2:
  movq  (%rax), %rbx
  cmpq  %rbx, %rdi
  jge   .L3
  movq  8(%rax), %rax
  cmpq  %rbx, %rdi
  jge   .L4
  jmp   .L0
.L3:
  cmpq  %rbx, %rdi
  jl    .L0
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


(* CR ttebbi: We repeat the same branch twice. *)
let complex_branching_on_two_comparisons (x: int) (y: int) c1 c2 c3 =
 match x = 2, y = 2 with
 | true, true -> c1 ()
 | _, false -> c2 ()
 | false, _ -> c3 ()
[%%expect_asm X86_64{|
complex_branching_on_two_comparisons:
  movq  %rbx, %rcx
  movq  %rsi, %rbx
  cmpq  $5, %rax
  jne   .L0
  cmpq  $5, %rcx
  jne   .L0
  movl  $1, %eax
  movq  (%rdi), %rsi
  movq  %rdi, %rbx
  jmp   *%rsi
.L0:
  cmpq  $5, %rcx
  jne   .L1
  movl  $1, %eax
  movq  (%rdx), %rdi
  movq  %rdx, %rbx
  jmp   *%rdi
.L1:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

let fold_float_compare_into_branch (z : int) (x : float) (y : float) f g =
  let b = (z = 0) && x < y in
  if b then f () else g ()
[%%expect_asm X86_64{|
fold_float_compare_into_branch:
  movq  %rbx, %rcx
  movq  %rdx, %rbx
  cmpq  $1, %rax
  jne   .L0
  vmovsd (%rdi), %xmm0
  vmovsd (%rcx), %xmm1
  vcomisd %xmm1, %xmm0
  jbe   .L0
  movl  $1, %eax
  movq  (%rsi), %rdi
  movq  %rsi, %rbx
  jmp   *%rdi
.L0:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

(* Jump threading resolves the [Switch] to a direct jump for a predecessor
   where the switched-on value is a constant. *)
let switch_on_merged_constant x (t : unit -> int) f g =
  let i = if x = 3 then 4 else x in
  match i with
  | 0 | 5 | 3 -> f ()
  | _ -> g ()
[%%expect_asm X86_64{|
switch_on_merged_constant:
  movq  %rdi, %rbx
  cmpq  $7, %rax
  je    .L0
  movq  %rax, %rdi
  sarq  $1, %rdi
  cmpq  $11, %rax
  ja    .L0
  leaq  <hidden PC-relative offset>(%rip), %rax
  movslq (%rax,%rdi,4), %rdx
  addq  %rdx, %rax
  jmp   *%rax
.L0:
  movl  $1, %eax
  movq  (%rsi), %rdi
  movq  %rsi, %rbx
  jmp   *%rdi
.L1:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

(* Jump threading folds the unsigned range check of the pattern match for both
   predecessors of the merge block. *)
let range_check_on_merged_constant b f g =
  let i = if b then 2 else 5 in
  match i with
  | 0 | 1 | 2 -> f ()
  | _ -> g ()
[%%expect_asm X86_64{|
range_check_on_merged_constant:
  cmpq  $1, %rax
  jne   .L0
  movl  $1, %eax
  movq  (%rdi), %rsi
  movq  %rdi, %rbx
  jmp   *%rsi
.L0:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

(* Jump threading folds the parity test of [Obj.is_int] for the predecessor
   that binds an immediate; the other predecessor binds a symbol, which is not
   folded. *)
let parity_test_on_merged_constant b f g =
  let x = if b then Obj.repr 1 else Obj.repr "foo" in
  if Obj.is_int x then f () else g ()
[%%expect_asm X86_64{|
parity_test_on_merged_constant:
  movq  %rbx, %rsi
  movq  %rdi, %rbx
  cmpq  $1, %rax
  jne   .L0
  movq  <hidden PC-relative offset>(%rip), %rax
  testb $1, %al
  je    .L1
.L0:
  movl  $1, %eax
  movq  (%rsi), %rdi
  movq  %rsi, %rbx
  jmp   *%rdi
.L1:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

let fold_two_float_compares_into_branch (x : float) (y : float) f g =
  let b = x < 0.5 || y < 0.5 in
  if b then f () else g ()
[%%expect_asm X86_64{|
fold_two_float_compares_into_branch:
  movq  %rbx, %rdx
  movq  %rsi, %rbx
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  vmovsd (%rax), %xmm1
  vcomisd %xmm1, %xmm0
  ja    .L0
  vmovsd (%rdx), %xmm1
  vcomisd %xmm1, %xmm0
  jbe   .L1
.L0:
  movl  $1, %eax
  movq  (%rdi), %rsi
  movq  %rdi, %rbx
  jmp   *%rsi
.L1:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

(* CR ttebbi: The float comparison is not folded, even though both
   predecessors forward a constant into it: on the [nan] predecessor [x < 1.0]
   is unordered and hence statically false, while on the [0.5] predecessor it
   is statically true. Jump threading only tracks integer constants. *)
let float_compare_on_merged_constant b f g =
  let x = if b then Float.nan else 0.5 in
  if x < 1.0 then f () else g ()
[%%expect_asm X86_64{|
float_compare_on_merged_constant:
  movq  %rbx, %rsi
  movq  %rdi, %rbx
  cmpq  $1, %rax
  jne   .L0
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  jmp   .L1
.L0:
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
.L1:
  vmovsd <hidden PC-relative offset>(%rip), %xmm1
  vcomisd %xmm0, %xmm1
  jbe   .L2
  movl  $1, %eax
  movq  (%rsi), %rdi
  movq  %rsi, %rbx
  jmp   *%rdi
.L2:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

(* [Float.is_nan x] is [x <> x], whose folded [Float_test] has to send the
   unordered case to the [true] arm. *)
let fold_is_nan_into_branch (z : int) (x : float) f g =
  let b = z = 0 && Float.is_nan x in
  if b then f () else g ()
[%%expect_asm X86_64{|
fold_is_nan_into_branch:
  movq  %rbx, %rdx
  movq  %rsi, %rbx
  cmpq  $1, %rax
  jne   .L0
  vmovsd (%rdx), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L0
  movl  $1, %eax
  movq  (%rdi), %rsi
  movq  %rdi, %rbx
  jmp   *%rsi
.L0:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]
