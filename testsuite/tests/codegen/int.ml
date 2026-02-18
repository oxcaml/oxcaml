(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)

(* Codegen tests for int operations *)

let neg x = -x
[%%expect_asm X86_64{|
neg:
  movq  %rax, %rbx
  movl  $2, %eax
  subq  %rbx, %rax
  ret
|}]

let add x y = x + y
[%%expect_asm X86_64{|
add:
  leaq  -1(%rax,%rbx), %rax
  ret
|}]

let sub x y = x - y
[%%expect_asm X86_64{|
sub:
  subq  %rbx, %rax
  incq  %rax
  ret
|}]

let mul x y = x * y
[%%expect_asm X86_64{|
mul:
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  ret
|}]

(* CR ttebbi: imul could be replaced with lea (x*2+x-2) *)
let mul_3 x = x * 3
[%%expect_asm X86_64{|
mul_3:
  imulq $3, %rax
  addq  $-2, %rax
  ret
|}]

let div x y = x / y
[%%expect_asm X86_64{|
div:
  movq  %rbx, %rcx
  cmpq  $1, %rcx
  je    .L115
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rax,%rax), %rax
  ret
.L115:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let div_by_constant x = x / 1234
[%%expect_asm X86_64{|
div_by_constant:
  sarq  $1, %rax
  movq  %rax, %rbx
  shrq  $63, %rbx
  movabsq $7653754429286296943, %rdi
  imulq %rdi
  sarq  $9, %rdx
  addq  %rbx, %rdx
  leaq  1(%rdx,%rdx), %rax
  ret
|}]

(* CR ttebbi:
    The last two instructions:
      sarq  $1, %rax
      leaq  1(%rax,%rax), %rax
    are the same as
      orq $1, %rax
*)
let div_2 x = x / 2
[%%expect_asm X86_64{|
div_2:
  sarq  $1, %rax
  movq  %rax, %rbx
  shrq  $63, %rbx
  addq  %rbx, %rax
  sarq  $1, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let rem x y = x mod y
[%%expect_asm X86_64{|
rem:
  movq  %rbx, %rcx
  cmpq  $1, %rcx
  je    .L115
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rdx,%rdx), %rax
  ret
.L115:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

(* CR ttebbi: This could be:
    movq  %rax, %rbx
    and   $3, %rax
    cmpq  $0, $rbx
    leaq  -4(%rax), %rbx
    cmovgq %rax, %rcx
    ret
*)
let rem_2 x = x mod 2
[%%expect_asm X86_64{|
rem_2:
  sarq  $1, %rax
  movq  $-2, %rsi
  movq  %rax, %rbx
  shrq  $63, %rbx
  movq  %rax, %rdi
  addq  %rbx, %rdi
  andq  %rsi, %rdi
  subq  %rdi, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let succ x = x + 1
[%%expect_asm X86_64{|
succ:
  addq  $2, %rax
  ret
|}]

let pred x = x - 1
[%%expect_asm X86_64{|
pred:
  addq  $-2, %rax
  ret
|}]


(* CR ttebbi: This should be branchfree. *)
let abs x = abs x
[%%expect_asm X86_64{|
abs:
  movq  %rax, %rbx
  cmpq  $1, %rbx
  jl    .L105
  movq  %rbx, %rax
  ret
.L105:
  movl  $2, %eax
  subq  %rbx, %rax
  ret
|}]


let logand x y = x land y
[%%expect_asm X86_64{|
logand:
  andq  %rbx, %rax
  ret
|}]


let logor x y = x lor y
[%%expect_asm X86_64{|
logor:
  orq   %rbx, %rax
  ret
|}]


let logxor x y = x lxor y
[%%expect_asm X86_64{|
logxor:
  xorq  %rbx, %rax
  orq   $1, %rax
  ret
|}]


(* CR ttebbi: This could be `xorq  $-2, %rax` *)
let lognot x = lnot x
[%%expect_asm X86_64{|
lognot:
  xorq  $-1, %rax
  orq   $1, %rax
  ret
|}]


let shift_left x y = x lsl y
[%%expect_asm X86_64{|
shift_left:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  decq  %rax
  salq  %cl, %rax
  incq  %rax
  ret
|}]


let shift_right x y = x asr y
[%%expect_asm X86_64{|
shift_right:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  orq   $1, %rax
  ret
|}]


let shift_right_logical x y = x lsr y
[%%expect_asm X86_64{|
shift_right_logical:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  shrq  %cl, %rax
  orq   $1, %rax
  ret
|}]


(* CR ttebbi: There is no need to repeat compq. *)
let compare (x: int) (y: int) = compare x y
[%%expect_asm X86_64{|
compare:
  movq  %rax, %rdi
  cmpq  %rbx, %rdi
  setl  %al
  movzbq %al, %rsi
  cmpq  %rbx, %rdi
  setg  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]


let equal (x: int) (y: int) = x = y
[%%expect_asm X86_64{|
equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]


(* CR ttebbi: This is very inefficient, should be like `equal`. *)
let equal_using_compare (x: int) (y: int) = Stdlib.compare x y = 0
[%%expect_asm X86_64{|
equal_using_compare:
  movq  %rax, %rdi
  cmpq  %rbx, %rdi
  setl  %al
  movzbq %al, %rsi
  cmpq  %rbx, %rdi
  setg  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
  cmpq  $1, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]


(* CR ttebbi: This should be branchfree. *)
let min (x: int) (y: int) = min x y
[%%expect_asm X86_64{|
min:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jg    .L105
  movq  %rdi, %rax
  ret
.L105:
  ret
|}]

(* CR ttebbi: This should be branchfree. *)
let max (x: int) (y: int) = max x y
[%%expect_asm X86_64{|
max:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jl    .L105
  movq  %rdi, %rax
  ret
.L105:
  ret
|}]


(* CR ttebbi: The two -1 could be folded together. *)
let nested_arithmetic x y = (x + y) + (x + y)
[%%expect_asm X86_64{|
nested_arithmetic:
  leaq  -1(%rax,%rbx), %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}]


(* CR ttebbi:
  - We should detect that `n mod 2` is just a bitwise-and because n is positive.
  - We should use untagged arithmetic for `n`, as it never leaves the loop.
*)
let collatz n =
  let rec loop n steps =
      if n <= 1
      then steps
      else loop (if n mod 2 = 0 then n/2 else 3*n + 1) (steps + 1)
  in loop n 0
;;
[%%expect_asm X86_64{|
collatz:
  movq  %rax, %rbx
  movl  $1, %eax
  cmpq  $3, %rbx
  jg    .L110
.L108:
  ret
.L110:
  addq  $2, %rax
  movq  %rbx, %rdi
  sarq  $1, %rdi
  movq  $-2, %rcx
  movq  %rdi, %rsi
  shrq  $63, %rsi
  movq  %rdi, %rdx
  addq  %rsi, %rdx
  movq  %rdx, %rsi
  andq  %rcx, %rsi
  subq  %rsi, %rdi
  leaq  1(%rdi,%rdi), %rdi
  cmpq  $1, %rdi
  jne   .L126
  sarq  $1, %rdx
  leaq  1(%rdx,%rdx), %rbx
  cmpq  $3, %rbx
  jg    .L110
  jmp   .L108
.L126:
  imulq $3, %rbx
  cmpq  $3, %rbx
  jg    .L110
  jmp   .L108
|}]
