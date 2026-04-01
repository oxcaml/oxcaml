(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 expect.opt;
*)

open Intrinsics


(* CR ttebbi: What could have been a nice dense loop got
   interrupted by the loop exit path. In addition, we should do
   loop rotation to remove the unconditional jump. *)
let loop_code_layout n =
  let[@cold] cold () = () in
  let sum = ref 0 in
  let rec loop x =
    sum := !sum + x;
    if x = 0 then (cold (); !sum) else loop (x - 1)
  in
  loop n
[%%expect_asm X86_64{|
loop_code_layout:
  subq  $8, %rsp
  movl  $1, %ebx
.L106:
  leaq  -1(%rbx,%rax), %rbx
  cmpq  $1, %rax
  jne   .L112
  movq  %rbx, (%rsp)
  movl  $1, %eax
  call  camlTOP2__cold_1_4_code@PLT
.L118:
  movq  (%rsp), %rax
  addq  $8, %rsp
  ret
.L112:
  addq  $-2, %rax
  jmp   .L106

loop_code_layout.cold:
  movl  $1, %eax
  ret
|}]

let for_loop_layout n f =
  for i = 0 to n do
    f()
  done
[%%expect_asm X86_64{|
for_loop_layout:
  cmpq  $1, %rax
  jl    .L120
  subq  $24, %rsp
  movq  %rbx, 16(%rsp)
  sarq  $1, %rax
  movq  %rax, (%rsp)
  xorl  %eax, %eax
.L109:
  movq  %rax, 8(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L124:
  movq  8(%rsp), %rax
  incq  %rax
  movq  (%rsp), %rbx
  cmpq  %rbx, %rax
  jg    .L116
  movq  16(%rsp), %rbx
  jmp   .L109
.L116:
  movl  $1, %eax
  addq  $24, %rsp
  ret
.L120:
  movl  $1, %eax
  ret
|}]

(* CR ttebbi: loop peeling could avoid repeating List.hd *)
let loop_with_non_dominating_load x l =
  let[@inline always] rec loop i acc =
    if i > 0 then loop (i - 1) (acc + List.hd l)
    else acc
  in
  loop 100 0
[%%expect_asm X86_64{|
loop_with_non_dominating_load:
  movl  $1, %eax
  movl  $201, %edi
.L108:
  testb $1, %bl
  je    .L112
  movq  camlStdlib__List__Pmakeblock2305@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L112:
  movq  (%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jg    .L108
  ret
|}]


(* CR ttebbi: Closure values are reloaded in the loop,
   despite having a dominating use. *)
let f x =
  let[@cold] do_work () =
    let sum = ref (x + x) in
    for i = 1 to 100 do
      sum := !sum + x
    done;
    !sum
  in
  do_work ()
[%%expect_asm X86_64{|
f:
  subq  $8, %rsp
  subq  $32, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rbx
  movq  $3319, -8(%rbx)
  movq  camlTOP5__do_work_11_15_code@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movabsq $108086391056891911, %rdi
  movq  %rdi, 8(%rbx)
  movq  %rax, 16(%rbx)
  movl  $1, %eax
  addq  $8, %rsp
  jmp   camlTOP5__do_work_11_15_code@PLT

f.do_work:
  movq  16(%rbx), %rax
  leaq  -1(%rax,%rax), %rax
  movl  $1, %edi
.L120:
  movq  16(%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  incq  %rdi
  cmpq  $100, %rdi
  jle   .L120
  ret
|}]


(* CR ttebbi: noop loop could be eliminated
   https://github.com/oxcaml/oxcaml/issues/4752 *)
let noop_loop lo hi = for i = lo to hi do () done
[%%expect_asm X86_64{|
noop_loop:
  cmpq  %rbx, %rax
  jg    .L119
  sarq  $1, %rax
  sarq  $1, %rbx
.L110:
  incq  %rax
  cmpq  %rbx, %rax
  jle   .L110
  movl  $1, %eax
  ret
.L119:
  movl  $1, %eax
  ret
|}]


(* CR ttebbi: We should replace a multiplication with a constant in a loop with
    an addition in an extra register. First is a minimal example and then a
    more realistic one.
*)

let f n =
  let sum = ref 0 in
  for i = 0 to n do
    sum := !sum + 3 * i;
  done;
  !sum
[%%expect_asm X86_64{|
f:
  movq  %rax, %rbx
  cmpq  $1, %rbx
  jl    .L121
  sarq  $1, %rbx
  movl  $1, %eax
  xorl  %edi, %edi
.L109:
  movq  %rdi, %rsi
  imulq $6, %rsi
  addq  %rsi, %rax
  incq  %rdi
  cmpq  %rbx, %rdi
  jle   .L109
  ret
.L121:
  movl  $1, %eax
  ret
|}]

module M = struct
  type f64 = Float_u.t
  type t = #(f64 * f64 * f64) array

  external unsafe_get :
    (t[@local_opt]) -> (int[@local_opt]) -> #(f64 * f64 * f64)
    @@ portable = "%array_unsafe_get"

  external length :
    (t[@local_opt]) @ immutable -> int @@ portable = "%array_length"

  let f (t : t) =
    let sum = ref 0.0 in
    for i = 0 to length t - 1 do
      let #(x,y,z) = unsafe_get t i in
      sum :=
        Float_u.(!sum +. to_float x *. to_float y *. to_float z)
    done;
    Float_u.of_float (!sum)
end
[%%expect_asm X86_64{|
M.f:
  movq  %rax, %rbx
  movq  -8(%rbx), %rax
  salq  $8, %rax
  shrq  $18, %rax
  movq  %rax, %rdi
  shrq  $63, %rdi
  movabsq $6148914691236517206, %rsi
  imulq %rsi
  leaq  (%rdx,%rdi), %rax
  leaq  -1(%rax,%rax), %rax
  cmpq  $1, %rax
  jl    .L132
  sarq  $1, %rax
  vxorpd %xmm0, %xmm0, %xmm0
  xorl  %edi, %edi
.L116:
  movq  %rdi, %rsi
  imulq $6, %rsi
  incq  %rsi
  vmovsd -4(%rbx,%rsi,4), %xmm1
  vmulsd 4(%rbx,%rsi,4), %xmm1, %xmm1
  vmulsd 12(%rbx,%rsi,4), %xmm1, %xmm1
  vaddsd %xmm1, %xmm0, %xmm0
  incq  %rdi
  cmpq  %rax, %rdi
  jle   .L116
  ret
.L132:
  vxorpd %xmm0, %xmm0, %xmm0
  ret
|}]
