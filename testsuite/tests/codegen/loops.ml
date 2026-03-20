(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
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

(* CR ttebbi: For for loops, we could avoid the unconditional jumps even without
   a general loop rotation optimization by just lowering them right. *)
let for_loop_layout n f =
  for i = 0 to n do
    f()
  done
[%%expect_asm X86_64{|
for_loop_layout:
  cmpq  $1, %rax
  jl    .L119
  subq  $24, %rsp
  movq  %rbx, 8(%rsp)
  movq  %rax, (%rsp)
  movl  $1, %eax
.L108:
  movq  %rax, 16(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L123:
  movq  (%rsp), %rdi
  movq  8(%rsp), %rbx
  movq  16(%rsp), %rax
  cmpq  %rdi, %rax
  je    .L115
  addq  $2, %rax
  jmp   .L108
.L115:
  movl  $1, %eax
  addq  $24, %rsp
  ret
.L119:
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
  movl  $3, %edi
.L120:
  movq  16(%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  cmpq  $201, %rdi
  je    .L127
  addq  $2, %rdi
  jmp   .L120
.L127:
  ret
|}]


(* CR ttebbi: noop loop could be eliminated
   https://github.com/oxcaml/oxcaml/issues/4752 *)
let noop_loop lo hi = for i = lo to hi do () done
[%%expect_asm X86_64{|
noop_loop:
  cmpq  %rbx, %rax
  jg    .L117
  cmpq  %rbx, %rax
  je    .L113
.L110:
  addq  $2, %rax
  cmpq  %rbx, %rax
  jne   .L110
.L113:
  movl  $1, %eax
  ret
.L117:
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
  jl    .L119
  movl  $1, %eax
  movl  $1, %edi
.L108:
  leaq  (%rdi,%rdi,2), %rsi
  leaq  -3(%rax,%rsi), %rax
  cmpq  %rbx, %rdi
  je    .L115
  addq  $2, %rdi
  jmp   .L108
.L115:
  ret
.L119:
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
  jl    .L129
  vxorpd %xmm0, %xmm0, %xmm0
  movl  $1, %edi
.L115:
  leaq  -2(%rdi,%rdi,2), %rsi
  vmovsd -4(%rbx,%rsi,4), %xmm1
  vmulsd 4(%rbx,%rsi,4), %xmm1, %xmm1
  vmulsd 12(%rbx,%rsi,4), %xmm1, %xmm1
  vaddsd %xmm1, %xmm0, %xmm0
  cmpq  %rax, %rdi
  je    .L125
  addq  $2, %rdi
  jmp   .L115
.L125:
  ret
.L129:
  vxorpd %xmm0, %xmm0, %xmm0
  ret
|}]
