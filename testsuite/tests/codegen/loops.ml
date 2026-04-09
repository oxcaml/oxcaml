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
 flags += " -cfg-merge-blocks";
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
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L106:
=======
.L101:
>>>>>>> 42782c097b (passes testsuite)
  leaq  -1(%rbx,%rax), %rbx
  cmpq  $1, %rax
<<<<<<< HEAD
  jne   .L2
||||||| parent of 42782c097b (passes testsuite)
  jne   .L112
=======
  jne   .L105
>>>>>>> 42782c097b (passes testsuite)
  movq  %rbx, (%rsp)
  movl  $1, %eax
  call  camlTOP2__cold_1_4_code@PLT
<<<<<<< HEAD
<<<<<<< HEAD
.L1:
||||||| parent of de3d4ac415 (working prototype)
.L118:
=======
.L126:
>>>>>>> de3d4ac415 (working prototype)
||||||| parent of 42782c097b (passes testsuite)
.L126:
=======
.L109:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rsp), %rax
  addq  $8, %rsp
  ret
<<<<<<< HEAD
.L2:
||||||| parent of 42782c097b (passes testsuite)
.L112:
=======
.L105:
>>>>>>> 42782c097b (passes testsuite)
  addq  $-2, %rax
<<<<<<< HEAD
  jmp   .L0
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L106
=======
  jmp   .L101
>>>>>>> 42782c097b (passes testsuite)

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
<<<<<<< HEAD
  jl    .L2
||||||| parent of 42782c097b (passes testsuite)
  jl    .L120
=======
  jl    .L102
>>>>>>> 42782c097b (passes testsuite)
  subq  $24, %rsp
  movq  %rbx, (%rsp)
  sarq  $1, %rax
  movq  %rax, 8(%rsp)
  xorl  %eax, %eax
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L109:
=======
.L104:
>>>>>>> 42782c097b (passes testsuite)
  movq  %rax, 16(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
<<<<<<< HEAD
<<<<<<< HEAD
.L1:
||||||| parent of de3d4ac415 (working prototype)
.L124:
=======
.L135:
>>>>>>> de3d4ac415 (working prototype)
||||||| parent of 42782c097b (passes testsuite)
.L135:
=======
.L112:
>>>>>>> 42782c097b (passes testsuite)
  movq  16(%rsp), %rax
  incq  %rax
  movq  (%rsp), %rbx
  movq  8(%rsp), %rdi
  cmpq  %rdi, %rax
<<<<<<< HEAD
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  jle   .L109
=======
  jle   .L104
>>>>>>> 42782c097b (passes testsuite)
  movl  $1, %eax
  addq  $24, %rsp
  ret
<<<<<<< HEAD
.L2:
||||||| parent of 42782c097b (passes testsuite)
.L120:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
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
  subq  $8, %rsp
  movl  $1, %eax
  movl  $201, %edi
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L108:
=======
.L103:
>>>>>>> 42782c097b (passes testsuite)
  testb $1, %bl
<<<<<<< HEAD
  je    .L1
||||||| parent of 42782c097b (passes testsuite)
  je    .L112
=======
  je    .L106
>>>>>>> 42782c097b (passes testsuite)
  movq  camlStdlib__List__Pmakeblock2305@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L112:
=======
.L106:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
<<<<<<< HEAD
  jg    .L0
||||||| parent of 42782c097b (passes testsuite)
  jg    .L108
=======
  jg    .L103
  addq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
<<<<<<< HEAD
  jb    .L105
.L0:
||||||| parent of de3d4ac415 (working prototype)
  jb    .L105
.L107:
=======
  jb    .L108
.L110:
>>>>>>> de3d4ac415 (working prototype)
||||||| parent of 42782c097b (passes testsuite)
  jb    .L108
.L110:
=======
  jb    .L103
.L105:
>>>>>>> 42782c097b (passes testsuite)
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
  subq  $8, %rsp
  movq  16(%rbx), %rax
  leaq  -1(%rax,%rax), %rax
  movl  $1, %edi
<<<<<<< HEAD
<<<<<<< HEAD
.L0:
||||||| parent of de3d4ac415 (working prototype)
.L120:
=======
.L123:
>>>>>>> de3d4ac415 (working prototype)
||||||| parent of 42782c097b (passes testsuite)
.L123:
=======
.L112:
>>>>>>> 42782c097b (passes testsuite)
  movq  16(%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  incq  %rdi
  cmpq  $100, %rdi
<<<<<<< HEAD
<<<<<<< HEAD
  jle   .L0
||||||| parent of de3d4ac415 (working prototype)
  jle   .L120
=======
  jle   .L123
>>>>>>> de3d4ac415 (working prototype)
||||||| parent of 42782c097b (passes testsuite)
  jle   .L123
=======
  jle   .L112
  addq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
  ret
|}]


(* CR ttebbi: noop loop could be eliminated
   https://github.com/oxcaml/oxcaml/issues/4752 *)
let noop_loop lo hi = for i = lo to hi do () done
[%%expect_asm X86_64{|
noop_loop:
  cmpq  %rbx, %rax
<<<<<<< HEAD
  jg    .L1
||||||| parent of 42782c097b (passes testsuite)
  jg    .L119
=======
  jg    .L102
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
  sarq  $1, %rax
  sarq  $1, %rbx
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L110:
=======
.L105:
>>>>>>> 42782c097b (passes testsuite)
  incq  %rax
  cmpq  %rbx, %rax
<<<<<<< HEAD
  jle   .L0
.L1:
||||||| parent of 42782c097b (passes testsuite)
  jle   .L110
.L119:
=======
  jle   .L105
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L102:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  jl    .L1
||||||| parent of 42782c097b (passes testsuite)
  jl    .L121
=======
  jl    .L102
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
  sarq  $1, %rbx
  movl  $1, %eax
  xorl  %edi, %edi
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L109:
=======
.L104:
>>>>>>> 42782c097b (passes testsuite)
  movq  %rdi, %rsi
  imulq $6, %rsi
  addq  %rsi, %rax
  incq  %rdi
  cmpq  %rbx, %rdi
<<<<<<< HEAD
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  jle   .L109
=======
  jle   .L104
  addq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L121:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
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
  movq  %rax, %rdi
  movq  -8(%rdi), %rax
  salq  $8, %rax
  shrq  $18, %rax
  movq  %rax, %rbx
  shrq  $63, %rbx
  movabsq $6148914691236517206, %rsi
  imulq %rsi
  leaq  (%rdx,%rbx), %rax
  leaq  -1(%rax,%rax), %rax
  cmpq  $1, %rax
<<<<<<< HEAD
  jl    .L1
||||||| parent of 42782c097b (passes testsuite)
  jl    .L132
=======
  jl    .L109
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
  sarq  $1, %rax
  vxorpd %xmm0, %xmm0, %xmm0
<<<<<<< HEAD
  xorl  %edi, %edi
.L0:
  movq  %rdi, %rsi
||||||| parent of 42782c097b (passes testsuite)
  xorl  %edi, %edi
.L116:
  movq  %rdi, %rsi
=======
  xorl  %ebx, %ebx
.L111:
  movq  %rbx, %rsi
>>>>>>> 42782c097b (passes testsuite)
  imulq $6, %rsi
  incq  %rsi
  vmovsd -4(%rdi,%rsi,4), %xmm1
  vmulsd 4(%rdi,%rsi,4), %xmm1, %xmm1
  vmulsd 12(%rdi,%rsi,4), %xmm1, %xmm1
  vaddsd %xmm1, %xmm0, %xmm0
<<<<<<< HEAD
  incq  %rdi
  cmpq  %rax, %rdi
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  incq  %rdi
  cmpq  %rax, %rdi
  jle   .L116
=======
  incq  %rbx
  cmpq  %rax, %rbx
  jle   .L111
  addq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L132:
=======
.L109:
>>>>>>> 42782c097b (passes testsuite)
  vxorpd %xmm0, %xmm0, %xmm0
  ret
|}]


(* CR ttebbi: We should branch first and loop afterwards.*)
let loop_invariant_code (should_call_f : bool) (f : unit -> unit) =
  for _ = 0 to 9 do
    if should_call_f then f ()
  done
[%%expect_asm X86_64{|
loop_invariant_code:
  subq  $24, %rsp
  movq  %rax, (%rsp)
  movq  %rbx, 8(%rsp)
  xorl  %edi, %edi
  cmpq  $1, %rax
<<<<<<< HEAD
  je    .L3
  jmp   .L1
.L0:
||||||| parent of 42782c097b (passes testsuite)
  je    .L118
  jmp   .L113
.L109:
=======
  jne   .L106
  jmp   .L103
.L104:
>>>>>>> 42782c097b (passes testsuite)
  cmpq  $1, %rax
<<<<<<< HEAD
  je    .L3
.L1:
||||||| parent of 42782c097b (passes testsuite)
  je    .L118
.L113:
=======
  je    .L103
.L106:
>>>>>>> 42782c097b (passes testsuite)
  movq  %rdi, 16(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
<<<<<<< HEAD
.L2:
||||||| parent of 42782c097b (passes testsuite)
.L130:
=======
.L114:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rsp), %rax
  movq  8(%rsp), %rbx
  movq  16(%rsp), %rdi
<<<<<<< HEAD
.L3:
||||||| parent of 42782c097b (passes testsuite)
.L118:
=======
.L103:
>>>>>>> 42782c097b (passes testsuite)
  incq  %rdi
  cmpq  $9, %rdi
<<<<<<< HEAD
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  jle   .L109
=======
  jle   .L104
>>>>>>> 42782c097b (passes testsuite)
  movl  $1, %eax
  addq  $24, %rsp
  ret
|}]
