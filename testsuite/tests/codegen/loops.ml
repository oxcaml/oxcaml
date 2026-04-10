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
.L101:
  leaq  -1(%rbx,%rax), %rbx
  cmpq  $1, %rax
  jne   .L105
  movq  %rbx, (%rsp)
  movl  $1, %eax
  call  camlTOP2__cold_1_4_code@PLT
.L109:
  movq  (%rsp), %rax
  addq  $8, %rsp
  ret
.L105:
  addq  $-2, %rax
  jmp   .L101

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
  jl    .L102
  subq  $24, %rsp
  movq  %rbx, (%rsp)
  sarq  $1, %rax
  movq  %rax, 8(%rsp)
  xorl  %eax, %eax
.L104:
  movq  %rax, 16(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L112:
  movq  16(%rsp), %rax
  incq  %rax
  movq  (%rsp), %rbx
  movq  8(%rsp), %rdi
  cmpq  %rdi, %rax
  jle   .L104
  movl  $1, %eax
  addq  $24, %rsp
  ret
.L102:
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
.L103:
  testb $1, %bl
  je    .L106
  movq  camlStdlib__List__Pmakeblock2305@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L106:
  movq  (%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jg    .L103
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
  jb    .L103
.L105:
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
.L112:
  movq  16(%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  incq  %rdi
  cmpq  $100, %rdi
  jle   .L112
  ret
|}]


(* CR ttebbi: noop loop could be eliminated
   https://github.com/oxcaml/oxcaml/issues/4752 *)
let noop_loop lo hi = for i = lo to hi do () done
[%%expect_asm X86_64{|
noop_loop:
  cmpq  %rbx, %rax
  jg    .L109
  sarq  $1, %rax
  sarq  $1, %rbx
.L105:
  incq  %rax
  cmpq  %rbx, %rax
  jle   .L105
.L109:
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
  jl    .L102
  sarq  $1, %rbx
  movl  $1, %eax
  xorl  %edi, %edi
.L104:
  movq  %rdi, %rsi
  imulq $6, %rsi
  addq  %rsi, %rax
  incq  %rdi
  cmpq  %rbx, %rdi
  jle   .L104
  ret
.L102:
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
  jl    .L109
  sarq  $1, %rax
  vxorpd %xmm0, %xmm0, %xmm0
  xorl  %ebx, %ebx
.L111:
  movq  %rbx, %rsi
  imulq $6, %rsi
  incq  %rsi
  vmovsd -4(%rdi,%rsi,4), %xmm1
  vmulsd 4(%rdi,%rsi,4), %xmm1, %xmm1
  vmulsd 12(%rdi,%rsi,4), %xmm1, %xmm1
  vaddsd %xmm1, %xmm0, %xmm0
  incq  %rbx
  cmpq  %rax, %rbx
  jle   .L111
  ret
.L109:
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
  jne   .L106
  jmp   .L103
.L104:
  cmpq  $1, %rax
  je    .L103
.L106:
  movq  %rdi, 16(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L114:
  movq  (%rsp), %rax
  movq  8(%rsp), %rbx
  movq  16(%rsp), %rdi
.L103:
  incq  %rdi
  cmpq  $9, %rdi
  jle   .L104
  movl  $1, %eax
  addq  $24, %rsp
  ret
|}]
