(* TEST
 flags += " -O3 -extension-universe upstream_compatible";
 include stdlib_upstream_compatible;
 only-default-codegen;
 expect.opt;
*)

open Stdlib_upstream_compatible


let neg x = Float_u.neg x
[%%expect_asm X86_64{|
neg:
  vxorpd caml_negf_mask(%rip), %xmm0, %xmm0
  ret
|}]

let add x y = Float_u.add x y
[%%expect_asm X86_64{|
add:
  vaddsd %xmm1, %xmm0, %xmm0
  ret
|}]

let sub x y = Float_u.sub x y
[%%expect_asm X86_64{|
sub:
  vsubsd %xmm1, %xmm0, %xmm0
  ret
|}]

let mul x y = Float_u.mul x y
[%%expect_asm X86_64{|
mul:
  vmulsd %xmm1, %xmm0, %xmm0
  ret
|}]

let div x y = Float_u.div x y
[%%expect_asm X86_64{|
div:
  vdivsd %xmm1, %xmm0, %xmm0
  ret
|}]

let abs x = Float_u.abs x
[%%expect_asm X86_64{|
abs:
  vandpd caml_absf_mask(%rip), %xmm0, %xmm0
  ret
|}]

(* CR ttebbi: This should be vsqrtsd %xmm0, %xmm0, %xmm0 *)
let sqrt x = Float_u.sqrt x
[%%expect_asm X86_64{|
sqrt:
  vxorpd %xmm1, %xmm1, %xmm1
  vsqrtsd %xmm0, %xmm1, %xmm0
  ret
|}]

let fma x y z = Float_u.fma x y z
[%%expect_asm X86_64{|
fma:
  subq  $8, %rsp
  call  caml_fma@PLT
  addq  $8, %rsp
  ret
|}]

let of_int x = Float_u.of_int x
[%%expect_asm X86_64{|
of_int:
  sarq  $1, %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

let to_int x = Float_u.to_int x
[%%expect_asm X86_64{|
to_int:
  vcvttsd2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let of_float x = Float_u.of_float x
[%%expect_asm X86_64{|
of_float:
  vmovsd (%rax), %xmm0
  ret
|}]

(* CR ttebbi: This function only needs a stack for calling the GC, so we should avoid
   adjusting %rsp in the common case. *)
let to_float x = Float_u.to_float x
[%%expect_asm X86_64{|
to_float:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L104
.L106:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]


external float_equal :
  (float[@local_opt]) -> (float[@local_opt]) -> bool @@ portable = "%equal"


(* CR ttebbi: We could save the neg instruction by negating the vcmpsd predicate. *)
let equal x y = float_equal (Float_u.to_float x) (Float_u.to_float y)
[%%expect_asm X86_64{|
equal:
  subq  $8, %rsp
  vcmpsd $0, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  vmovsd %xmm0, (%rsp)
  leaq  1(%rax,%rax), %rax
  addq  $8, %rsp
  ret
|}]

(* CR ttebbi: excessive amount of code, calls to caml_signbit, unnecessary spilling *)
let min x y = Float_u.min x y
[%%expect_asm X86_64{|
min:
  subq  $24, %rsp
  vmovsd %xmm0, (%rsp)
  vmovsd %xmm1, 8(%rsp)
  vmovsd 8(%rsp), %xmm0
  vmovsd (%rsp), %xmm1
  vcomisd %xmm1, %xmm0
  ja    .L134
  vmovsd 8(%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L126
  vmovsd (%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L134
.L126:
  vmovsd (%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L130
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
.L130:
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
.L134:
  vmovsd 8(%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L138
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
.L138:
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
|}]

(* CR ttebbi: excessive amount of code, calls to caml_signbit, unnecessary spilling *)
let max x y = Float_u.max x y
[%%expect_asm X86_64{|
max:
  subq  $24, %rsp
  vmovsd %xmm0, (%rsp)
  vmovsd %xmm1, 8(%rsp)
  vmovsd 8(%rsp), %xmm0
  vmovsd (%rsp), %xmm1
  vcomisd %xmm1, %xmm0
  ja    .L134
  vmovsd 8(%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L126
  vmovsd (%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L134
.L126:
  vmovsd 8(%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L130
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
.L130:
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
.L134:
  vmovsd (%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L138
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
.L138:
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
|}]

(* CR ttebbi: excessive amount of code, calls to caml_signbit, unnecessary spilling *)
let min_num x y = Float_u.min_num x y
[%%expect_asm X86_64{|
min_num:
  subq  $24, %rsp
  vmovsd %xmm0, (%rsp)
  vmovsd %xmm1, 8(%rsp)
  vmovsd 8(%rsp), %xmm0
  vmovsd (%rsp), %xmm1
  vcomisd %xmm1, %xmm0
  ja    .L134
  vmovsd 8(%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L126
  vmovsd (%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L134
.L126:
  vmovsd 8(%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L130
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
.L130:
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
.L134:
  vmovsd (%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L138
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
.L138:
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
|}]

(* CR ttebbi: excessive amount of code, calls to caml_signbit, unnecessary spilling *)
let max_num x y = Float_u.max_num x y
[%%expect_asm X86_64{|
max_num:
  subq  $24, %rsp
  vmovsd %xmm0, (%rsp)
  vmovsd %xmm1, 8(%rsp)
  vmovsd 8(%rsp), %xmm0
  vmovsd (%rsp), %xmm1
  vcomisd %xmm1, %xmm0
  ja    .L134
  vmovsd 8(%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L126
  vmovsd (%rsp), %xmm0
  call  caml_signbit@PLT
  cmpq  $1, %rax
  jne   .L134
.L126:
  vmovsd (%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L130
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
.L130:
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
.L134:
  vmovsd 8(%rsp), %xmm0
  vucomisd %xmm0, %xmm0
  jnp   .L138
  vmovsd (%rsp), %xmm0
  addq  $24, %rsp
  ret
.L138:
  vmovsd 8(%rsp), %xmm0
  addq  $24, %rsp
  ret
|}]

(* CR ttebbi: This could use the minsd instructions. *)
let min_unchecked (a : Float_u.t) (b : Float_u.t) =
  let ( < ) a b = Float.(Float_u.to_float a < Float_u.to_float b) in
  if a < b then a else b
;;
[%%expect_asm X86_64{|
min_unchecked:
  vmovapd %xmm0, %xmm2
  vmovapd %xmm1, %xmm0
  vcomisd %xmm2, %xmm0
  jbe   .L105
  vmovapd %xmm2, %xmm0
  ret
.L105:
  ret
|}]

(* CR ttebbi: Bad codegen:
      - useless spill and hence no need for a frame
      - could negate vcmpsd predicate to replace (~res)*2+1 with res*2+3
*)
let is_nan (x: Float_u.t) = Float.is_nan (Float_u.to_float x)
[%%expect_asm X86_64{|
is_nan:
  subq  $8, %rsp
  vcmpsd $4, %xmm0, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  vmovsd %xmm0, (%rsp)
  leaq  1(%rax,%rax), %rax
  addq  $8, %rsp
  ret
|}]


(* CR ttebbi: Bad codegen:
      - useless spill and hence no need for a frame
      - could negate vcmpsd predicate to replace (~res)*2+1 with res*2+3
*)
let is_finite (x: Float_u.t) =
  let equal x y = float_equal (Float_u.to_float x) (Float_u.to_float y) in
  let zero_or_nan = Float_u.sub x x in
  equal zero_or_nan zero_or_nan
;;
[%%expect_asm X86_64{|
is_finite:
  subq  $8, %rsp
  vsubsd %xmm0, %xmm0, %xmm0
  vcmpsd $0, %xmm0, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  vmovsd %xmm0, (%rsp)
  leaq  1(%rax,%rax), %rax
  addq  $8, %rsp
  ret
|}]


(* CR ttebbi:
    - OCaml's Float.compare is difficult to implement, but we could at least
      compose the bits within xmm registers.
   - Plenty of useless spills
*)
let compare (x: Float_u.t) (y: Float_u.t) : int =
   Float_u.compare x y
;;
[%%expect_asm X86_64{|
compare:
  subq  $8, %rsp
  vcmpsd $0, %xmm1, %xmm1, %xmm2
  vmovq %xmm2, %rbx
  neg   %rbx
  vmovsd %xmm2, (%rsp)
  vcmpsd $0, %xmm0, %xmm0, %xmm2
  vmovq %xmm2, %rax
  neg   %rax
  vmovsd %xmm2, (%rsp)
  subq  %rbx, %rax
  vcmpsd $1, %xmm1, %xmm0, %xmm2
  vmovq %xmm2, %rdi
  neg   %rdi
  vmovsd %xmm2, (%rsp)
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rbx
  neg   %rbx
  vmovsd %xmm0, (%rsp)
  subq  %rdi, %rbx
  addq  %rax, %rbx
  leaq  1(%rbx,%rbx), %rax
  addq  $8, %rsp
  ret
|}]



(* CR ttebbi: We do not optimize the case where we check for one value of Float.compare *)
let compare_equal (x: Float_u.t) (y: Float_u.t) : bool =
   Float_u.compare x y == 0
;;
[%%expect_asm X86_64{|
compare_equal:
  subq  $8, %rsp
  vcmpsd $0, %xmm1, %xmm1, %xmm2
  vmovq %xmm2, %rbx
  neg   %rbx
  vmovsd %xmm2, (%rsp)
  vcmpsd $0, %xmm0, %xmm0, %xmm2
  vmovq %xmm2, %rax
  neg   %rax
  vmovsd %xmm2, (%rsp)
  subq  %rbx, %rax
  vcmpsd $1, %xmm1, %xmm0, %xmm2
  vmovq %xmm2, %rdi
  neg   %rdi
  vmovsd %xmm2, (%rsp)
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rbx
  neg   %rbx
  vmovsd %xmm0, (%rsp)
  subq  %rdi, %rbx
  addq  %rax, %rbx
  leaq  1(%rbx,%rbx), %rax
  cmpq  $1, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  addq  $8, %rsp
  ret
|}]
