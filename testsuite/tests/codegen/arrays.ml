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

type 'a vec = #{
  size : int;
  buffer : 'a array;
}

(* CR ttebbi:
  - Array bounds checks need a lot of instructions.
  - We could inline a write-barrier fast path (like when the
    target object is young). This is the only way to do something
    like creating short arrays of pointers efficiently. *)
(* using option to avoid the float array case *)
let push (vec : 'a option vec) x =
  let #{size; buffer} = vec in
  buffer.(size) <- x;
  #{vec with size = size + 1}
;;
[%%expect_asm X86_64{|
push:
  subq  $8, %rsp
  movq  %rax, %r12
  movq  %rdi, %rsi
  movq  -8(%rbx), %rax
  salq  $8, %rax
  shrq  $17, %rax
  cmpq  %rax, %r12
  jae   .L117
  leaq  -4(%rbx,%r12,4), %rdi
  call  caml_modify@PLT
  leaq  2(%r12), %rax
  addq  $8, %rsp
  ret
.L117:
  movq  camlTOP3__block33@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]


external int64_u_unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (Int64_u.t[@local_opt]) -> 'a
    @@ portable
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly]

(* CR ttebbi: When indexing with an untagged index,
   there is no need to first tag the index. *)
let int32_unsafe_get_indexed_by_int64 (x : Int32_u.t array) (i : Int64_u.t) =
  int64_u_unsafe_get x i |> Int32_u.to_int32 |> Int64_u.of_int32
;;
[%%expect_asm X86_64{|
int32_unsafe_get_indexed_by_int64:
  leaq  1(%rbx,%rbx), %rbx
  movslq -2(%rax,%rbx,2), %rax
  ret
|}]

(* Unsafe get/set for each element type *)

let int_unsafe_get (a : int array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
int_unsafe_get:
  movq  -4(%rax,%rbx,4), %rax
  ret
|}]

let int_unsafe_set (a : int array) (i : int) (v : int) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
int_unsafe_set:
  movq  %rdi, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
|}]

let ref_unsafe_get (a : string array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
ref_unsafe_get:
  movq  -4(%rax,%rbx,4), %rax
  ret
|}]

let ref_unsafe_set (a : string array) (i : int) (v : string) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
ref_unsafe_set:
  subq  $8, %rsp
  movq  %rdi, %rsi
  leaq  -4(%rax,%rbx,4), %rdi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}]

(* CR ttebbi: Stackframe creation is only necessary on the GC-calling path.
   In this case, this would require moving the stackframe creation into the
   hidden block where the GC is actually called. *)
let poly_unsafe_get (a : 'a array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
poly_unsafe_get:
  movq  %rax, %rdi
  movzbq -8(%rdi), %rax
  cmpq  $254, %rax
  jne   .L108
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L119
.L121:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd -4(%rdi,%rbx,4), %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
.L108:
  movq  -4(%rdi,%rbx,4), %rax
  ret
|}]

let poly_unsafe_set (a : 'a array) (i : int) (v : 'a) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
poly_unsafe_set:
  movq  %rdi, %rsi
  movzbq -8(%rax), %rdi
  cmpq  $254, %rdi
  jne   .L108
  vmovsd (%rsi), %xmm0
  vmovsd %xmm0, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
.L108:
  subq  $8, %rsp
  leaq  -4(%rax,%rbx,4), %rdi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}]

let int32_unsafe_get (a : int32# array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
int32_unsafe_get:
  movslq -2(%rax,%rbx,2), %rax
  ret
|}]

let int32_unsafe_set (a : int32# array) (i : int) (v : int32#) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
int32_unsafe_set:
  movl  %edi, -2(%rax,%rbx,2)
  movl  $1, %eax
  ret
|}]

let int64_unsafe_get (a : int64# array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
int64_unsafe_get:
  movq  -4(%rax,%rbx,4), %rax
  ret
|}]

let int64_unsafe_set (a : int64# array) (i : int) (v : int64#) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
int64_unsafe_set:
  movq  %rdi, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
|}]

let float_unsafe_get (a : float# array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
float_unsafe_get:
  vmovsd -4(%rax,%rbx,4), %xmm0
  ret
|}]

let float_unsafe_set (a : float# array) (i : int) (v : float#) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
float_unsafe_set:
  vmovsd %xmm0, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
|}]

let float_unsafe_get_plain (a : float array) (i : int) =
  Float_u.of_float (Array.unsafe_get a i)
[%%expect_asm X86_64{|
float_unsafe_get_plain:
  vmovsd -4(%rax,%rbx,4), %xmm0
  ret
|}]

let float_unsafe_set_plain (a : float array) (i : int) (v : float#) =
  Array.unsafe_set a i (Float_u.to_float v)
[%%expect_asm X86_64{|
float_unsafe_set_plain:
  vmovsd %xmm0, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
|}]

let nativeint_unsafe_get (a : nativeint# array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
nativeint_unsafe_get:
  movq  -4(%rax,%rbx,4), %rax
  ret
|}]

let nativeint_unsafe_set (a : nativeint# array) (i : int)
    (v : nativeint#) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
nativeint_unsafe_set:
  movq  %rdi, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
|}]

let floatarray_unsafe_get (a : floatarray) (i : int) =
  Float_u.of_float (Floatarray.unsafe_get a i)
[%%expect_asm X86_64{|
floatarray_unsafe_get:
  vmovsd -4(%rax,%rbx,4), %xmm0
  ret
|}]

let floatarray_unsafe_set (a : floatarray) (i : int) (v : float#) =
  Floatarray.unsafe_set a i (Float_u.to_float v)
[%%expect_asm X86_64{|
floatarray_unsafe_set:
  vmovsd %xmm0, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
|}]

let float32_unsafe_get (a : float32# array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
float32_unsafe_get:
  vmovss -2(%rax,%rbx,2), %xmm0
  ret
|}]

let float32_unsafe_set (a : float32# array) (i : int) (v : float32#) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
float32_unsafe_set:
  vmovss %xmm0, -2(%rax,%rbx,2)
  movl  $1, %eax
  ret
|}]

let int8_unsafe_get (a : int8# array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
int8_unsafe_get:
  sarq  $1, %rbx
  movsbq (%rax,%rbx), %rax
  ret
|}]

let int8_unsafe_set (a : int8# array) (i : int) (v : int8#) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
int8_unsafe_set:
  sarq  $1, %rbx
  movb  %dil, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let int16_unsafe_get (a : int16# array) (i : int) =
  Array.unsafe_get a i
[%%expect_asm X86_64{|
int16_unsafe_get:
  movswq -1(%rax,%rbx), %rax
  ret
|}]

let int16_unsafe_set (a : int16# array) (i : int) (v : int16#) =
  Array.unsafe_set a i v
[%%expect_asm X86_64{|
int16_unsafe_set:
  movw  %di, -1(%rax,%rbx)
  movl  $1, %eax
  ret
|}]

(* Array length *)

let int_length (a : int array) = Array.length a
[%%expect_asm X86_64{|
int_length:
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $17, %rax
  orq   $1, %rax
  ret
|}]

let poly_length (a : 'a array) = Array.length a
[%%expect_asm X86_64{|
poly_length:
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $17, %rax
  orq   $1, %rax
  ret
|}]

(* CR ttebbi: The header is loaded twice. Also, extracting the bits can be done
    better. Also for other arrays with element size < 8 below. *)
let int32_length (a : int32# array) = Array.length a
[%%expect_asm X86_64{|
int32_length:
  movzbq -8(%rax), %rbx
  andl  $1, %ebx
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $18, %rax
  salq  $1, %rax
  subq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let int64_length (a : int64# array) = Array.length a
[%%expect_asm X86_64{|
int64_length:
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $18, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let float_length (a : float# array) = Array.length a
[%%expect_asm X86_64{|
float_length:
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $17, %rax
  orq   $1, %rax
  ret
|}]

let float32_length (a : float32# array) = Array.length a
[%%expect_asm X86_64{|
float32_length:
  movzbq -8(%rax), %rbx
  andl  $1, %ebx
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $18, %rax
  salq  $1, %rax
  subq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let int16_length (a : int16# array) = Array.length a
[%%expect_asm X86_64{|
int16_length:
  movzbq -8(%rax), %rbx
  andl  $3, %ebx
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $18, %rax
  salq  $2, %rax
  subq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let int8_length (a : int8# array) = Array.length a
[%%expect_asm X86_64{|
int8_length:
  movzbq -8(%rax), %rbx
  andl  $7, %ebx
  movq  -8(%rax), %rax
  salq  $8, %rax
  shrq  $18, %rax
  salq  $3, %rax
  subq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Safe (bounds-checked) array access *)

let int_safe_get (a : int array) (i : int) =
  Array.get a i
[%%expect_asm X86_64{|
int_safe_get:
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  cmpq  %rdi, %rbx
  jae   .L115
  movq  -4(%rax,%rbx,4), %rax
  ret
.L115:
  movq  camlTOP38__block1142@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let ref_safe_set (a : string array) (i : int) (v : string) =
  Array.set a i v
[%%expect_asm X86_64{|
ref_safe_set:
  subq  $8, %rsp
  movq  %rdi, %rsi
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  cmpq  %rdi, %rbx
  jae   .L116
  leaq  -4(%rax,%rbx,4), %rdi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L116:
  movq  camlTOP39__block1184@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

(* CR ttebbi: The header is loaded twice: once for the bounds check and once
   for the tag check (to distingish float arrays). The non-float case should
   be the inline one for code compactness.
*)
let poly_safe_get (a : 'a array) (i : int) =
  Array.get a i
[%%expect_asm X86_64{|
poly_safe_get:
  movq  %rax, %rdi
  movq  -8(%rdi), %rax
  salq  $8, %rax
  shrq  $17, %rax
  cmpq  %rax, %rbx
  jae   .L123
  movzbq -8(%rdi), %rax
  cmpq  $254, %rax
  jne   .L116
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L140
.L142:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd -4(%rdi,%rbx,4), %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
.L116:
  movq  -4(%rdi,%rbx,4), %rax
  ret
.L123:
  subq  $8, %rsp
  movq  camlTOP40__block1227@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let poly_safe_set (a : 'a array) (i : int) (v : 'a) =
  Array.set a i v
[%%expect_asm X86_64{|
poly_safe_set:
  movq  %rdi, %rsi
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  cmpq  %rdi, %rbx
  jae   .L124
  movzbq -8(%rax), %rdi
  cmpq  $254, %rdi
  jne   .L116
  vmovsd (%rsi), %xmm0
  vmovsd %xmm0, -4(%rax,%rbx,4)
  movl  $1, %eax
  ret
.L116:
  subq  $8, %rsp
  leaq  -4(%rax,%rbx,4), %rdi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L124:
  subq  $8, %rsp
  movq  camlTOP41__block1282@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

(* CR ttebbi: shrq $18 followed by salq $1 could be shrq $17. *)
let int64_safe_get (a : int64# array) (i : int) =
  Array.get a i
[%%expect_asm X86_64{|
int64_safe_get:
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $18, %rdi
  salq  $1, %rdi
  cmpq  %rdi, %rbx
  jae   .L116
  movq  -4(%rax,%rbx,4), %rax
  ret
.L116:
  movq  camlTOP42__block1339@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let float_safe_get (a : float# array) (i : int) =
  Array.get a i
[%%expect_asm X86_64{|
float_safe_get:
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  cmpq  %rdi, %rbx
  jae   .L115
  vmovsd -4(%rax,%rbx,4), %xmm0
  ret
.L115:
  movq  camlTOP43__block1380@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let float_safe_get_plain (a : float array) (i : int) =
  Float_u.of_float (Array.get a i)
[%%expect_asm X86_64{|
float_safe_get_plain:
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  cmpq  %rdi, %rbx
  jae   .L115
  vmovsd -4(%rax,%rbx,4), %xmm0
  ret
.L115:
  movq  camlTOP44__block1421@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let int32_safe_get (a : int32# array) (i : int) =
  Array.get a i
[%%expect_asm X86_64{|
int32_safe_get:
  movzbq -8(%rax), %rdi
  andl  $1, %edi
  movq  -8(%rax), %rsi
  salq  $8, %rsi
  shrq  $18, %rsi
  salq  $1, %rsi
  subq  %rdi, %rsi
  salq  $1, %rsi
  cmpq  %rsi, %rbx
  jae   .L120
  movslq -2(%rax,%rbx,2), %rax
  ret
.L120:
  movq  camlTOP45__block1466@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]
