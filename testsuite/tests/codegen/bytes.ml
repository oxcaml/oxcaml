(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -experimental-optimizations";
 flags += " -g -gdwarf-inlined-frames";
 expect.opt;
*)

open Intrinsics

let bytes_get_uint8 (buf : bytes) (i : int) =
  Bytes.unsafe_get buf i
[%%expect_asm X86_64{|
bytes_get_uint8:
  sarq  $1, %rbx
  movzbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
bytes_get_uint8:
.L0:
  add   x0, x0, x1, asr #1
  ldrb  w0, [x0, #0]
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let bytes_set_uint8 (buf : bytes) (i : int) (v : int) =
  Bytes.unsafe_set buf i v
[%%expect_asm X86_64{|
bytes_set_uint8:
  sarq  $1, %rbx
  sarq  $1, %rdi
  movb  %dil, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_uint8:
.L0:
  add   x0, x0, x1, asr #1
  sbfm  x1, x2, #1, #63
  strb  w1, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]


let bytes_get_int8 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int8 buf i
[%%expect_asm X86_64{|
bytes_get_int8:
  sarq  $1, %rbx
  movsbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
bytes_get_int8:
.L0:
  add   x0, x0, x1, asr #1
  ldrsb x0, [x0, #0]
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let bytes_get_uint16 (buf : bytes) (i : int) =
  Bytes.unsafe_get_uint16_ne buf i
[%%expect_asm X86_64{|
bytes_get_uint16:
  sarq  $1, %rbx
  movzwq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
bytes_get_uint16:
.L0:
  add   x0, x0, x1, asr #1
  ldrh  w0, [x0, #0]
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let bytes_set_uint16 (buf : bytes) (i : int) (v : int) =
  Bytes.unsafe_set_uint16_ne buf i v
[%%expect_asm X86_64{|
bytes_set_uint16:
  sarq  $1, %rbx
  sarq  $1, %rdi
  movw  %di, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_uint16:
.L0:
  add   x0, x0, x1, asr #1
  sbfm  x1, x2, #1, #63
  strh  w1, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_int16 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int16_ne buf i
[%%expect_asm X86_64{|
bytes_get_int16:
  sarq  $1, %rbx
  movswq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
bytes_get_int16:
.L0:
  add   x0, x0, x1, asr #1
  ldrsh x0, [x0, #0]
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let bytes_get_int32 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int32_ne buf i
[%%expect_asm X86_64{|
bytes_get_int32:
  sarq  $1, %rbx
  movslq (%rax,%rbx), %rax
  ret
|}, Arm64{|
bytes_get_int32:
.L0:
  add   x0, x0, x1, asr #1
  ldrsw x0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_int32 (buf : bytes) (i : int) (v : Int32_u.t) =
  Bytes.unsafe_set_int32_ne buf i v
[%%expect_asm X86_64{|
bytes_set_int32:
  sarq  $1, %rbx
  movl  %edi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int32:
.L0:
  add   x0, x0, x1, asr #1
  str   w2, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_int64 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int64_ne buf i
[%%expect_asm X86_64{|
bytes_get_int64:
  sarq  $1, %rbx
  movq  (%rax,%rbx), %rax
  ret
|}, Arm64{|
bytes_get_int64:
.L0:
  add   x0, x0, x1, asr #1
  ldr   x0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_int64 (buf : bytes) (i : int) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne buf i v
[%%expect_asm X86_64{|
bytes_set_int64:
  sarq  $1, %rbx
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int64:
.L0:
  add   x0, x0, x1, asr #1
  dmb   ishld
  str   x2, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_float32 (buf : bytes) (i : int) =
  Bytes.unsafe_get_float32_ne buf i
[%%expect_asm X86_64{|
bytes_get_float32:
  sarq  $1, %rbx
  vmovss (%rax,%rbx), %xmm0
  ret
|}, Arm64{|
bytes_get_float32:
.L0:
  add   x0, x0, x1, asr #1
  ldr   s0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_float32 (buf : bytes) (i : int) (v : Float32_u.t) =
  Bytes.unsafe_set_float32_ne buf i v
[%%expect_asm X86_64{|
bytes_set_float32:
  sarq  $1, %rbx
  vmovss %xmm0, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_float32:
.L0:
  add   x0, x0, x1, asr #1
  str   s0, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let string_get_int64 (s : string) (i : int) =
  String.unsafe_get_int64_ne s i
[%%expect_asm X86_64{|
string_get_int64:
  sarq  $1, %rbx
  movq  (%rax,%rbx), %rax
  ret
|}, Arm64{|
string_get_int64:
.L0:
  add   x0, x0, x1, asr #1
  ldr   x0, [x0, #0]
  ret
.L1:
|}]

let string_get_float32 (s : string) (i : int) =
  String.unsafe_get_float32_ne s i
[%%expect_asm X86_64{|
string_get_float32:
  sarq  $1, %rbx
  vmovss (%rax,%rbx), %xmm0
  ret
|}, Arm64{|
string_get_float32:
.L0:
  add   x0, x0, x1, asr #1
  ldr   s0, [x0, #0]
  ret
.L1:
|}]

(* CR ttebbi: The first [movslq] could be [movl] *)
(* CR ttebbi: mov + bswap could be movbe *)
let bytes_get_int32_bswap (buf : bytes) (i : int) =
  Int32_u.bswap (Bytes.unsafe_get_int32_ne buf i)
[%%expect_asm X86_64{|
bytes_get_int32_bswap:
  sarq  $1, %rbx
  movslq (%rax,%rbx), %rax
  bswap %eax
  movslq %eax, %rax
  ret
|}, Arm64{|
bytes_get_int32_bswap:
.L0:
  add   x0, x0, x1, asr #1
  ldrsw x0, [x0, #0]
.L1:
  rev   w0, w0
  sbfm  x0, x0, #0, #31
  ret
.L2:
|}]

let bytes_get_int64_bswap (buf : bytes) (i : int) =
  Int64_u.bswap (Bytes.unsafe_get_int64_ne buf i)
[%%expect_asm X86_64{|
bytes_get_int64_bswap:
  sarq  $1, %rbx
  movq  (%rax,%rbx), %rax
  bswap %rax
  ret
|}, Arm64{|
bytes_get_int64_bswap:
.L0:
  add   x0, x0, x1, asr #1
  ldr   x0, [x0, #0]
.L1:
  rev   x0, x0
  ret
.L2:
|}]

(* CR ttebbi: This bounds check is way too long, even given the complex
   length encoding. If we use an unsigned right shift to untag the int, then
   we could add the extra 3 bytes to the index instead of subtracting it from
   the length, removing the need for computing max(0, length - 3). *)
let bytes_safe_get_int32 (buf : bytes) (i : int) =
  Bytes.get_int32_ne buf i
[%%expect_asm X86_64{|
bytes_safe_get_int32:
  movq  -8(%rax), %rdi
  shrq  $10, %rdi
  leaq  -1(,%rdi,8), %rdi
  movzbq (%rax,%rdi), %rsi
  subq  %rsi, %rdi
  addq  $-3, %rdi
  sarq  $1, %rbx
  movq  %rdi, %rsi
  sarq  $63, %rsi
  notq  %rsi
  andq  %rdi, %rsi
  cmpq  %rsi, %rbx
  jae   .L0
  movslq (%rax,%rbx), %rax
  ret
.L0:
  subq  $8, %rsp
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  $0, 272(%r14)
  call  caml_raise_exn@PLT
.L1:
|}, Arm64{|
bytes_safe_get_int32:
.L0:
  ldr   x2, [x0, #-8]
  ubfm  x2, x2, #10, #63
  ubfm  x2, x2, #61, #60
  sub   x2, x2, #1
  add   x3, x0, x2
  ldrb  w3, [x3, #0]
  sub   x2, x2, x3
  sub   x2, x2, #3
  sbfm  x1, x1, #1, #63
  sbfm  x3, x2, #63, #63
  orn   x3, xzr, x3
  and   x2, x3, x2
  subs  xzr, x1, x2
  b.cs  .L1
  add   x0, x0, x1
  ldrsw x0, [x0, #0]
  ret
.L1:
  sub   sp, sp, #16
  str   lr, [sp, #8]
  adrp  x0, <hidden PC-relative offset>
  ldr   x0, [x0, <hidden PC-relative offset>]
  bl    caml_raise_exn
.L2:
.L3:
|}]

let bytes_get_int64_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_int64_ne_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int64:
  movq  (%rax,%rbx), %rax
  ret
|}, Arm64{|
bytes_get_int64_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldr   x0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_int64_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int64 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int64:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int64_indexed_by_int64:
.L0:
  add   x0, x0, x1
  dmb   ishld
  str   x2, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_int8_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_int8_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_int8_indexed_by_int64:
  movsbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
bytes_get_int8_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldrsb x0, [x0, #0]
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let bytes_set_int8_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) (v : int) =
  Bytes.unsafe_set_int8_indexed_by_int64 buf i v
[%%expect_asm X86_64{|
bytes_set_int8_indexed_by_int64:
  sarq  $1, %rdi
  movb  %dil, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int8_indexed_by_int64:
.L0:
  add   x0, x0, x1
  sbfm  x1, x2, #1, #63
  strb  w1, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_int32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_int32_ne_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_int32_indexed_by_int64:
  movslq (%rax,%rbx), %rax
  ret
|}, Arm64{|
bytes_get_int32_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldrsw x0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_int32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) (v : Int32_u.t) =
  Bytes.unsafe_set_int32_ne_indexed_by_int64 buf i v
[%%expect_asm X86_64{|
bytes_set_int32_indexed_by_int64:
  movl  %edi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int32_indexed_by_int64:
.L0:
  add   x0, x0, x1
  str   w2, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_float32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_float32_ne_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_float32_indexed_by_int64:
  vmovss (%rax,%rbx), %xmm0
  ret
|}, Arm64{|
bytes_get_float32_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldr   s0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_float32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) (v : Float32_u.t) =
  Bytes.unsafe_set_float32_ne_indexed_by_int64 buf i v
[%%expect_asm X86_64{|
bytes_set_float32_indexed_by_int64:
  vmovss %xmm0, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_float32_indexed_by_int64:
.L0:
  add   x0, x0, x1
  str   s0, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_int64_indexed_by_int32
    (buf : bytes) (i : Int32_u.t) =
  Bytes.unsafe_get_int64_ne_indexed_by_int32 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int32:
  movq  (%rax,%rbx), %rax
  ret
|}, Arm64{|
bytes_get_int64_indexed_by_int32:
.L0:
  add   x0, x0, x1
  ldr   x0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_int64_indexed_by_int32
    (buf : bytes) (i : Int32_u.t) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int32 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int32:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int64_indexed_by_int32:
.L0:
  add   x0, x0, x1
  dmb   ishld
  str   x2, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_int64_indexed_by_int16
    (buf : bytes) (i : int16#) =
  Bytes.unsafe_get_int64_ne_indexed_by_int16 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int16:
  movq  (%rax,%rbx), %rax
  ret
|}, Arm64{|
bytes_get_int64_indexed_by_int16:
.L0:
  add   x0, x0, x1
  ldr   x0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_int64_indexed_by_int16
    (buf : bytes) (i : int16#) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int16 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int16:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int64_indexed_by_int16:
.L0:
  add   x0, x0, x1
  dmb   ishld
  str   x2, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let bytes_get_int64_indexed_by_int8
    (buf : bytes) (i : int8#) =
  Bytes.unsafe_get_int64_ne_indexed_by_int8 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int8:
  movq  (%rax,%rbx), %rax
  ret
|}, Arm64{|
bytes_get_int64_indexed_by_int8:
.L0:
  add   x0, x0, x1
  ldr   x0, [x0, #0]
  ret
.L1:
|}]

let bytes_set_int64_indexed_by_int8
    (buf : bytes) (i : int8#) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int8 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int8:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}, Arm64{|
bytes_set_int64_indexed_by_int8:
.L0:
  add   x0, x0, x1
  dmb   ishld
  str   x2, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let string_get_int8_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_int8_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_int8_indexed_by_int64:
  movsbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
string_get_int8_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldrsb x0, [x0, #0]
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let string_get_int32_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_int32_ne_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_int32_indexed_by_int64:
  movslq (%rax,%rbx), %rax
  ret
|}, Arm64{|
string_get_int32_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldrsw x0, [x0, #0]
  ret
.L1:
|}]

let string_get_int64_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_int64_ne_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_int64_indexed_by_int64:
  movq  (%rax,%rbx), %rax
  ret
|}, Arm64{|
string_get_int64_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldr   x0, [x0, #0]
  ret
.L1:
|}]

let string_get_float32_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_float32_ne_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_float32_indexed_by_int64:
  vmovss (%rax,%rbx), %xmm0
  ret
|}, Arm64{|
string_get_float32_indexed_by_int64:
.L0:
  add   x0, x0, x1
  ldr   s0, [x0, #0]
  ret
.L1:
|}]

(* CR ttebbi: We convert the loaded char to int before comparing against a
   constant, this could be a comparison against an untagged constant instead. *)
let string_unsafe_get_and_use (t : string) : bool =
    let first_char = String.unsafe_get t 0 in
    first_char == 'A'
[%%expect_asm X86_64{|
string_unsafe_get_and_use:
  movzbq (%rax), %rax
  leaq  1(%rax,%rax), %rax
  cmpq  $131, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
string_unsafe_get_and_use:
.L0:
  ldrb  w0, [x0, #0]
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  subs  xzr, x0, #131
  csinc x0, xzr, xzr, ne
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let str_length (s : string) = String.length s
[%%expect_asm X86_64{|
str_length:
  movq  -8(%rax), %rbx
  shrq  $10, %rbx
  leaq  -1(,%rbx,8), %rbx
  movzbq (%rax,%rbx), %rax
  subq  %rax, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}, Arm64{|
str_length:
.L0:
  ldr   x1, [x0, #-8]
  ubfm  x1, x1, #10, #63
  ubfm  x1, x1, #61, #60
  sub   x1, x1, #1
  add   x0, x0, x1
  ldrb  w0, [x0, #0]
  sub   x0, x1, x0
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

let buf_length (b : bytes) = Bytes.length b
[%%expect_asm X86_64{|
buf_length:
  movq  -8(%rax), %rbx
  shrq  $10, %rbx
  leaq  -1(,%rbx,8), %rbx
  movzbq (%rax,%rbx), %rax
  subq  %rax, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}, Arm64{|
buf_length:
.L0:
  ldr   x1, [x0, #-8]
  ubfm  x1, x1, #10, #63
  ubfm  x1, x1, #61, #60
  sub   x1, x1, #1
  add   x0, x0, x1
  ldrb  w0, [x0, #0]
  sub   x0, x1, x0
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]


let u8_to_int_unsafe_set (x : bytes) (y : Uint8_u.t) =
  Bytes.unsafe_set x 0 (Uint8_u.to_int y)
[%%expect_asm X86_64{|
u8_to_int_unsafe_set:
  movb  %bl, (%rax)
  movl  $1, %eax
  ret
|}, Arm64{|
u8_to_int_unsafe_set:
.L0:
  strb  w1, [x0, #0]
  orr   x0, xzr, #1
  ret
.L1:
|}]

let unsafe_get_u8_to_int (x : bytes) =
  Uint8_u.to_int (Bytes.unsafe_get_int8_u_indexed_by_int64 x #0L)
[%%expect_asm X86_64{|
unsafe_get_u8_to_int:
  movzbq (%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
unsafe_get_u8_to_int:
.L0:
  ldrb  w0, [x0, #0]
.L1:
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L2:
|}]
