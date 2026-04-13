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

let bytes_get_uint8 (buf : bytes) (i : int) =
  Bytes.unsafe_get buf i
[%%expect_asm X86_64{|
bytes_get_uint8:
  sarq  $1, %rbx
  movzbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
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
|}]


let bytes_get_int8 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int8 buf i
[%%expect_asm X86_64{|
bytes_get_int8:
  sarq  $1, %rbx
  movsbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let bytes_get_uint16 (buf : bytes) (i : int) =
  Bytes.unsafe_get_uint16_ne buf i
[%%expect_asm X86_64{|
bytes_get_uint16:
  sarq  $1, %rbx
  movzwq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
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
|}]

let bytes_get_int16 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int16_ne buf i
[%%expect_asm X86_64{|
bytes_get_int16:
  sarq  $1, %rbx
  movswq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let bytes_get_int32 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int32_ne buf i
[%%expect_asm X86_64{|
bytes_get_int32:
  sarq  $1, %rbx
  movslq (%rax,%rbx), %rax
  ret
|}]

let bytes_set_int32 (buf : bytes) (i : int) (v : Int32_u.t) =
  Bytes.unsafe_set_int32_ne buf i v
[%%expect_asm X86_64{|
bytes_set_int32:
  sarq  $1, %rbx
  movl  %edi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_get_int64 (buf : bytes) (i : int) =
  Bytes.unsafe_get_int64_ne buf i
[%%expect_asm X86_64{|
bytes_get_int64:
  sarq  $1, %rbx
  movq  (%rax,%rbx), %rax
  ret
|}]

let bytes_set_int64 (buf : bytes) (i : int) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne buf i v
[%%expect_asm X86_64{|
bytes_set_int64:
  sarq  $1, %rbx
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_get_float32 (buf : bytes) (i : int) =
  Bytes.unsafe_get_float32_ne buf i
[%%expect_asm X86_64{|
bytes_get_float32:
  sarq  $1, %rbx
  vmovss (%rax,%rbx), %xmm0
  ret
|}]

let bytes_set_float32 (buf : bytes) (i : int) (v : Float32_u.t) =
  Bytes.unsafe_set_float32_ne buf i v
[%%expect_asm X86_64{|
bytes_set_float32:
  sarq  $1, %rbx
  vmovss %xmm0, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let string_get_int64 (s : string) (i : int) =
  String.unsafe_get_int64_ne s i
[%%expect_asm X86_64{|
string_get_int64:
  sarq  $1, %rbx
  movq  (%rax,%rbx), %rax
  ret
|}]

let string_get_float32 (s : string) (i : int) =
  String.unsafe_get_float32_ne s i
[%%expect_asm X86_64{|
string_get_float32:
  sarq  $1, %rbx
  vmovss (%rax,%rbx), %xmm0
  ret
|}]

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
|}]

let bytes_get_int64_bswap (buf : bytes) (i : int) =
  Int64_u.bswap (Bytes.unsafe_get_int64_ne buf i)
[%%expect_asm X86_64{|
bytes_get_int64_bswap:
  sarq  $1, %rbx
  movq  (%rax,%rbx), %rax
  bswap %rax
  ret
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
  salq  $8, %rdi
  shrq  $18, %rdi
  leaq  -1(,%rdi,8), %rdi
  movzbq (%rax,%rdi), %rsi
  subq  %rsi, %rdi
  addq  $-3, %rdi
  sarq  $1, %rbx
  movq  %rdi, %rsi
  sarq  $63, %rsi
  xorq  $-1, %rsi
  andq  %rdi, %rsi
  cmpq  %rsi, %rbx
  jae   .L123
  movslq (%rax,%rbx), %rax
  ret
.L123:
  movq  camlTOP18__block602@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let bytes_get_int64_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_int64_ne_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int64:
  movq  (%rax,%rbx), %rax
  ret
|}]

let bytes_set_int64_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int64 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int64:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_get_int8_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_int8_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_int8_indexed_by_int64:
  movsbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
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
|}]

let bytes_get_int32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_int32_ne_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_int32_indexed_by_int64:
  movslq (%rax,%rbx), %rax
  ret
|}]

let bytes_set_int32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) (v : Int32_u.t) =
  Bytes.unsafe_set_int32_ne_indexed_by_int64 buf i v
[%%expect_asm X86_64{|
bytes_set_int32_indexed_by_int64:
  movl  %edi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_get_float32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) =
  Bytes.unsafe_get_float32_ne_indexed_by_int64 buf i
[%%expect_asm X86_64{|
bytes_get_float32_indexed_by_int64:
  vmovss (%rax,%rbx), %xmm0
  ret
|}]

let bytes_set_float32_indexed_by_int64
    (buf : bytes) (i : Int64_u.t) (v : Float32_u.t) =
  Bytes.unsafe_set_float32_ne_indexed_by_int64 buf i v
[%%expect_asm X86_64{|
bytes_set_float32_indexed_by_int64:
  vmovss %xmm0, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_get_int64_indexed_by_int32
    (buf : bytes) (i : Int32_u.t) =
  Bytes.unsafe_get_int64_ne_indexed_by_int32 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int32:
  movq  (%rax,%rbx), %rax
  ret
|}]

let bytes_set_int64_indexed_by_int32
    (buf : bytes) (i : Int32_u.t) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int32 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int32:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_get_int64_indexed_by_int16
    (buf : bytes) (i : int16#) =
  Bytes.unsafe_get_int64_ne_indexed_by_int16 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int16:
  movq  (%rax,%rbx), %rax
  ret
|}]

let bytes_set_int64_indexed_by_int16
    (buf : bytes) (i : int16#) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int16 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int16:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_get_int64_indexed_by_int8
    (buf : bytes) (i : int8#) =
  Bytes.unsafe_get_int64_ne_indexed_by_int8 buf i
[%%expect_asm X86_64{|
bytes_get_int64_indexed_by_int8:
  movq  (%rax,%rbx), %rax
  ret
|}]

let bytes_set_int64_indexed_by_int8
    (buf : bytes) (i : int8#) (v : Int64_u.t) =
  Bytes.unsafe_set_int64_ne_indexed_by_int8 buf i v
[%%expect_asm X86_64{|
bytes_set_int64_indexed_by_int8:
  movq  %rdi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let string_get_int8_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_int8_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_int8_indexed_by_int64:
  movsbq (%rax,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let string_get_int32_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_int32_ne_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_int32_indexed_by_int64:
  movslq (%rax,%rbx), %rax
  ret
|}]

let string_get_int64_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_int64_ne_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_int64_indexed_by_int64:
  movq  (%rax,%rbx), %rax
  ret
|}]

let string_get_float32_indexed_by_int64
    (s : string) (i : Int64_u.t) =
  String.unsafe_get_float32_ne_indexed_by_int64 s i
[%%expect_asm X86_64{|
string_get_float32_indexed_by_int64:
  vmovss (%rax,%rbx), %xmm0
  ret
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
|}]

let str_length (s : string) = String.length s
[%%expect_asm X86_64{|
str_length:
  movq  -8(%rax), %rbx
  salq  $8, %rbx
  shrq  $18, %rbx
  leaq  -1(,%rbx,8), %rbx
  movzbq (%rax,%rbx), %rax
  subq  %rax, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

let buf_length (b : bytes) = Bytes.length b
[%%expect_asm X86_64{|
buf_length:
  movq  -8(%rax), %rbx
  salq  $8, %rbx
  shrq  $18, %rbx
  leaq  -1(,%rbx,8), %rbx
  movzbq (%rax,%rbx), %rax
  subq  %rax, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]
