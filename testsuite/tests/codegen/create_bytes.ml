(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 stack-allocation;
 expect.opt;
*)

(* [Bytes.create] creates strings with uninitialized contents by calling the
   runtime. Small strings of known length are allocated inline instead, with
   their size, header and last word computed at compile time. *)

let unknown_length n = Bytes.create n
[%%expect_asm X86_64{|
unknown_length:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  caml_create_bytes@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

(* The last word is zero apart from its final byte, which counts the bytes
   between it and the end of the string. *)
let known_length () = Bytes.create 5
[%%expect_asm X86_64{|
known_length:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1276, -8(%rax)
  movabsq $144115188075855872, %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
|}]

(* Unlike empty arrays, empty strings take a word, for their final byte. *)
let known_empty () = Bytes.create 0
[%%expect_asm X86_64{|
known_empty:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1276, -8(%rax)
  movabsq $504403158265495552, %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
|}]

(* The runtime raises the exception for invalid lengths. *)
let known_invalid_length () = Bytes.create (-1)
[%%expect_asm X86_64{|
known_invalid_length:
  subq  $8, %rsp
  movq  $-1, %rdi
  movq  caml_create_bytes@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

(* Strings of more than [Config.max_young_wosize] words are created by the
   runtime, even when their length is known. *)
let known_length_too_big () = Bytes.create 2048
[%%expect_asm X86_64{|
known_length_too_big:
  subq  $8, %rsp
  movl  $4097, %edi
  movq  caml_create_bytes@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

let local_unknown_length n = exclave_ Bytes.create__stack n
[%%expect_asm X86_64{|
local_unknown_length:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  caml_create_local_bytes@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

let local_known_length () = exclave_ Bytes.create__stack 5
[%%expect_asm X86_64{|
local_known_length:
  subq  $8, %rsp
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $2044, -8(%rax)
  movabsq $144115188075855872, %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
|}]

(* Strings of known length are allocated like other local blocks, so their
   allocations can be combined. *)
let two_local_known_lengths () =
  exclave_ #(Bytes.create__stack 3, Bytes.create__stack 12)
[%%expect_asm X86_64{|
two_local_known_lengths:
  subq  $8, %rsp
  movq  64(%r14), %rbx
  subq  $40, %rbx
  movq  %rbx, 64(%r14)
  cmpq  80(%r14), %rbx
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rbx
  addq  $8, %rbx
  addq  $16, %rbx
  movq  $3068, -8(%rbx)
  movabsq $216172782113783808, %rax
  movq  %rax, 8(%rbx)
  leaq  -16(%rbx), %rax
  movq  $2044, -8(%rax)
  movabsq $288230376151711744, %rdi
  movq  %rdi, (%rax)
  addq  $8, %rsp
  ret
|}]

(* The C API functions take untagged lengths. *)
external alloc_string : (int[@untagged]) -> bytes
  = "caml_no_bytecode_impl" "caml_alloc_string"

external alloc_local_string : (int[@untagged]) -> bytes @ local
  = "caml_no_bytecode_impl" "caml_alloc_local_string"

let c_api_unknown_length n = alloc_string n
[%%expect_asm X86_64{|
c_api_unknown_length:
  subq  $8, %rsp
  movq  %rax, %rdi
  sarq  $1, %rdi
  movq  caml_alloc_string@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

let c_api_known_length () = alloc_string 5
[%%expect_asm X86_64{|
c_api_known_length:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1276, -8(%rax)
  movabsq $144115188075855872, %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
|}]

let c_api_local_known_length () = exclave_ alloc_local_string 5
[%%expect_asm X86_64{|
c_api_local_known_length:
  subq  $8, %rsp
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $2044, -8(%rax)
  movabsq $144115188075855872, %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
|}]

(* Flambda knows the length of the strings that the runtime functions create for
   known lengths, so it can remove bounds checks. *)

let known_length_no_bounds_check () =
  let b = Bytes.create 5 in
  Bytes.set b 4 'a';
  b
[%%expect_asm X86_64{|
known_length_no_bounds_check:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1276, -8(%rax)
  movabsq $144115188075855872, %rbx
  movq  %rbx, (%rax)
  movl  $97, %ebx
  movb  %bl, 4(%rax)
  addq  $8, %rsp
  ret
|}]

external set_local : bytes @ local -> int -> char -> unit = "%bytes_safe_set"

let local_known_length_no_bounds_check () =
  exclave_
  let b = Bytes.create__stack 5 in
  set_local b 4 'a';
  b
[%%expect_asm X86_64{|
local_known_length_no_bounds_check:
  subq  $8, %rsp
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $2044, -8(%rax)
  movabsq $144115188075855872, %rbx
  movq  %rbx, (%rax)
  movl  $97, %ebx
  movb  %bl, 4(%rax)
  addq  $8, %rsp
  ret
|}]

let c_api_known_length_no_bounds_check () =
  let b = alloc_string 5 in
  Bytes.set b 4 'a';
  b
[%%expect_asm X86_64{|
c_api_known_length_no_bounds_check:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1276, -8(%rax)
  movabsq $144115188075855872, %rbx
  movq  %rbx, (%rax)
  movl  $97, %ebx
  movb  %bl, 4(%rax)
  addq  $8, %rsp
  ret
|}]
