(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 stack-allocation;
 expect.opt;
*)

(* [%makearray_dynamic_uninit] creates arrays whose elements are not scanned by
   the GC by calling the runtime. Small arrays of known length are allocated
   inline instead, with their size and header computed at compile time. *)

external[@layout_poly] make_local :
  ('a : any mod separable). int -> 'a array @ local
  = "%makearray_dynamic_uninit"

external[@layout_poly] make_heap :
  ('a : any mod separable). int -> 'a array = "%makearray_dynamic_uninit"

let unknown_length n : float# array = exclave_ make_local n
[%%expect_asm X86_64{|
unknown_length:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  caml_make_local_unboxed_float64_vect@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

let known_length () : float# array = exclave_ make_local 3
[%%expect_asm X86_64{|
known_length:
  subq  $8, %rsp
  movq  64(%r14), %rax
  subq  $32, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $4094, -8(%rax)
  addq  $8, %rsp
  ret
|}]

let known_empty () : float# array = exclave_ make_local 0
[%%expect_asm X86_64{|
known_empty:
  movq  caml_atom_0@GOTPCREL(%rip), %rax
  addq  $8, %rax
  ret
|}]

(* The runtime raises the exception for invalid lengths. *)
let known_invalid_length () : float# array = exclave_ make_local (-1)
[%%expect_asm X86_64{|
known_invalid_length:
  subq  $8, %rsp
  movq  $-1, %rdi
  movq  caml_make_local_unboxed_float64_vect@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

(* [int8#] arrays pack eight elements into each word, and their tag records how
   many elements of the last word are unused. *)
let int8_known_length () : int8# array = exclave_ make_local 5
[%%expect_asm X86_64{|
int8_known_length:
  subq  $8, %rsp
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rax
  addq  $8, %rax
  movabsq $72057594037929747, %rbx
  movq  %rbx, -8(%rax)
  addq  $8, %rsp
  ret
|}]

let int8_known_invalid_length () : int8# array = exclave_ make_local (-1)
[%%expect_asm X86_64{|
int8_known_invalid_length:
  subq  $8, %rsp
  movq  $-1, %rdi
  movq  caml_make_local_untagged_int8_vect@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

(* Each element of this unboxed product array takes two words. *)
let product_known_length () : #(float# * int64_u) array = exclave_ make_local 3
[%%expect_asm X86_64{|
product_known_length:
  subq  $8, %rsp
  movq  64(%r14), %rax
  subq  $56, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rax
  addq  $8, %rax
  movabsq $72057594037934848, %rbx
  movq  %rbx, -8(%rax)
  addq  $8, %rsp
  ret
|}]

(* Arrays of known length are allocated like other local blocks, so their
   allocations can be combined. *)
let two_known_lengths () : #(float# array * int8# array) =
  exclave_ #(make_local 2, make_local 5)
[%%expect_asm X86_64{|
two_known_lengths:
  subq  $8, %rsp
  movq  64(%r14), %rbx
  subq  $40, %rbx
  movq  %rbx, 64(%r14)
  cmpq  80(%r14), %rbx
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rbx
  addq  $8, %rbx
  addq  $24, %rbx
  movabsq $72057594037929747, %rax
  movq  %rax, -8(%rbx)
  leaq  -24(%rbx), %rax
  movq  $3070, -8(%rax)
  addq  $8, %rsp
  ret
|}]

(* Arrays of more than [Config.max_young_wosize] words are created by the
   runtime, even when their length is known. *)
let known_length_too_big () : float# array = exclave_ make_local 257
[%%expect_asm X86_64{|
known_length_too_big:
  subq  $8, %rsp
  movl  $515, %edi
  movq  caml_make_local_unboxed_float64_vect@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

let heap_unknown_length n : float# array = make_heap n
[%%expect_asm X86_64{|
heap_unknown_length:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  caml_make_unboxed_float64_vect@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

(* Small heap arrays of known length are allocated inline on the minor heap. *)
let heap_known_length () : float# array = make_heap 3
[%%expect_asm X86_64{|
heap_known_length:
  subq  $8, %rsp
  subq  $32, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $3326, -8(%rax)
  addq  $8, %rsp
  ret
|}]

let heap_known_length_too_big () : float# array = make_heap 257
[%%expect_asm X86_64{|
heap_known_length_too_big:
  subq  $8, %rsp
  movl  $515, %edi
  movq  caml_make_unboxed_float64_vect@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  addq  $8, %rsp
  ret
|}]

(* Flambda knows the length of the array created by the runtime, so it can
   remove bounds checks and fold [Array.length]. *)

external[@layout_poly] length :
  ('a : any mod separable). 'a array @ local -> int = "%array_length"

external[@layout_poly] set :
  ('a : any mod separable). 'a array @ local -> int -> 'a -> unit
  = "%array_safe_set"

let known_length_no_bounds_check () : int32_u array =
  let t = make_heap 1 in
  set t 0 #1l;
  t
[%%expect_asm X86_64{|
known_length_no_bounds_check:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movabsq $72057594037928963, %rbx
  movq  %rbx, -8(%rax)
  movl  $1, %ebx
  movl  %ebx, (%rax)
  addq  $8, %rsp
  ret
|}]

let unknown_length_array_length n =
  let t : int32_u array = make_heap n in
  length t
[%%expect_asm X86_64{|
unknown_length_array_length:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rdi, (%rsp)
  movq  caml_make_unboxed_int32_vect@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  movq  (%rsp), %rax
  addq  $8, %rsp
  ret
|}]

(* [%makearray_dynamic] creates local arrays of values by calling
   [caml_array_make_local]. *)

external[@layout_poly] make_local_with_init :
  ('a : any mod separable). int -> 'a -> 'a array @ local
  = "%makearray_dynamic"

let local_values_known_length_no_bounds_check () : int array =
  exclave_
  let t = make_local_with_init 1 0 in
  set t 0 1;
  t
[%%expect_asm X86_64{|
local_values_known_length_no_bounds_check:
  subq  $8, %rsp
  movl  $1, %esi
  movl  $3, %edi
  movq  caml_array_make_local@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  movq  $3, (%rax)
  addq  $8, %rsp
  ret
|}]
