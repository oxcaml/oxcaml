(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 expect.opt;
*)

open Intrinsics

(* Count leading zeros - int *)

let clz_tagged x = Builtins.int_clz x
[%%expect_asm X86_64{|
clz_tagged:
  lzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Count leading zeros - int64 *)

let clz64 x = Builtins.int64_clz (Int64_u.to_int64 x)
[%%expect_asm X86_64{|
clz64:
  lzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let clz64_nonzero x =
  Builtins.int64_clz_nonzero (Int64_u.to_int64 x)
[%%expect_asm X86_64{|
clz64_nonzero:
  lzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Count leading zeros - int32 *)

(* CR ttebbi: Could use lzcntl directly instead of zero-extend + lzcntq
   + subtract 32. *)
let clz32 x = Builtins.int32_clz (Int32_u.to_int32 x)
[%%expect_asm X86_64{|
clz32:
  movl  %eax, %eax
  lzcnt %rax, %rax
  addq  $-32, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let clz32_nonzero x =
  Builtins.int32_clz_nonzero (Int32_u.to_int32 x)
[%%expect_asm X86_64{|
clz32_nonzero:
  movl  %eax, %eax
  lzcnt %rax, %rax
  addq  $-32, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Count leading zeros - nativeint *)

let clz_native x =
  Builtins.nativeint_clz (Nativeint_u.to_nativeint x)
[%%expect_asm X86_64{|
clz_native:
  lzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let clz_native_nonzero x =
  Builtins.nativeint_clz_nonzero (Nativeint_u.to_nativeint x)
[%%expect_asm X86_64{|
clz_native_nonzero:
  lzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Count trailing zeros - int *)

(* CR ttebbi: We should do tzcnt(x-1)-1 *)
let ctz_int x = Builtins.int_ctz x
[%%expect_asm X86_64{|
ctz_untagged:
  movl  $1, %ebx
  salq  $63, %rbx
  sarq  $1, %rax
  orq   %rbx, %rax
  tzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Count trailing zeros - int64 *)

let ctz64 x = Builtins.int64_ctz (Int64_u.to_int64 x)
[%%expect_asm X86_64{|
ctz64:
  tzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let ctz64_nonzero x =
  Builtins.int64_ctz_nonzero (Int64_u.to_int64 x)
[%%expect_asm X86_64{|
ctz64_nonzero:
  tzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Count trailing zeros - int32 *)

(* CR ttebbi: We should use the 32bit tzcnt instruction. *)
let ctz32 x = Builtins.int32_ctz (Int32_u.to_int32 x)
[%%expect_asm X86_64{|
ctz32:
  movabsq $4294967296, %rbx
  orq   %rbx, %rax
  tzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let ctz32_nonzero x =
  Builtins.int32_ctz_nonzero (Int32_u.to_int32 x)
[%%expect_asm X86_64{|
ctz32_nonzero:
  tzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Count trailing zeros - nativeint *)

let ctz_native x =
  Builtins.nativeint_ctz (Nativeint_u.to_nativeint x)
[%%expect_asm X86_64{|
ctz_native:
  tzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let ctz_native_nonzero x =
  Builtins.nativeint_ctz_nonzero (Nativeint_u.to_nativeint x)
[%%expect_asm X86_64{|
ctz_native_nonzero:
  tzcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Population count - int *)

(* CR ttebbi: The -1 should be folded into the lea. *)
let popcnt_tagged x = Builtins.int_popcnt x
[%%expect_asm X86_64{|
popcnt_tagged:
  popcnt %rax, %rax
  decq  %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Population count - int64 *)

let popcnt64 x = Builtins.int64_popcnt (Int64_u.to_int64 x)
[%%expect_asm X86_64{|
popcnt64:
  popcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Population count - int32 *)

(* CR ttebbi: Could use 32bit popcntl directly. *)
let popcnt32 x = Builtins.int32_popcnt (Int32_u.to_int32 x)
[%%expect_asm X86_64{|
popcnt32:
  movl  %eax, %eax
  popcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* Population count - nativeint *)

let popcnt_native x =
  Builtins.nativeint_popcnt (Nativeint_u.to_nativeint x)
[%%expect_asm X86_64{|
popcnt_native:
  popcnt %rax, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* High multiply *)

let mulhi_signed x y =
  Int64_u.of_int64
    (Builtins.int64_mulhi_s
       (Int64_u.to_int64 x) (Int64_u.to_int64 y))
[%%expect_asm X86_64{|
mulhi_signed:
  imulq %rbx
  movq  %rdx, %rax
  ret
|}]

let mulhi_unsigned x y =
  Int64_u.of_int64
    (Builtins.int64_mulhi_u
       (Int64_u.to_int64 x) (Int64_u.to_int64 y))
[%%expect_asm X86_64{|
mulhi_unsigned:
  mulq  %rbx
  movq  %rdx, %rax
  ret
|}]

(* Prefetch *)

let do_prefetch_read_high x = Builtins.prefetch_read_high x
[%%expect_asm X86_64{|
do_prefetch_read_high:
  prefetcht0 (%rax)
  movl  $1, %eax
  ret
|}]

let do_prefetch_read_moderate x = Builtins.prefetch_read_moderate x
[%%expect_asm X86_64{|
do_prefetch_read_moderate:
  prefetcht1 (%rax)
  movl  $1, %eax
  ret
|}]

let do_prefetch_read_low x = Builtins.prefetch_read_low x
[%%expect_asm X86_64{|
do_prefetch_read_low:
  prefetcht2 (%rax)
  movl  $1, %eax
  ret
|}]

let do_prefetch_read_none x = Builtins.prefetch_read_none x
[%%expect_asm X86_64{|
do_prefetch_read_none:
  prefetchnta (%rax)
  movl  $1, %eax
  ret
|}]

(* CR ttebbi: Should use prefetchw. *)
let do_prefetch_write_high x = Builtins.prefetch_write_high x
[%%expect_asm X86_64{|
do_prefetch_write_high:
  prefetcht0 (%rax)
  movl  $1, %eax
  ret
|}]

let do_prefetch_write_low x = Builtins.prefetch_write_low x
[%%expect_asm X86_64{|
do_prefetch_write_low:
  prefetcht2 (%rax)
  movl  $1, %eax
  ret
|}]

(* Pause *)

let do_pause () = Builtins.pause_hint ()
[%%expect_asm X86_64{|
do_pause:
  movl  $1, %eax
  pause
  ret
|}]

(* Native pointer load/store - int64 *)

let ptr_load_int64 (p : nativeint#) =
  Builtins.native_pointer_load_int64 p
[%%expect_asm X86_64{|
ptr_load_int64:
  movq  (%rax), %rax
  ret
|}]

let ptr_store_int64 (p : nativeint#) (v : int64#) =
  Builtins.native_pointer_store_int64 p v
[%%expect_asm X86_64{|
ptr_store_int64:
  movq  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer load/store - int32 *)

(* CR ttebbi: Double sign extension: movslq from memory, then movslq again. *)
let ptr_load_int32 (p : nativeint#) =
  Builtins.native_pointer_load_int32 p
[%%expect_asm X86_64{|
ptr_load_int32:
  movslq (%rax), %rax
  movslq %eax, %rax
  ret
|}]

let ptr_store_int32 (p : nativeint#) (v : int32#) =
  Builtins.native_pointer_store_int32 p v
[%%expect_asm X86_64{|
ptr_store_int32:
  movl  %ebx, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer load/store - nativeint *)

let ptr_load_nativeint (p : nativeint#) =
  Builtins.native_pointer_load_nativeint p
[%%expect_asm X86_64{|
ptr_load_nativeint:
  movq  (%rax), %rax
  ret
|}]

let ptr_store_nativeint (p : nativeint#) (v : nativeint#) =
  Builtins.native_pointer_store_nativeint p v
[%%expect_asm X86_64{|
ptr_store_nativeint:
  movq  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer load/store - float *)

let ptr_load_float (p : nativeint#) =
  Builtins.native_pointer_load_float p
[%%expect_asm X86_64{|
ptr_load_float:
  vmovsd (%rax), %xmm0
  ret
|}]

let ptr_store_float (p : nativeint#) (v : float#) =
  Builtins.native_pointer_store_float p v
[%%expect_asm X86_64{|
ptr_store_float:
  vmovsd %xmm0, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer load/store - uint8 *)

let ptr_load_uint8 (p : nativeint#) =
  Builtins.native_pointer_load_uint8 p
[%%expect_asm X86_64{|
ptr_load_uint8:
  movzbq (%rax), %rax
  ret
|}]

let ptr_store_uint8 (p : nativeint#) (v : int) =
  Builtins.native_pointer_store_uint8 p v
[%%expect_asm X86_64{|
ptr_store_uint8:
  movb  %bl, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer load/store - sint8 *)

let ptr_load_sint8 (p : nativeint#) =
  Builtins.native_pointer_load_sint8 p
[%%expect_asm X86_64{|
ptr_load_sint8:
  movsbq (%rax), %rax
  ret
|}]

let ptr_store_sint8 (p : nativeint#) (v : int) =
  Builtins.native_pointer_store_sint8 p v
[%%expect_asm X86_64{|
ptr_store_sint8:
  movb  %bl, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer load/store - uint16 *)

let ptr_load_uint16 (p : nativeint#) =
  Builtins.native_pointer_load_uint16 p
[%%expect_asm X86_64{|
ptr_load_uint16:
  movzwq (%rax), %rax
  ret
|}]

let ptr_store_uint16 (p : nativeint#) (v : int) =
  Builtins.native_pointer_store_uint16 p v
[%%expect_asm X86_64{|
ptr_store_uint16:
  movw  %bx, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer load/store - sint16 *)

let ptr_load_sint16 (p : nativeint#) =
  Builtins.native_pointer_load_sint16 p
[%%expect_asm X86_64{|
ptr_load_sint16:
  movswq (%rax), %rax
  ret
|}]

let ptr_store_sint16 (p : nativeint#) (v : int) =
  Builtins.native_pointer_store_sint16 p v
[%%expect_asm X86_64{|
ptr_store_sint16:
  movw  %bx, (%rax)
  movl  $1, %eax
  ret
|}]

(* Native pointer atomics - int *)

let ptr_cas_int (p : nativeint#) old_v new_v =
  Builtins.native_pointer_cas_int p old_v new_v
[%%expect_asm X86_64{|
ptr_cas_int:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, (%rsi)
  sete  %al
  movzbq %al, %rax
  salq  $1, %rax
  incq  %rax
  ret
|}]

let ptr_fetch_add_int (p : nativeint#) v =
  Builtins.native_pointer_fetch_add_int p v
[%%expect_asm X86_64{|
ptr_fetch_add_int:
  movq  %rax, %rdi
  movq  %rbx, %rax
  lock xaddq %rax, (%rdi)
  ret
|}]

let ptr_fetch_sub_int (p : nativeint#) v =
  Builtins.native_pointer_fetch_sub_int p v
[%%expect_asm X86_64{|
ptr_fetch_sub_int:
  movq  %rax, %rdi
  xorl  %eax, %eax
  subq  %rbx, %rax
  lock xaddq %rax, (%rdi)
  ret
|}]

(* Native pointer atomics - int64 *)

let ptr_cas_int64 (p : nativeint#) (old_v : int64#) (new_v : int64#) =
  Builtins.native_pointer_cas_int64 p old_v new_v
[%%expect_asm X86_64{|
ptr_cas_int64:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, (%rsi)
  sete  %al
  movzbq %al, %rax
  salq  $1, %rax
  incq  %rax
  ret
|}]

let ptr_fetch_add_int64 (p : nativeint#) (v : int64#) =
  Builtins.native_pointer_fetch_add_int64 p v
[%%expect_asm X86_64{|
ptr_fetch_add_int64:
  movq  %rax, %rdi
  movq  %rbx, %rax
  lock xaddq %rax, (%rdi)
  ret
|}]

let ptr_fetch_sub_int64 (p : nativeint#) (v : int64#) =
  Builtins.native_pointer_fetch_sub_int64 p v
[%%expect_asm X86_64{|
ptr_fetch_sub_int64:
  movq  %rax, %rdi
  xorl  %eax, %eax
  subq  %rbx, %rax
  lock xaddq %rax, (%rdi)
  ret
|}]

(* Native pointer atomics - int32 *)

let ptr_cas_int32 (p : nativeint#) (old_v : int32#) (new_v : int32#) =
  Builtins.native_pointer_cas_int32 p old_v new_v
[%%expect_asm X86_64{|
ptr_cas_int32:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgl %edi, (%rsi)
  sete  %al
  movzbq %al, %rax
  salq  $1, %rax
  incq  %rax
  ret
|}]

let ptr_fetch_add_int32 (p : nativeint#) (v : int32#) =
  Builtins.native_pointer_fetch_add_int32 p v
[%%expect_asm X86_64{|
ptr_fetch_add_int32:
  lock xaddl %ebx, (%rax)
  movslq %ebx, %rax
  ret
|}]

(* CR ttebbi: We should use neg instead of subtracting from zero. *)
let ptr_fetch_sub_int32 (p : nativeint#) (v : int32#) =
  Builtins.native_pointer_fetch_sub_int32 p v
[%%expect_asm X86_64{|
ptr_fetch_sub_int32:
  xorl  %edi, %edi
  subq  %rbx, %rdi
  lock xaddl %edi, (%rax)
  movslq %edi, %rax
  ret
|}]

(* Native pointer atomics - nativeint *)

let ptr_cas_nativeint (p : nativeint#)
    (old_v : nativeint#) (new_v : nativeint#) =
  Builtins.native_pointer_cas_nativeint p old_v new_v
[%%expect_asm X86_64{|
ptr_cas_nativeint:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, (%rsi)
  sete  %al
  movzbq %al, %rax
  salq  $1, %rax
  incq  %rax
  ret
|}]

let ptr_fetch_add_nativeint (p : nativeint#) (v : nativeint#) =
  Builtins.native_pointer_fetch_add_nativeint p v
[%%expect_asm X86_64{|
ptr_fetch_add_nativeint:
  movq  %rax, %rdi
  movq  %rbx, %rax
  lock xaddq %rax, (%rdi)
  ret
|}]

let ptr_fetch_sub_nativeint (p : nativeint#) (v : nativeint#) =
  Builtins.native_pointer_fetch_sub_nativeint p v
[%%expect_asm X86_64{|
ptr_fetch_sub_nativeint:
  movq  %rax, %rdi
  xorl  %eax, %eax
  subq  %rbx, %rax
  lock xaddq %rax, (%rdi)
  ret
|}]
