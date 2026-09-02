(* TEST
 include stdlib_stable;
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* Codegen tests for atomic operations through block indices, block pointers
   and external pointers. *)

open Stdlib_stable

type t =
  { mutable x : int [@atomic]
  ; mutable y : int [@atomic]
  }

(* Block indices *)

let idx_get t = Idx_atomic.get t (.y)
[%%expect_asm X86_64{|
idx_get:
  movq  8(%rax), %rax
  ret
|}]

let idx_set t v = Idx_atomic.set t (.y) v
[%%expect_asm X86_64{|
idx_set:
  xchg  %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let idx_exchange t v = Idx_atomic.exchange t (.y) v
[%%expect_asm X86_64{|
idx_exchange:
  movq  %rax, %rdi
  movq  %rbx, %rax
  xchg  %rax, 8(%rdi)
  ret
|}]

let idx_compare_and_set t old new_ = Idx_atomic.compare_and_set t (.y) old new_
[%%expect_asm X86_64{|
idx_compare_and_set:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, 8(%rsi)
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let idx_compare_exchange t old new_ =
  Idx_atomic.compare_exchange t (.y) old new_
[%%expect_asm X86_64{|
idx_compare_exchange:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, 8(%rsi)
  ret
|}]

let idx_fetch_and_add t n = Idx_atomic.fetch_and_add t (.y) n
[%%expect_asm X86_64{|
idx_fetch_and_add:
  movq  %rax, %rdi
  leaq  -1(%rbx), %rax
  lock xaddq %rax, 8(%rdi)
  ret
|}]

let idx_add t n = Idx_atomic.add t (.y) n
[%%expect_asm X86_64{|
idx_add:
  decq  %rbx
  lock addq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let idx_sub t n = Idx_atomic.sub t (.y) n
[%%expect_asm X86_64{|
idx_sub:
  decq  %rbx
  lock subq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let idx_logand t n = Idx_atomic.logand t (.y) n
[%%expect_asm X86_64{|
idx_logand:
  lock andq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let idx_logor t n = Idx_atomic.logor t (.y) n
[%%expect_asm X86_64{|
idx_logor:
  lock orq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let idx_logxor t n = Idx_atomic.logxor t (.y) n
[%%expect_asm X86_64{|
idx_logxor:
  decq  %rbx
  lock xorq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

(* Block pointers *)

module Ptr_atomic = struct
  type ('a : value_or_null, 'b : value_or_null) t =
    #('a * ('a, 'b) idx_atomic)

  external get :
    ('a : value_or_null) ('b : value_or_null).
    ('a, 'b) t @ local -> 'b = "%unsafe_atomic_load_ptr"

  external set :
    ('a : value_or_null) ('b : value_or_null).
    (('a, 'b) t[@local_opt]) -> 'b -> unit = "%unsafe_atomic_set_ptr"

  external exchange :
    ('a : value_or_null) ('b : value_or_null).
    (('a, 'b) t[@local_opt]) -> 'b -> 'b = "%unsafe_atomic_exchange_ptr"

  external compare_and_set :
    ('a : value_or_null) ('b : value_or_null).
    (('a, 'b) t[@local_opt]) -> 'b -> 'b -> bool = "%unsafe_atomic_cas_ptr"

  external compare_exchange :
    ('a : value_or_null) ('b : value_or_null).
    (('a, 'b) t[@local_opt]) -> 'b -> 'b -> 'b
    = "%unsafe_atomic_compare_exchange_ptr"

  external fetch_and_add :
    ('a : value_or_null). ('a, int) t @ local -> int -> int
    = "%unsafe_atomic_fetch_add_ptr"

  external add :
    ('a : value_or_null). ('a, int) t @ local -> int -> unit
    = "%unsafe_atomic_add_ptr"

  external sub :
    ('a : value_or_null). ('a, int) t @ local -> int -> unit
    = "%unsafe_atomic_sub_ptr"

  external logand :
    ('a : value_or_null). ('a, int) t @ local -> int -> unit
    = "%unsafe_atomic_land_ptr"

  external logor :
    ('a : value_or_null). ('a, int) t @ local -> int -> unit
    = "%unsafe_atomic_lor_ptr"

  external logxor :
    ('a : value_or_null). ('a, int) t @ local -> int -> unit
    = "%unsafe_atomic_lxor_ptr"
end

let ptr_get t = Ptr_atomic.get #(t, (.y))
[%%expect_asm X86_64{|
ptr_get:
  movq  8(%rax), %rax
  ret
|}]

let ptr_set t v = Ptr_atomic.set #(t, (.y)) v
[%%expect_asm X86_64{|
ptr_set:
  xchg  %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let ptr_exchange t v = Ptr_atomic.exchange #(t, (.y)) v
[%%expect_asm X86_64{|
ptr_exchange:
  movq  %rax, %rdi
  movq  %rbx, %rax
  xchg  %rax, 8(%rdi)
  ret
|}]

let ptr_compare_and_set t old new_ =
  Ptr_atomic.compare_and_set #(t, (.y)) old new_
[%%expect_asm X86_64{|
ptr_compare_and_set:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, 8(%rsi)
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let ptr_compare_exchange t old new_ =
  Ptr_atomic.compare_exchange #(t, (.y)) old new_
[%%expect_asm X86_64{|
ptr_compare_exchange:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, 8(%rsi)
  ret
|}]

let ptr_fetch_and_add t n = Ptr_atomic.fetch_and_add #(t, (.y)) n
[%%expect_asm X86_64{|
ptr_fetch_and_add:
  movq  %rax, %rdi
  leaq  -1(%rbx), %rax
  lock xaddq %rax, 8(%rdi)
  ret
|}]

let ptr_add t n = Ptr_atomic.add #(t, (.y)) n
[%%expect_asm X86_64{|
ptr_add:
  decq  %rbx
  lock addq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let ptr_sub t n = Ptr_atomic.sub #(t, (.y)) n
[%%expect_asm X86_64{|
ptr_sub:
  decq  %rbx
  lock subq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let ptr_logand t n = Ptr_atomic.logand #(t, (.y)) n
[%%expect_asm X86_64{|
ptr_logand:
  lock andq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let ptr_logor t n = Ptr_atomic.logor #(t, (.y)) n
[%%expect_asm X86_64{|
ptr_logor:
  lock orq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let ptr_logxor t n = Ptr_atomic.logxor #(t, (.y)) n
[%%expect_asm X86_64{|
ptr_logxor:
  decq  %rbx
  lock xorq %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

(* External pointers *)

module Ext_ptr_atomic = struct
  type t = int64#

  external get :
    ('a : value_or_null). t @ local -> 'a = "%unsafe_atomic_load_ext_ptr"

  external set :
    ('a : value_or_null). (t[@local_opt]) -> 'a -> unit
    = "%unsafe_atomic_set_ext_ptr"

  external exchange :
    ('a : value_or_null). (t[@local_opt]) -> 'a -> 'a
    = "%unsafe_atomic_exchange_ext_ptr"

  external compare_and_set :
    ('a : value_or_null). (t[@local_opt]) -> 'a -> 'a -> bool
    = "%unsafe_atomic_cas_ext_ptr"

  external compare_exchange :
    ('a : value_or_null). (t[@local_opt]) -> 'a -> 'a -> 'a
    = "%unsafe_atomic_compare_exchange_ext_ptr"

  external fetch_and_add : t @ local -> int -> int
    = "%unsafe_atomic_fetch_add_ext_ptr"

  external add : t @ local -> int -> unit = "%unsafe_atomic_add_ext_ptr"
  external sub : t @ local -> int -> unit = "%unsafe_atomic_sub_ext_ptr"
  external logand : t @ local -> int -> unit = "%unsafe_atomic_land_ext_ptr"
  external logor : t @ local -> int -> unit = "%unsafe_atomic_lor_ext_ptr"
  external logxor : t @ local -> int -> unit = "%unsafe_atomic_lxor_ext_ptr"
end

let ext_ptr_get (p : Ext_ptr_atomic.t) : int = Ext_ptr_atomic.get p
[%%expect_asm X86_64{|
ext_ptr_get:
  movq  (%rax), %rax
  ret
|}]

let ext_ptr_set (p : Ext_ptr_atomic.t) (v : int) = Ext_ptr_atomic.set p v
[%%expect_asm X86_64{|
ext_ptr_set:
  xchg  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let ext_ptr_exchange (p : Ext_ptr_atomic.t) (v : int) =
  Ext_ptr_atomic.exchange p v
[%%expect_asm X86_64{|
ext_ptr_exchange:
  movq  %rax, %rdi
  movq  %rbx, %rax
  xchg  %rax, (%rdi)
  ret
|}]

let ext_ptr_compare_and_set (p : Ext_ptr_atomic.t) (old : int) new_ =
  Ext_ptr_atomic.compare_and_set p old new_
[%%expect_asm X86_64{|
ext_ptr_compare_and_set:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, (%rsi)
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let ext_ptr_compare_exchange (p : Ext_ptr_atomic.t) (old : int) new_ =
  Ext_ptr_atomic.compare_exchange p old new_
[%%expect_asm X86_64{|
ext_ptr_compare_exchange:
  movq  %rax, %rsi
  movq  %rbx, %rax
  lock cmpxchgq %rdi, (%rsi)
  ret
|}]

let ext_ptr_fetch_and_add (p : Ext_ptr_atomic.t) n =
  Ext_ptr_atomic.fetch_and_add p n
[%%expect_asm X86_64{|
ext_ptr_fetch_and_add:
  movq  %rax, %rdi
  leaq  -1(%rbx), %rax
  lock xaddq %rax, (%rdi)
  ret
|}]

let ext_ptr_add (p : Ext_ptr_atomic.t) n = Ext_ptr_atomic.add p n
[%%expect_asm X86_64{|
ext_ptr_add:
  decq  %rbx
  lock addq %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let ext_ptr_sub (p : Ext_ptr_atomic.t) n = Ext_ptr_atomic.sub p n
[%%expect_asm X86_64{|
ext_ptr_sub:
  decq  %rbx
  lock subq %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let ext_ptr_logand (p : Ext_ptr_atomic.t) n = Ext_ptr_atomic.logand p n
[%%expect_asm X86_64{|
ext_ptr_logand:
  lock andq %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let ext_ptr_logor (p : Ext_ptr_atomic.t) n = Ext_ptr_atomic.logor p n
[%%expect_asm X86_64{|
ext_ptr_logor:
  lock orq %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let ext_ptr_logxor (p : Ext_ptr_atomic.t) n = Ext_ptr_atomic.logxor p n
[%%expect_asm X86_64{|
ext_ptr_logxor:
  decq  %rbx
  lock xorq %rbx, (%rax)
  movl  $1, %eax
  ret
|}]
