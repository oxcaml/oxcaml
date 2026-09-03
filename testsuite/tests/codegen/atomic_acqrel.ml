(* TEST
 only-default-codegen;
 expect.opt;
*)

(* Acquire loads and release stores are plain loads and stores on x86, unlike
   the sequentially consistent [Atomic.get] and [Atomic.set]. *)

external get_acquire : 'a Atomic.t -> 'a = "%atomic_load_acquire"
external set_release : 'a Atomic.t -> 'a -> unit = "%atomic_store_release"
external loc_get_acquire : 'a Atomic.Loc.t @ local -> 'a
  = "%atomic_load_acquire_loc"
external loc_set_release : 'a Atomic.Loc.t @ local -> 'a -> unit
  = "%atomic_store_release_loc"

type 'a r = { filler : unit; mutable x : 'a [@atomic] }

let seq_cst_get (a : int Atomic.t) = Atomic.get a
[%%expect_asm X86_64{|
seq_cst_get:
  movq  (%rax), %rax
  ret
|}]

let acquire_get (a : int Atomic.t) = get_acquire a
[%%expect_asm X86_64{|
acquire_get:
  movq  (%rax), %rax
  ret
|}]

let seq_cst_set (a : int Atomic.t) v = Atomic.set a v
[%%expect_asm X86_64{|
seq_cst_set:
  xchg  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let release_set (a : int Atomic.t) v = set_release a v
[%%expect_asm X86_64{|
release_set:
  movq  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let release_set_ptr (a : string Atomic.t) v = set_release a v
[%%expect_asm X86_64{|
release_set_ptr:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rsi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}]

let loc_acquire_get (r : int r) = loc_get_acquire [%atomic.loc r.x]
[%%expect_asm X86_64{|
loc_acquire_get:
  movq  8(%rax), %rax
  ret
|}]

let loc_release_set (r : int r) v = loc_set_release [%atomic.loc r.x] v
[%%expect_asm X86_64{|
loc_release_set:
  movq  %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]
