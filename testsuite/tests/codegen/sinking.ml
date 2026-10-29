(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 flags += " -g -gno-upstream-dwarf";
 expect.opt;
*)

(* [z] is only needed when [b] is true: it is computed in that branch only. *)
let partially_dead x y b =
  let z = x * y in
  if b then z + (z lsl 3) else 0
[%%expect_asm X86_64{|
partially_dead:
  cmpq  $1, %rdi
  jne   .L0
  movl  $1, %eax
  ret
.L0:
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  leaq  -8(%rax,%rax,8), %rax
  ret
|}]

(* The whole chain computing [c] sinks into the branch. *)
let chain x b =
  let a = x * 3 in
  let c = a lxor 5 in
  if b then c + c else 1
[%%expect_asm X86_64{|
chain:
  cmpq  $1, %rbx
  jne   .L0
  movl  $3, %eax
  ret
.L0:
  leaq  -2(%rax,%rax,2), %rax
  xorq  $11, %rax
  orq   $1, %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}]

(* [y] is only used in the loop: it sinks to the loop preheader, but not into
   the loop. *)
let not_into_loop x n =
  let y = x * 3 in
  let r = ref 0 in
  if n > 0 then for i = 0 to n do r := !r + (y * i) + y done;
  !r
[%%expect_asm X86_64{|
not_into_loop:
  cmpq  $1, %rbx
  jle   .L1
  cmpq  $1, %rbx
  jl    .L1
  leaq  -2(%rax,%rax,2), %rcx
  sarq  $1, %rbx
  movl  $1, %eax
  xorl  %edi, %edi
.L0:
  movq  %rdi, %rsi
  salq  $1, %rsi
  sarq  $1, %rsi
  leaq  -1(%rcx), %rdx
  imulq %rsi, %rdx
  addq  %rdx, %rax
  leaq  -1(%rax,%rcx), %rax
  incq  %rdi
  cmpq  %rbx, %rdi
  jle   .L0
  ret
.L1:
  movl  $1, %eax
  ret
|}]

(* [v] is computed in the loop but only used on one exit: it sinks out of the
   loop. *)
let rec loop_exit (a : int array) i n =
  if i >= n
  then -1
  else
    let v = i * 7 in
    if Array.unsafe_get a i = 0 then v * v else loop_exit a (i + 1) n
[%%expect_asm X86_64{|
loop_exit:
.L0:
  cmpq  %rdi, %rbx
  jl    .L1
  movq  $-1, %rax
  ret
.L1:
  movq  -4(%rax,%rbx,4), %rsi
  cmpq  $1, %rsi
  jne   .L2
  imulq $7, %rbx
  leaq  -6(%rbx), %rax
  movq  %rax, %rbx
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  ret
.L2:
  addq  $2, %rbx
  jmp   .L0
|}]

(* [y] is only used by the exception handler. *)
let handler_use x f =
  let y = x * 5 in
  match f () with v -> v | exception Not_found -> y * y
[%%expect_asm X86_64{|
handler_use:
  subq  $24, %rsp
  movq  %rax, (%rsp)
  movq  64(%r14), %rax
  movq  %rax, 8(%rsp)
.L0:
  leaq  <hidden PC-relative offset>(%rip), %r11
  pushq %r11
  pushq 48(%r14)
  movq  %rsp, 48(%r14)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L1:
.L2:
  popq  48(%r14)
  addq  $8, %rsp
  addq  $24, %rsp
  ret
.L3:
  movq  8(%rsp), %rbx
  movq  %rbx, 64(%r14)
  movq  caml_exn_Not_found@GOTPCREL(%rip), %rbx
  movq  (%rsp), %rdi
  cmpq  %rbx, %rax
  jne   .L4
  leaq  -4(%rdi,%rdi,4), %rax
  movq  %rax, %rbx
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  addq  $24, %rsp
  ret
.L4:
  call  caml_reraise_exn@PLT
.L5:
|}]

(* Loads from immutable memory can be sunk. *)
let immutable_load (p : int * int) b =
  let x = fst p in
  if b then x * x else 0
[%%expect_asm X86_64{|
immutable_load:
  cmpq  $1, %rbx
  jne   .L0
  movl  $1, %eax
  ret
.L0:
  movq  (%rax), %rax
  movq  %rax, %rbx
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  ret
|}]

(* Loads from mutable memory are not sunk. *)
let mutable_load (r : int ref) b =
  let x = !r in
  if b then x * x else 0
[%%expect_asm X86_64{|
mutable_load:
  movq  (%rax), %rax
  cmpq  $1, %rbx
  jne   .L0
  movl  $1, %eax
  ret
.L0:
  movq  %rax, %rbx
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  ret
|}]

(* [v] is loaded from a block allocated in a region that ends before the
   branch: it must not sink past the end of the region. *)
let region_end (x : int) b =
  let r = stack_ (x, x + 1) in
  let v = fst (Sys.opaque_identity r) in
  exclave_ (if b then v + 1 else 0)
[%%expect_asm X86_64{|
region_end:
  subq  $8, %rsp
  movq  64(%r14), %rsi
  movq  64(%r14), %rdi
  subq  $24, %rdi
  movq  %rdi, 64(%r14)
  cmpq  80(%r14), %rdi
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rdi
  addq  $8, %rdi
  movq  $2816, -8(%rdi)
  movq  %rax, (%rdi)
  addq  $2, %rax
  movq  %rax, 8(%rdi)
  movq  (%rdi), %rax
  movq  %rsi, 64(%r14)
  cmpq  $1, %rbx
  jne   .L1
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L1:
  addq  $2, %rax
  addq  $8, %rsp
  ret
|}]

(* The region only ends on return, after the branch: [v] can sink. *)
let region_load (p : int * int) b =
  let v = fst p in
  let r = stack_ (b, b) in
  if fst (Sys.opaque_identity r) then v + 1 else 0
[%%expect_asm X86_64{|
region_load:
  subq  $8, %rsp
  movq  64(%r14), %rsi
  movq  64(%r14), %rdi
  subq  $24, %rdi
  movq  %rdi, 64(%r14)
  cmpq  80(%r14), %rdi
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rdi
  addq  $8, %rdi
  movq  $2816, -8(%rdi)
  movq  %rbx, (%rdi)
  movq  %rbx, 8(%rdi)
  movq  (%rdi), %rbx
  cmpq  $1, %rbx
  jne   .L1
  movl  $1, %eax
  jmp   .L2
.L1:
  movq  (%rax), %rax
  addq  $2, %rax
.L2:
  movq  %rsi, 64(%r14)
  addq  $8, %rsp
  ret
|}]

(* Reuse a closed region's storage and collect while the new tuple is live.
   A stale root into this tuple can cause the GC to mark one of its fields as
   though it were a block header. *)
let[@inline never] reuse_local_region (x : int) =
  let r = stack_ (x, x, x, x, x, x, x) in
  Gc.full_major ();
  let a, b, c, d, e, f, g = Sys.opaque_identity r in
  if a <> x || b <> x || c <> x || d <> x || e <> x || f <> x || g <> x
  then failwith "local tuple corrupted"
[%%expect {|
val reuse_local_region : int -> unit = <fun>
|}]

(* Even a pure comparison must not keep a local pointer live past the end of
   its region. Here [End_region] is in the comparison's original block. *)
let[@inline never] region_end_compare x other b =
  let r = stack_ (x, x + 1) in
  let equal = Sys.opaque_identity r == Sys.opaque_identity other in
  exclave_ (
    reuse_local_region x;
    if b then equal else false)
[%%expect {|
val region_end_compare : int -> int * int -> bool -> bool @ local = <fun>
|}]

let () =
  for i = 1 to 100 do
    ignore (region_end_compare i (i, i) true)
  done
[%%expect {|
|}]

(* The call puts [End_region] in a later block, so this also exercises the
   region check on the path to the proposed sinking target. *)
let[@inline never] region_end_compare_across_call x other b before_region_end =
  let r = stack_ (x, x + 1) in
  let equal = Sys.opaque_identity r == Sys.opaque_identity other in
  before_region_end ();
  exclave_ (
    reuse_local_region x;
    if b then equal else false)
[%%expect {|
val region_end_compare_across_call :
  int -> int * int -> bool -> (unit -> 'a) -> bool @ local = <fun>
|}]

let () =
  for i = 1 to 100 do
    ignore (region_end_compare_across_call i (i, i) true (fun () -> ()))
  done
[%%expect {|
|}]
