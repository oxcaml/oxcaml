(* TEST
 flags += " -cfg-cse-join-points";
 only-default-codegen;
 expect.opt;
*)

(* Tests for CSE across join points ([-cfg-cse-join-points], also enabled
   by [-experimental-optimizations]): facts established before a branch
   survive the join when they are valid on all incoming paths. Mutable
   loads are used because they cannot be eliminated by the middle end, so
   any elimination visible here is performed by [Cfg_cse]. Without the
   flag, [Cfg_cse] starts from an empty state at every join point, and the
   reloads below would remain.

   A [match] on an option is used rather than an [if] on two pure arms so
   that the function contains an actual diamond: the [Some] arm loads from
   the block, which prevents the conditional from being turned into [Csel]
   at selection time. *)

(* The load of [r] before the branch is available on both paths, so the
   reload after the join is replaced by a reuse of the first load. *)
let available_across_join (r : int ref) (o : int option) =
  let a = !r in
  let b = match o with None -> a + 1 | Some x -> x + a in
  a + b + !r
[%%expect_asm X86_64{|
available_across_join:
  movq  (%rax), %rax
  testb $1, %bl
  je    .L0
  leaq  2(%rax), %rbx
  jmp   .L1
.L0:
  movq  (%rbx), %rbx
  leaq  -1(%rbx,%rax), %rbx
.L1:
  addq  %rax, %rbx
  leaq  -2(%rbx,%rax), %rax
  ret
|}]

(* The store on one of the paths invalidates the load equation there, so
   the intersection at the join must drop it: the reload has to remain
   (it yields a different value when [o] is [Some]). *)
let killed_on_one_path (r : int ref) (o : int option) =
  let a = !r in
  (match o with None -> () | Some x -> r := a + x);
  a + !r
[%%expect_asm X86_64{|
killed_on_one_path:
  movq  (%rax), %rdi
  testb $1, %bl
  jne   .L0
  movq  (%rbx), %rbx
  leaq  -1(%rdi,%rbx), %rbx
  movq  %rbx, (%rax)
.L0:
  movq  (%rax), %rax
  leaq  -1(%rdi,%rax), %rax
  ret
|}]
