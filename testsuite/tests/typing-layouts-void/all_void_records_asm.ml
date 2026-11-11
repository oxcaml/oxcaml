(* TEST
 flags += " -extension layouts_beta -O3 -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

type ('a : any) t = { mutable field : 'a }

(* Record creation should return a statically allocated block
   by moving it into [%rax]. *)
let make () : unit# t = { field = #() }
[%%expect_asm X86_64{|
make:
  movq  <hidden PC-relative offset>(%rip), %rax
  ret
|}]

(* Products shouldn't matter:
   they should also simply return a statically allocated block
   by moving it into [%rax]. *)
let product () : #(unit# * unit#) t = { field = #(#(), #()) }
[%%expect_asm X86_64{|
product:
  movq  <hidden PC-relative offset>(%rip), %rax
  ret
|}]

(* Getting a [void] field should be a no-op. *)
let get (t : unit# t) = t.field
[%%expect_asm X86_64{|
get:
  ret
|}]

(* Setting a [void] field should be a no-op. *)
let set (t : unit# t) = t.field <- #()
[%%expect_asm X86_64{|
set:
  movl  $1, %eax
  ret
|}]
