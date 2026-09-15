(* TEST
 readonly_files = "cse_address_intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "cse_address_intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -extension layouts_beta";
 flags += " -extension-universe alpha";
 flags += " -experimental-optimizations";
 flags += " -flambda2-simplify-stubs";
 expect.opt;
*)

open Cse_address_intrinsics

external[@layout_poly] get_idx_mut :
  'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b = "%get_idx"
external[@layout_poly] set_idx_mut :
  'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%set_idx"
external poll : unit -> unit = "%poll"

type source = { mutable payload : string }
type output = { mutable before : int64_u; mutable after : int64_u }

(* [payload] is loaded through [%get_idx]. The two [addr_of_value] calls must
   not be CSE'd across [poll]: a moving GC may update the [payload] root while
   leaving the first raw address snapshot unchanged. The [%set_idx] stores make
   the two snapshots visible in the assembly below: the second one must be
   recomputed from the (possibly updated) root after the poll. *)
let indexed_address_snapshots source output =
  let payload = get_idx_mut source (.payload) in
  let before = addr_of_value payload in
  poll ();
  let after = addr_of_value payload in
  set_idx_mut output (.before) before;
  set_idx_mut output (.after) after;
  #(source, payload, output)
[%%expect_asm X86_64{|
indexed_address_snapshots:
  subq  $8, %rsp
  movq  %rbx, %rdi
  movq  (%rax), %rbx
  movq  %rbx, %rsi
  cmpq  (%r14), %r15
  jbe   <hidden GC jump pad>
.L0:
  movq  %rbx, %rdx
  movq  %rsi, (%rdi)
  movq  %rdx, 8(%rdi)
  addq  $8, %rsp
  ret
|}]
