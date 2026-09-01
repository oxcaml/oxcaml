(* TEST
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt;
*)

(* Both branches of the or-pattern compile to CFG blocks that are identical
   except for their debug info (the two patterns cover different character
   ranges on the same line). [Cfg_merge_blocks] must ignore that difference
   and produce a single return path. *)

type void = unit#

type t =
  | A of { x : string; y : void }
  | B of { x : string; y : float }

let get_x (A { x; _ } | B { x; _ }) = x
[%%expect_asm X86_64{|
get_x:
  movzbq -8(%rax), %rbx
  movq  (%rax), %rax
  ret
|}]
