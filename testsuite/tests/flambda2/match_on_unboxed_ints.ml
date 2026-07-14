(* TEST
 flambda2;
 flags += " -flambda2-inline-small-function-size 0";
 flags += " -flambda2-inline-large-function-size 0";
 flags += " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt with dump-simplify;
*)

(* CR lambda/flambda: Improve match compilation when matching unboxed ints. *)

let map_ints_to_float_constants = function
  | #0n -> #0.
  | #1n -> #1.
  | #2n -> #2.
  | #3n -> #3.
  | _ -> #4.
;;
[%%expect_fexpr Simplify{|
let code map_ints_to_float_constants_0 deleted in
let code loopify(never) size(60) newer_version_of(map_ints_to_float_constants_0)
      map_ints_to_float_constants_0_1 (param : nativeint)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : float =
  let prim = %int_comp.`nativeint`.lt (param, 2n) in
  switch prim
    | 0 -> k2
    | 1 -> k3
    where k3 =
      let prim_1 = %int_comp.`nativeint`.ne (param, 0n) in
      (switch prim_1
         | 0 -> k (0x0p+0)
         | 1 -> k3
         where k3 =
           let prim_2 = %int_comp.`nativeint`.ne (param, 1n) in
           switch prim_2
             | 0 -> k (0x1p+0)
             | 1 -> k (0x1p+2))
    where k2 =
      let prim_1 = %int_comp.`nativeint`.ne (param, 2n) in
      (switch prim_1
         | 0 -> k (0x1p+1)
         | 1 -> k2
         where k2 =
           let prim_2 = %int_comp.`nativeint`.ne (param, 3n) in
           switch prim_2
             | 0 -> k (0x1.8p+1)
             | 1 -> k (0x1p+2))
in
let $camlTOP1__map_ints_to_float_constants_1 =
  closure map_ints_to_float_constants_0_1 @map_ints_to_float_constants
in
let $camlTOP1 = Block 0 ($camlTOP1__map_ints_to_float_constants_1) in
cont done ($camlTOP1)
|}]
[%%expect_asm X86_64{|
map_ints_to_float_constants:
  cmpq  $2, %rax
  jge   .L1
  testq %rax, %rax
  je    .L0
  cmpq  $1, %rax
  jne   .L2
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L0:
  vxorpd %xmm0, %xmm0, %xmm0
  ret
.L1:
  cmpq  $2, %rax
  je    .L4
  cmpq  $3, %rax
  je    .L3
.L2:
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L3:
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L4:
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
|}]

let map_tagged_ints_to_float_constants = function
  | 0 -> #0.
  | 1 -> #1.
  | 2 -> #2.
  | 3 -> #3.
  | _ -> #4.
;;
[%%expect_fexpr Simplify{|
let code map_tagged_ints_to_float_constants_2 deleted in
let $camlTOP2__switch_block102 =
  Float_array [|0x0p+0;
  0x1p+0;
  0x1p+1;
  0x1.8p+1|]
in
let code loopify(never) size(14) newer_version_of(map_tagged_ints_to_float_constants_2)
      map_tagged_ints_to_float_constants_2_1 (param : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : float =
  let prim = %int_comp.unsigned.lt (3, param) in
  switch prim
    | 0 -> k2
    | 1 -> k (0x1p+2)
    where k2 =
      let arg = %array_load.`float` ($camlTOP2__switch_block102, param) in
      cont k (arg)
in
let $camlTOP2__map_tagged_ints_to_float_constants_3 =
  closure map_tagged_ints_to_float_constants_2_1
    @map_tagged_ints_to_float_constants
in
let $camlTOP2 = Block 0 ($camlTOP2__map_tagged_ints_to_float_constants_3) in
cont done ($camlTOP2)
|}]
[%%expect_asm X86_64{|
map_tagged_ints_to_float_constants:
  cmpq  $7, %rax
  jbe   .L0
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L0:
  leaq  <hidden PC-relative offset>(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}]

let[@inline never] opaque_fun1 () = ()
let[@inline never] opaque_fun2 () = ()
let[@inline never] opaque_fun3 () = ()
let[@inline never] opaque_fun4 () = ()

let match_on_ints = function
  | #0n -> opaque_fun1 ()
  | #1n -> opaque_fun2 ()
  | #2n -> opaque_fun3 ()
  | #3n -> opaque_fun4 ()
  | _ -> ()
;;
[%%expect_fexpr Simplify{|
let code match_on_ints_12 deleted in
let opaque_fun4 = %block_load.[`0`] ($TOP6.camlTOP6) in
let opaque_fun3 = %block_load.[`0`] ($TOP5.camlTOP5) in
let opaque_fun2 = %block_load.[`0`] ($TOP4.camlTOP4) in
let opaque_fun1 = %block_load.[`0`] ($TOP3.camlTOP3) in
let $camlTOP7__match_on_ints_13 =
  closure match_on_ints_12_1 @match_on_ints
and code loopify(never) size(88) newer_version_of(match_on_ints_12)
      match_on_ints_12_1 (param : nativeint)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : imm tagged =
  let opaque_fun1_1 =
    %project_value_slot.[match_on_ints].[opaque_fun1]
      ($camlTOP7__match_on_ints_13)
  in
  let opaque_fun2_1 =
    %project_value_slot.[match_on_ints].[opaque_fun2]
      ($camlTOP7__match_on_ints_13)
  in
  let opaque_fun3_1 =
    %project_value_slot.[match_on_ints].[opaque_fun3]
      ($camlTOP7__match_on_ints_13)
  in
  let opaque_fun4_1 =
    %project_value_slot.[match_on_ints].[opaque_fun4]
      ($camlTOP7__match_on_ints_13)
  in
  let prim = %int_comp.`nativeint`.lt (param, 2n) in
  switch prim
    | 0 -> k2
    | 1 -> k3
    where k3 =
      let prim_1 = %int_comp.`nativeint`.ne (param, 0n) in
      (switch prim_1
         | 0 -> k3
         | 1 -> k4
         where k4 =
           let prim_2 = %int_comp.`nativeint`.ne (param, 1n) in
           (switch prim_2
              | 0 -> k4
              | 1 -> k (0)
              where k4 =
                apply opaque_fun2_1 (0) -> k * k1)
         where k3 =
           apply opaque_fun1_1 (0) -> k * k1)
    where k2 =
      let prim_1 = %int_comp.`nativeint`.ne (param, 2n) in
      (switch prim_1
         | 0 -> k2
         | 1 -> k3
         where k3 =
           let prim_2 = %int_comp.`nativeint`.ne (param, 3n) in
           (switch prim_2
              | 0 -> k3
              | 1 -> k (0)
              where k3 =
                apply opaque_fun4_1 (0) -> k * k1)
         where k2 =
           apply opaque_fun3_1 (0) -> k * k1)
  with {
    opaque_fun1 = opaque_fun1;
    opaque_fun2 = opaque_fun2;
    opaque_fun3 = opaque_fun3;
    opaque_fun4 = opaque_fun4
  }
in
let $camlTOP7 = Block 0 ($camlTOP7__match_on_ints_13) in
cont done ($camlTOP7)
|}]
[%%expect_asm X86_64{|
match_on_ints:
  cmpq  $2, %rax
  jge   .L1
  testq %rax, %rax
  je    .L0
  cmpq  $1, %rax
  jne   .L2
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  32(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L0:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  40(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L1:
  cmpq  $2, %rax
  je    .L4
  cmpq  $3, %rax
  je    .L3
.L2:
  movl  $1, %eax
  ret
.L3:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L4:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  24(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]

let match_on_tagged_ints = function
  | 0 -> opaque_fun1 ()
  | 1 -> opaque_fun2 ()
  | 2 -> opaque_fun3 ()
  | 3 -> opaque_fun4 ()
  | _ -> ()
;;
[%%expect_fexpr Simplify{|
let code match_on_tagged_ints_14 deleted in
let opaque_fun4 = %block_load.[`0`] ($TOP6.camlTOP6) in
let opaque_fun3 = %block_load.[`0`] ($TOP5.camlTOP5) in
let opaque_fun2 = %block_load.[`0`] ($TOP4.camlTOP4) in
let opaque_fun1 = %block_load.[`0`] ($TOP3.camlTOP3) in
let $camlTOP8__match_on_tagged_ints_15 =
  closure match_on_tagged_ints_14_1 @match_on_tagged_ints
and code loopify(never) size(61) newer_version_of(match_on_tagged_ints_14)
      match_on_tagged_ints_14_1 (param : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : imm tagged =
  let opaque_fun1_1 =
    %project_value_slot.[match_on_tagged_ints].[opaque_fun1]
      ($camlTOP8__match_on_tagged_ints_15)
  in
  let opaque_fun2_1 =
    %project_value_slot.[match_on_tagged_ints].[opaque_fun2]
      ($camlTOP8__match_on_tagged_ints_15)
  in
  let opaque_fun3_1 =
    %project_value_slot.[match_on_tagged_ints].[opaque_fun3]
      ($camlTOP8__match_on_tagged_ints_15)
  in
  let opaque_fun4_1 =
    %project_value_slot.[match_on_tagged_ints].[opaque_fun4]
      ($camlTOP8__match_on_tagged_ints_15)
  in
  let prim = %int_comp.unsigned.lt (3, param) in
  switch prim
    | 0 -> k2
    | 1 -> k (0)
    where k2 =
      ((let untagged = %untag_imm (param) in
        switch untagged
          | 0 -> k2
          | 1 -> k3
          | 2 -> k4
          | 3 -> k5)
         where k5 =
           apply opaque_fun4_1 (0) -> k * k1
         where k4 =
           apply opaque_fun3_1 (0) -> k * k1
         where k3 =
           apply opaque_fun2_1 (0) -> k * k1
         where k2 =
           apply opaque_fun1_1 (0) -> k * k1)
  with {
    opaque_fun1 = opaque_fun1;
    opaque_fun2 = opaque_fun2;
    opaque_fun3 = opaque_fun3;
    opaque_fun4 = opaque_fun4
  }
in
let $camlTOP8 = Block 0 ($camlTOP8__match_on_tagged_ints_15) in
cont done ($camlTOP8)
|}]
[%%expect_asm X86_64{|
match_on_tagged_ints:
  cmpq  $7, %rax
  jbe   .L0
  movl  $1, %eax
  ret
.L0:
  sarq  $1, %rax
  leaq  <hidden PC-relative offset>(%rip), %rdx
  movslq (%rdx,%rax,4), %rax
  addq  %rax, %rdx
  jmp   *%rdx
.L1:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  40(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L2:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  32(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L3:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  24(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L4:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]
