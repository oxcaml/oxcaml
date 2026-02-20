(* TEST
 flags += " -O3 -extension-universe upstream_compatible";
 include stdlib_upstream_compatible;
 only-default-codegen;
 expect.opt;
*)

open Stdlib_upstream_compatible

type 'a vec = #{
  size : int;
  buffer : 'a array;
}

(* CR ttebbi:
  - Array bounds checks need a lot of instructions.
  - We could inline a write-barrier fast path (like when the
    target object is young). This is the only way to do something
    like creating short arrays of pointers efficiently. *)
(* using option to avoid the float array case *)
let push (vec : 'a option vec) x =
  let #{size; buffer} = vec in
  buffer.(size) <- x;
  #{vec with size = size + 1}
;;
[%%expect_asm X86_64{|
push:
  subq  $8, %rsp
  movq  %rax, %r12
  movq  %rdi, %rsi
  movq  -8(%rbx), %rax
  salq  $8, %rax
  shrq  $17, %rax
  cmpq  %rax, %r12
  jae   .L117
  leaq  -4(%rbx,%r12,4), %rdi
  call  caml_modify@PLT
  movq  %r12, %rax
  addq  $2, %rax
  addq  $8, %rsp
  ret
.L117:
  movq  camlTOP3__block33@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11

caml_curryVV_V_RVV:
  subq  $8, %rsp
  movq  %rax, %rsi
  subq  $48, %r15
  cmpq  (%r14), %r15
  jb    .L133
.L135:
  leaq  8(%r15), %rax
  movq  $5367, -8(%rax)
  movq  caml_curryVV_V_RVV_1@GOTPCREL(%rip), %rdx
  movq  %rdx, (%rax)
  movabsq $108086391056891909, %rdx
  movq  %rdx, 8(%rax)
  movq  %rsi, 16(%rax)
  movq  %rbx, 24(%rax)
  movq  %rdi, 32(%rax)
  addq  $8, %rsp
  ret

caml_curryVV_V_RVV_1:
  movq  %rax, %rdi
  movq  %rbx, %rax
  movq  32(%rax), %rsi
  movq  24(%rax), %rbx
  movq  16(%rax), %rax
  movq  16(%rsi), %rdx
  jmp   *%rdx
|}]


external int64_u_unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (Int64_u.t[@local_opt]) -> 'a
    @@ portable
    = "%array_unsafe_get_indexed_by_int64#"
  [@@layout_poly]

(* CR ttebbi: When indexing with an untagged index,
   there is no need to first tag the index. *)
let f (x : Int32_u.t array) (i : Int64_u.t) =
  int64_u_unsafe_get x i |> Int32_u.to_int32 |> Int64_u.of_int32
;;
[%%expect_asm X86_64{|
f:
  leaq  1(%rbx,%rbx), %rbx
  movslq -2(%rax,%rbx,2), %rax
  ret

caml_curryV_I_RI:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $40, %r15
  cmpq  (%r14), %r15
  jb    .L118
.L120:
  leaq  8(%r15), %rax
  movq  $4343, -8(%rax)
  movq  caml_curryV_I_RI_1@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movabsq $108086391056891909, %rsi
  movq  %rsi, 8(%rax)
  movq  %rdi, 16(%rax)
  movq  %rbx, 24(%rax)
  addq  $8, %rsp
  ret

caml_curryV_I_RI_1:
  movq  %rax, %rsi
  movq  24(%rbx), %rdi
  movq  16(%rbx), %rax
  movq  16(%rdi), %rdx
  movq  %rsi, %rbx
  jmp   *%rdx
|}]
