(* TEST
 arch_amd64;
 only-default-codegen;
 flags = "-O3";
 expect.opt;
*)

external unsafe_get_int32 : bytes -> int -> int32 = "%caml_bytes_get32u"
external unsafe_set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"

let[@inline never] increment b =
  unsafe_set_int64 b 0
    (Int64.succ (Int64.of_int32 (unsafe_get_int32 b 0)))
[%%expect_asm X86_64{|
increment:
  movslq (%rax), %rbx
  incq  %rbx
  movq  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

(* The 32-bit load reads zero, so the correct 64-bit result is 1.
   All accesses are in bounds. *)
let () =
  let b = Bytes.make 8 '\000' in
  Bytes.set_int64_ne b 0 0x0000000100000000L;
  increment b;
  Format.printf "%Ld@." (Bytes.get_int64_ne b 0)
[%%expect {|
1
|}]
