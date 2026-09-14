(* Encoding tests for [X86_binary_emitter].

   Each case assembles a single instruction and prints the resulting bytes. The
   expected output was produced by assembling the same instructions with GNU as,
   so any difference is a deviation from the system assembler.

   The cases focus on memory operands that use the [Mem64_RIP] constructor
   (RIP-relative addressing of a symbol, as produced with [-nodynlink]), and on
   immediate forms of [test] and the [add]-family instructions whose operand
   width is determined by the memory operand. *)

open X86_ast
open X86_dsl

let hex s =
  String.concat " "
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let assemble instr =
  let section =
    { X86_binary_emitter.sec_name = X86_proc.Section_name.of_string ".text";
      sec_instrs = [| Ins instr |]
    }
  in
  X86_binary_emitter.clear_cross_section_labels ();
  X86_binary_emitter.contents (X86_binary_emitter.assemble_section X64 section)

let test gas_syntax instr =
  let bytes =
    match assemble instr with
    | bytes -> hex bytes
    | exception exn -> Printf.sprintf "EXCEPTION %s" (Printexc.to_string exn)
  in
  Printf.printf "%-40s %s\n" gas_syntax bytes

let rip typ = mem64_rip typ "sym"

let stack typ = mem64 typ 8 (Scalar RSP)

let r9 = Reg64 R9

let r9d = Reg32 R9

let () =
  (* Sign- and zero-extending loads from RIP-relative addresses. *)
  test "movsbq sym(%rip), %rax" (MOVSX (rip BYTE, rax));
  test "movsbl sym(%rip), %eax" (MOVSX (rip BYTE, eax));
  test "movsbq sym(%rip), %r9" (MOVSX (rip BYTE, r9));
  test "movsbl sym(%rip), %r9d" (MOVSX (rip BYTE, r9d));
  test "movswq sym(%rip), %rax" (MOVSX (rip WORD, rax));
  test "movswl sym(%rip), %eax" (MOVSX (rip WORD, eax));
  test "movswq sym(%rip), %r9" (MOVSX (rip WORD, r9));
  test "movswl sym(%rip), %r9d" (MOVSX (rip WORD, r9d));
  test "movzbq sym(%rip), %rax" (MOVZX (rip BYTE, rax));
  test "movzbq sym(%rip), %r9" (MOVZX (rip BYTE, r9));
  test "movzwq sym(%rip), %rax" (MOVZX (rip WORD, rax));
  test "movzwl sym(%rip), %eax" (MOVZX (rip WORD, eax));
  test "movzwq sym(%rip), %r9" (MOVZX (rip WORD, r9));
  test "movzwl sym(%rip), %r9d" (MOVZX (rip WORD, r9d));
  (* Byte arithmetic with an immediate. *)
  test "addb $1, sym(%rip)" (ADD (int 1, rip BYTE));
  test "addb $-1, sym(%rip)" (ADD (int (-1), rip BYTE));
  test "cmpb $1, sym(%rip)" (CMP (int 1, rip BYTE));
  test "xorb $-1, sym(%rip)" (XOR (int (-1), rip BYTE));
  test "addb $1, %ah" (ADD (int 1, ah));
  test "addb $1, 8(%rsp)" (ADD (int 1, stack BYTE));
  (* 32-bit arithmetic with an immediate. *)
  test "addl $1, sym(%rip)" (ADD (int 1, rip DWORD));
  test "addl $1000, sym(%rip)" (ADD (int 1000, rip DWORD));
  test "cmpl $1, sym(%rip)" (CMP (int 1, rip DWORD));
  test "subl $-1, sym(%rip)" (SUB (int (-1), rip DWORD));
  test "andl $1000, sym(%rip)" (AND (int 1000, rip DWORD));
  (* 64-bit arithmetic with an immediate. *)
  test "addq $1, sym(%rip)" (ADD (int 1, rip QWORD));
  test "addq $1000, sym(%rip)" (ADD (int 1000, rip QWORD));
  test "cmpq $1, sym(%rip)" (CMP (int 1, rip QWORD));
  test "andq $-1, sym(%rip)" (AND (int (-1), rip QWORD));
  (* [test] with an immediate. *)
  test "testb $1, sym(%rip)" (TEST (int 1, rip BYTE));
  test "testb $1, 8(%rsp)" (TEST (int 1, stack BYTE));
  test "testl $1, sym(%rip)" (TEST (int 1, rip DWORD));
  test "testl $1, 8(%rsp)" (TEST (int 1, stack DWORD));
  test "testl $1, %r9d" (TEST (int 1, r9d));
  test "testq $1, sym(%rip)" (TEST (int 1, rip QWORD));
  test "testq $1, 8(%rsp)" (TEST (int 1, stack QWORD));
  test "testq $1, %r9" (TEST (int 1, r9))
