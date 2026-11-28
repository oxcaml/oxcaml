(* TEST
 include eval;
 flags = "-extension runtime_metaprogramming";
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   native;
 } {
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   native;
 } {
   codegen_variant = "sc";
   variant_available;
   compiler_directory_suffix = ".sc";
   native;
 } {
   codegen_variant = "poll";
   variant_available;
   compiler_directory_suffix = ".poll";
   native;
 } {
   codegen_variant = "fp-sc";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   native;
 } {
   codegen_variant = "fp-poll";
   variant_available;
   compiler_directory_suffix = ".fp-poll";
   native;
 } {
   codegen_variant = "poll-sc";
   variant_available;
   compiler_directory_suffix = ".poll-sc";
   native;
 } {
   codegen_variant = "fp-poll-sc";
   variant_available;
   compiler_directory_suffix = ".fp-poll-sc";
   native;
 } {
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   native;
 } {
   codegen_variant = "asan-fp-poll";
   variant_available;
   compiler_directory_suffix = ".asan-fp-poll";
   native;
 }
*)

#syntax quotations on

(* Like include_ocamlcommon.ml, for [include eval]: a program built for a
   codegen variant is linked with that variant's build of eval and of the
   compiler's libraries, and the code that it compiles in process is code
   for that variant, like the program itself. *)

let () =
  let fib : int -> int =
    Eval.eval
      <[ let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in
         fib ]>
  in
  Printf.printf "fib 20 = %d\n" (fib 20)
