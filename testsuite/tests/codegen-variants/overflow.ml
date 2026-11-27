(* TEST
 ocamlrunparam += ",l=200000";
 script = "sh ${test_source_directory}/overflow_growable.sh";
 config_script;
 {
   codegen_variant = "-sc";
   variant_available;
   compiler_directory_suffix = ".base";
   native;
 }{
   codegen_variant = "+sc -fp";
   variant_available;
   compiler_directory_suffix = ".sc";
   native;
 }{
   codegen_variant = "+sc +fp";
   variant_available;
   compiler_directory_suffix = ".fp-sc";
   native;
 }
*)

(* The stack-overflow behavior of both stack-checks modes (see
   runtime/fiber.c and runtime/signals_nat.c):

   - Stack checks ("sc", "fp-sc"): stacks are small malloc'd blocks; the
     compiler-inserted limit checks call caml_try_realloc_stack, which
     doubles the stack (copying it) up to OCAMLRUNPARAM l (here 200_000
     words); past the cap the wrapper (caml_call_realloc_stack in
     runtime/amd64.S and arm64.S) raises Stack_overflow asynchronously
     via caml_raise_async.  The first [deep] below needs several hundred
     kilobytes of stack, an order of magnitude above the initial
     allocation, so it only succeeds if reallocation ran.

   - Guard pages ("base"): the main stack is allocated at full size up
     front, min(RLIMIT_STACK, 1 GiB, OCAMLRUNPARAM l) --
     caml_get_init_stack_wsize clamps the initial size to the l cap --
     with a PROT_NONE guard page below it; overflow faults in the guard
     page and the SIGSEGV handler (runtime/signals_nat.c) redirects to
     caml_raise_stack_overflow, which also raises asynchronously.  The l
     cap therefore bounds BOTH modes here: with l = 200_000 words (1.6 MB
     on 64-bit) the first [deep] (a few hundred kilobytes) fits and the
     second overflows at ~1.6 MB in either mode, independent of the box's
     RLIMIT_STACK for any sane rlimit (>= the first [deep]'s needs).

   Both modes therefore raise an asynchronous Stack_overflow, catchable
   only inside Sys.with_async_exns; both blocks check the same output.

   Neither mode is there in a runtime configured with
   --enable-mmap-map-stack, whose stacks have a fixed size and no guard
   page: overflow_growable.sh skips the test there. *)

let[@inline never] rec deep n =
  (* [a] and [b] live across the recursive call, forcing a real frame per
     level; the result is always 1. *)
  let a = Sys.opaque_identity n in
  let b = Sys.opaque_identity (n + 1) in
  if n = 0 then a + b else deep (n - 1) + a - b + 1

let () =
  Printf.printf "deep recursion: %d\n%!" (deep 10_000);
  try
    Sys.with_async_exns (fun () -> ignore (deep max_int : int));
    print_endline "unreachable"
  with Stack_overflow -> print_endline "caught Stack_overflow"
