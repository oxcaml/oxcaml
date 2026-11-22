(* TEST
 {
   codegen_variant = "+fp -sc";
   variant_available;
   script = "sh ${test_source_directory}/has-backtrace-symbols.sh";
   config_script;
   readonly_files = "fp_backtrace.c stack_realloc_.c";
   all_modules = "${readonly_files} stack_realloc.ml";
   compiler_directory_suffix = ".fp";
   {
 (* NOTE clang on macOS and gcc on Linux are less eager to inline
         certain C functions in the runtime. *)
     reference = "${test_source_directory}/stack_realloc.arm64.reference";
     arch_arm64;
     native;
   } {
     reference = "${test_source_directory}/stack_realloc.reference";
     arch_amd64;
     native;
   }
 }{
 (* Under fp-sc the fiber actually grows during [consume_stack] (the
    guard-page variants never reallocate), so this block executes the
    frame-pointer chain rewrite in caml_try_realloc_stack
    (runtime/fiber.c) across the C stub's frame: the backtrace below the
    growth cannot come out identical if the reallocation fails (the fiber
    is too small for the recursion) or rewrites the chain incorrectly. *)
   codegen_variant = "+fp +sc";
   variant_available;
   script = "sh ${test_source_directory}/has-backtrace-symbols.sh";
   config_script;
   readonly_files = "fp_backtrace.c stack_realloc_.c";
   all_modules = "${readonly_files} stack_realloc.ml";
   compiler_directory_suffix = ".fp-sc";
   {
     reference = "${test_source_directory}/stack_realloc.arm64.reference";
     arch_arm64;
     native;
   } {
 (* Trace the stack reallocations of [consume_stack] (see the
    [trace-stack-growth] handling below). The script fails unless the
    fiber grew there: without a reallocation this leg would not be
    exercising the chain rewrite at all. It then takes the trace out of
    the output, which leaves the backtrace of the other legs: how often
    the stack grows, and to what sizes, is not what this test is about. *)
     arguments = "trace-stack-growth";
     reference = "${test_source_directory}/stack_realloc.reference";
     arch_amd64;
     setup-ocamlopt.opt-build-env;
     ocamlopt.opt;
     check-ocamlopt.opt-output;
     run;
     stdout = "stack-growth.stdout";
     stderr = "stack-growth.stderr";
     script = "sh ${test_source_directory}/stack-growth.sh ${output}";
     script;
     check-program-output;
   }
 }
*)

open Effect
open Effect.Deep

type _ t += E : int -> int t

external fp_backtrace : string -> unit = "fp_backtrace" [@@noalloc]
external c_fun : unit -> int = "c_fun"

let[@inline never][@local never] f x = x

let[@inline never] consume_stack () =
  (* TODO Somehow get a value that would always cause a stack reallocation
   * Currently anything above 32 should cause a stack reallocation since a new
   * fiber stack size is given by caml_fiber_wsz = 2 * Stack_threshold_words
   * and Stack_threshold_words = 32 *)
  (* in words *)
  let size = 128 in
  let allocated = 2 * 2 (* 2 spilled registers *) + 1 (* saved frame pointer *) in
  let count = size / allocated in
  let[@inline never] rec gobbler i =
    (* Force spilling of x0 and x1 *)
    let x0 = Sys.opaque_identity 42 in
    let x1 = Sys.opaque_identity 42 in
    let _ = f x0 in
    let _ = f x1 in
    let _ = Sys.opaque_identity x0 in
    let _ = Sys.opaque_identity x1 in
    let v = if i = 1 then 42 (* dummy *) else gobbler (i - 1) in
    v - 1 (* dummy *)
  in
  ignore (gobbler count)

let[@inline never] callback () =
  (* The fp-sc leg (see the TEST block) passes [trace-stack-growth] to
     trace the stack reallocations that happen in [consume_stack]: 0x800 is
     CAML_GC_MSG_STACKS and 0x10000000 is CAML_GC_MSG_NO_TIMESTAMP
     (runtime/caml/misc.h), so each growth of the fiber's stack prints a
     "Growing stack to ..." line for the test to find. *)
  let trace = Array.exists (String.equal "trace-stack-growth") Sys.argv in
  if trace then Gc.set { (Gc.get ()) with verbose = 0x10000800 };
  consume_stack ();
  if trace then Gc.set { (Gc.get ()) with verbose = 0 };
  fp_backtrace Sys.argv.(0);
  0

let _ = Callback.register "callback" callback

let[@inline never] f () =
  let[@inline never] f_comp () =
    let v = perform (E 0) in
    let w = c_fun () in
    v + w + 1
  in
  let f_effc (type a) (eff : a t) : ((a, 'b) continuation -> 'b) option =
    let[@inline never] f_effc_e v k = continue k (v + 1) in
    match eff with
    | E v -> Some (f_effc_e v)
    | e -> None
  in
  match_with f_comp ()
  { retc = (fun v -> v);
    exnc = (fun e -> raise e);
    effc = f_effc }

let () = assert (f () == 3)
