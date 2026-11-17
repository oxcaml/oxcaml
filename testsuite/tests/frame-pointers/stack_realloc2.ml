(* TEST
 {
   codegen_variant = "fp";
   variant_available;
   script = "sh ${test_source_directory}/has-backtrace-symbols.sh";
   script;
   readonly_files = "fp_backtrace.c stack_realloc_.c";
   all_modules = "${readonly_files} stack_realloc2.ml";
   compiler_directory_suffix = ".fp";
   {
     (* NOTE clang on MacOS and gcc on Linux are less eager to inline
             certain C functions in the runtime. *)
     reference = "${test_source_directory}/stack_realloc2.arm64.reference";
     arch_arm64;
     native;
   } {
     reference = "${test_source_directory}/stack_realloc2.reference";
     arch_amd64;
     native;
   }
 }{
 (* Under fp-sc the parent stack grows while the fiber is suspended (the
    guard-page variants never reallocate), exercising the frame-pointer
    chain rewrite in caml_try_realloc_stack (runtime/fiber.c); see
    stack_realloc.ml. *)
   codegen_variant = "fp-sc";
   variant_available;
   script = "sh ${test_source_directory}/has-backtrace-symbols.sh";
   script;
   readonly_files = "fp_backtrace.c stack_realloc_.c";
   all_modules = "${readonly_files} stack_realloc2.ml";
   compiler_directory_suffix = ".fp-sc";
   {
     reference = "${test_source_directory}/stack_realloc2.arm64.reference";
     arch_arm64;
     native;
   } {
 (* Trace stack reallocations (see the [trace-stack-growth] handling
    below) so that the reference asserts the parent stack actually grew:
    without the "Growing stack" events this leg would not be exercising
    the chain rewrite at all. *)
     arguments = "trace-stack-growth";
     reference = "${test_source_directory}/stack_realloc2.fp-sc.reference";
     arch_amd64;
     setup-ocamlopt.opt-build-env;
     ocamlopt.opt;
     check-ocamlopt.opt-output;
     run;
 (* The debug runtime (USE_RUNTIME=d) starts stacks at 512 bytes rather
    than the normal 32k, so it also prints growth lines below 64k; filter
    those out so that the reference asserts the same four parent-stack
    growths (64k..512k) under both runtimes. *)
     stdout = "filter-small-stack-growth.stdout";
     stderr = "filter-small-stack-growth.stderr";
     script = "sh ${test_source_directory}/filter-small-stack-growth.sh ${output}";
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

let () =
  (* The fp-sc leg (see the TEST block) passes [trace-stack-growth] to
     trace stack reallocations: 0x800 is CAML_GC_MSG_STACKS and 0x10000000
     is CAML_GC_MSG_NO_TIMESTAMP (runtime/caml/misc.h), so each growth of
     the parent stack prints a deterministic "Growing stack to ..." line
     for the reference file to assert.  Enabled here rather than through
     OCAMLRUNPARAM because ocamltest exports the [ocamlrunparam] variable
     to the compiler invocations too, polluting the compiler output. *)
  if Array.exists (String.equal "trace-stack-growth") Sys.argv then
    Gc.set { (Gc.get ()) with verbose = 0x10000800 }

let[@inline never][@local never] f x = x

let[@inline never] consume_stack () =
  (* [consume_stack] runs in the effect handler, i.e. on the parent (main)
     stack while the fiber is suspended.  Under stack checks the main stack
     starts at 32KiB (Stack_init_bsize) and doubles on each reallocation;
     each [gobbler] frame occupies a few tens of bytes (48 on amd64), so
     7000 frames consume roughly 300-450KiB on any target, comfortably
     inside the 256KiB..512KiB doubling interval: the recursion forces four
     parent-stack reallocations (to 64k, 128k, 256k and 512k bytes) with a
     wide safety margin against codegen-induced frame-size drift.  The
     guard-page (plain fp) legs run the same recursion on the main stack,
     which never reallocates and is at least 8MiB there. *)
  let count = 7000 in
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
    let[@inline never] f_effc_e v k =
      consume_stack ();
      continue k (v + 1)
    in
    match eff with
    | E v -> Some (f_effc_e v)
    | e -> None
  in
  match_with f_comp ()
  { retc = (fun v -> v);
    exnc = (fun e -> raise e);
    effc = f_effc }

let () = assert (f () == 3)
