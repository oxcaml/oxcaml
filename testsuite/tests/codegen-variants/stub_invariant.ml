(* TEST
 codegen_variant = "+fp +sc";
 variant_available;
 arch_amd64;
 readonly_files = "sc_stub_omit_fp_.c";
 all_modules = "${readonly_files} stub_invariant.ml";
 flags = "-runtime-variant d -ccopt -fomit-frame-pointer";
 stderr = "stub_invariant.stderr";
 exit_status = "-4";
 native;
 exit_status = "0";
 stdout = "assertion.stdout";
 stderr = "assertion.stderr";
 script = "grep -q -F 'Assertion failed: fp->prev == link->sp' stub_invariant.stderr";
 script;
*)

(* Frame-pointer variants require C stubs to preserve the rbp chain: under
   fp-sc, growing the stack while a C frame sits between two OCaml regions
   makes caml_try_realloc_stack walk and rewrite the chain through that
   frame (runtime/fiber.c).  This test compiles the stub WITH
   -fomit-frame-pointer (overriding the configured flags) and triggers a
   stack reallocation from a callback under the fp-sc DEBUG runtime
   (-runtime-variant d), checking that the broken invariant is caught by
   the CAMLassert [fp->prev == link->sp] in caml_try_realloc_stack rather
   than silently corrupting the chain.  CAMLassert failure calls
   caml_abort, i.e. __builtin_trap: SIGILL on amd64, hence arch_amd64 and
   exit_status -4 (ocamltest reports signal deaths as the negated signal
   number).  Every failed CAMLassert ends in that signal, so the script
   then looks for this assertion in the message, which goes to a file of
   its own since it embeds a source line number. *)

open Effect
open Effect.Deep

type _ t += E : int -> int t

external c_fun : unit -> int = "c_fun"

let[@inline never][@local never] f x = x

let[@inline never] consume_stack () =
  (* Overflows the fiber's initial allocation (2 * Stack_threshold_words =
     64 words under stack checks), forcing a reallocation; same recipe as
     testsuite/tests/frame-pointers/stack_realloc.ml. *)
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
  consume_stack ();
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

let () =
  Printf.printf "entering callback\n%!";
  ignore (f () : int);
  (* Unreachable: the reallocation in [callback] must trip the CAMLassert. *)
  Printf.printf "returned normally\n%!"
