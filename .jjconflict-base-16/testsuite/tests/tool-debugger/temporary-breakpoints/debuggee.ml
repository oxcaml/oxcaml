(* TEST
 flags += "-g";
 debugger_script = "${test_source_directory}/input_script";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)

(* [finish] sets a temporary breakpoint on the caller's return address.
   In this program that address carries only a pseudo event: the return
   event of [f !r] is weakened because the result is bound by a [let].
   Previously, removing the temporary breakpoint sent a stale second reset
   for that address, making the runtime write an uninitialized word
   into the code; the debuggee then crashed the next time the loop
   reached it.  The [backtrace] and [print] commands in the script keep
   that uninitialized word from accidentally matching the original
   instruction. *)

let f x = x + 1

let () =
  let r = ref 0 in
  for _i = 1 to 5 do
    let a = f !r in
    r := a + 1
  done;
  print_int !r;
  print_newline ()
