(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 reference =
   "${test_source_directory}/never_returning_functions_backtrace.reference";
 flambda2;
 {
   native;
 }{
   flags = "-g -O3";
   native;
 }{
   flags = "-g -Oclassic";
   native;
 }{
   bytecode;
 }
*)

(* A call to a function that never returns, whose result types are not known
   to the caller, stays a call: the caller keeps its frame in backtraces. See
   [simplify_direct_full_application] in [simplify_apply_expr.ml]. *)

let[@opaque] fail s = raise (Failure s)

let[@inline never] caller x =
  if x > 0 then ignore (fail "caller" : int);
  x + 1

let () =
  match caller 1 with
  | _ -> print_endline "returned"
  | exception Failure s ->
    print_endline s;
    Printexc.print_backtrace stdout
