(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

let () = (
    assert false
  )

(* TEST
 exit_status = "2";
 {
   bytecode;
 }{
   native;
 }{
   (* If OCAMLRUNPARAM enables backtraces, the js_of_ocaml runtime rethrows
      the uncaught exception as a JS error, so node prints a JS stack trace
      and exits with status 7 instead of 2. *)
   ocamlrunparam = "b=0";
   jsir;
 }
*)
