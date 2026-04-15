(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* This example shows a case where the generated program starts with a sequence
   of deleted code bindings, then a variable, then the rest of the program.
   This used to cause a bug when printing, which this test doesn't try
   to catch, but the pattern seems interesting enough to keep the test. *)
type t = { mutable foo : int }

let env = { foo = 42 }

external id : 'a -> 'a = "%opaque"

let foobar () =
  let rec aux () = if id false then env.foo else aux () in
  aux ()
