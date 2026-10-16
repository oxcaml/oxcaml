(* TEST
 {
   flags = "-dlambda -dno-unique-ids";
   compiler_reference2 =
     "${test_source_directory}/try_reset_dst.lambda.reference";
   native;
 }{
   flags = "-runtime-variant=d";
   native;
 }{
   flags = "-runtime-variant=d -w -51";
   native;
 }
*)

(* The dps version of the [try] body writes the destination before the
   recursive call. When a deeper call raises, the handler writes the same
   destination again, so it must do so with [caml_modify]: TMC initializes
   the destination with [caml_initialize], which requires an immediate in the
   field. The debug runtime checks this. Paths that do not write the
   destination again, such as re-raising, must not pay for it. 
   This crashes the debug runtime, due to an assertion.
   *)

exception Fail

let[@inline never] fail () : unit = raise_notrace Fail

let[@tail_mod_cons] rec f n =
  try
    if n = 0 then fail ();
    n :: f (n - 1)
  with Fail when n = 2 -> [ n * 100 ]

(* [f 0] and [f 1] raise, [f 2] catches the exception. *)
let () = assert (f 4 = [ 4; 3; 200 ])
