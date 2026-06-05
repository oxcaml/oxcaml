(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -flambda2-inline-small-function-size 0";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt with dump-raw, dump-simplify;
   check-fexpr-dump;
 *)

type int_box = Box of int

let unbox_int_and_add_one (Box x) =
  (* Assume the body of this function is larger than the small function size,
     which we enforce by setting it to [0] in this test. *)
  x + 1

let[@inline] unbox_int_and_add_one_wrapper (x : int_box) =
  (* This call does not pass the "argument types useful" check, because we
     don't have more precise information about [x] in [g] than what we have
     from the value kinds on the parameters of [f]. *)
  unbox_int_and_add_one x

let box_int_then_call_unbox_int_and_add_one_wrapper (x : int) =
  (* This call should pass the "argument types useful" check: we don't know
     anything more precise about the value of the argument, but we do know that
     it's first field is available as a variable in the program -- we want to
     speculatively inline, because we could eliminate projection primitives. *)
  unbox_int_and_add_one_wrapper (Box x)
