(* TEST
 flambda2;
 { bytecode; }
 { native; }
 (* Disabled on JSIR due to known issue with nullary arguments *)
*)

type void : void mod everything
external void : unit -> void = "%unbox_unit"

let calls = ref 0

let[@inline never] f (_ : void) (_ : void) =
  incr calls;
  !calls

let[@inline never] check (f : void -> void -> int) =
  let partial = f (void ()) in
  assert (!calls = 0);
  assert (partial (void ()) = 1);
  assert (partial (void ()) = 2)

let () = check f
