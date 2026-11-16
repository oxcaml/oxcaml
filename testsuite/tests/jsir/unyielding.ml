(* TEST
 flags = "-djsir -nopervasives";
 jsir;
*)

(* Checks that the JSIR produced by Flambda 2 records which calls are
   unyielding (the function and all of its arguments are at mode
   [unyielding], so the call can never perform a free effect). In the dump,
   calls that may yield are printed as [f~(args)]; unyielding calls as
   [f(args)]. *)

external ( + ) : int -> int -> int = "%addint"
external opaque : 'a -> 'a = "%opaque"

let[@inline never] f x = x + 1

(* [f] and [41] are unyielding: expect [f(41)]. *)
let[@inline never] call_unyielding () = f 41 + 1

(* [h] may yield: expect [h~()]. *)
let[@inline never] call_yielding (h : (unit -> int) @ yielding) = h () + 1

(* Passing an unyielding function where a yielding one is accepted: the call
   itself is still unyielding. *)
let[@inline never] call_with_unyielding_arg () = call_yielding (fun () -> 7)

(* Over-application of an arity-one function: both resulting calls are
   unyielding. *)
let[@inline never] curried x =
  let x = opaque x in
  fun y -> x + y

let[@inline never] over_app () = curried 1 2

(* Partial application: the call inside the generated wrapper may yield, but
   the call to the wrapper is unyielding. *)
let[@inline never] add a b = a + b

let inc = add 1

let[@inline never] partial_app () = inc 5

let () =
  let _ = opaque (call_unyielding ()) in
  let _ = opaque (call_with_unyielding_arg ()) in
  let _ = opaque (call_yielding ((fun () -> 8) : _ @ yielding)) in
  let _ = opaque (over_app ()) in
  let _ = opaque (partial_app ()) in
  ()
