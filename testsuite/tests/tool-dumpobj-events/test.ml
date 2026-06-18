(* TEST
 flags = "-g -nopervasives";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Checks that debug events record which calls are unyielding (the function
   and all of its arguments are at mode [unyielding], so the call can never
   perform a free effect). Non-tail unyielding calls get an [Event_after]
   event with [Event_unyielding_call]; unyielding tail calls get a pseudo
   event with [Event_unyielding_call] at the APPTERM instruction; calls that
   may yield keep plain [Event_return] (or no event at all for tail calls). *)

external ( + ) : int -> int -> int = "%addint"
external opaque : 'a -> 'a = "%opaque"
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

external pipe_yielding :
  'a @ yielding -> ('a -> 'b) @ yielding -> 'b = "%revapply"

let f x = x + 1

(* Non-tail unyielding call: expect "after/unyielding-call(1)". *)
let call_nontail () = f 41 + 1

(* Unyielding tail call: expect "pseudo/unyielding-call(1)" at APPTERM. *)
let rec loop n = match n with 0 -> 0 | _ -> loop (n + (-1))

(* The call to [h] may yield: expect a plain "after/ret(1)". *)
let call_yielding_arg (h : (unit -> int) @ yielding) =
  let r = h () in
  r + 1

(* Yielding tail call: no event at all. *)
let tail_yielding (h : (unit -> int) @ yielding) = h ()

(* Partial application: still unyielding. *)
let add a b = a + b

let inc = add 1

(* The function is a lambda, so typecore does not rewrite this into a plain
   application and it reaches the [%revapply] primitive path: expect
   "after/unyielding-call(1)" on the call synthesized for [|>]. *)
let revapply_nontail x = (x |> (fun y -> y + 1)) + 1

(* Same, but through a [%revapply] whose declared argument modes are
   yielding: expect a plain "after/ret(1)". *)
let revapply_yielding x = pipe_yielding x (fun y -> y + 1) + 1

let () =
  let _ = opaque (call_nontail ()) in
  let _ = opaque (loop 3) in
  let _ = opaque (call_yielding_arg (fun () -> 7)) in
  let _ = opaque (tail_yielding (fun () -> 8)) in
  let _ = opaque (inc 5) in
  let _ = opaque (revapply_nontail 1) in
  let _ = opaque (revapply_yielding 2) in
  ()
