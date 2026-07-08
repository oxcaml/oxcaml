(* TEST
 flags += "-extension mode_polymorphism_alpha";
 native;
*)

(* Regression test: under mode polymorphism, a return mode may be inferred as
  [local -- global]. However, if a return value does not cause an allocation
    (meaning that a region can safely be removed), it's important that we do not
  infer the application mode to be [local]. Under mode polymorphism, we infer the
  return_mode to be local only when it returns via [exclave_]. The following tests
  cause fatal errors if this is not implemented correctly. *)

let takes_local (_ @ local) =
  ()

let local_argument_application () =
  takes_local 42

let out_of_order_partial_application () =
  let foo ~a:_ ~b:_ ~c:_ = () in
  let f = foo ~a:() ~c:() in
  f ~b:()

let rec direct_tail_call x =
  if x then x else direct_tail_call true

(* Tail-call position with && and || *)
let rec sequand_tail_call x =
  x && sequand_tail_call false

let rec sequor_tail_call x =
  x || sequor_tail_call true

let () =
  local_argument_application ();
  out_of_order_partial_application ();
  ignore (direct_tail_call false);
  ignore (sequand_tail_call true);
  ignore (sequor_tail_call false)
