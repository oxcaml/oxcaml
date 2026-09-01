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

(* --- Local returning functions without [exclave_] that allocate --- *)

let takes_local (_ @ local) =
  ()

let local_argument_application () =
  takes_local 42

let rec direct_tail_call x =
  if x then x else direct_tail_call true

(* Tail-call position with && and || *)
let rec sequand_tail_call x =
  x && sequand_tail_call false

let rec sequor_tail_call x =
  x || sequor_tail_call true

let labelled_arguments ~a ~b ~c =
  ()

let omitted_labelled_argument =
  labelled_arguments ~a:() ~c:()

let omitted_labelled_argument_application () =
  omitted_labelled_argument ~b:()

let () =
  local_argument_application ();
  ignore (direct_tail_call false);
  ignore (sequand_tail_call true);
  ignore (sequor_tail_call false);
  omitted_labelled_argument_application ()


(* --- Local returning functions with [exclave_] that do not allocate --- *)

let returns_local_unit () =
  exclave_ ()

(* does not type check without [exclave_] *)
let tail_call_returns_local_unit x =
  exclave_
  returns_local_unit x


(* --- Local returning functions with [exclave_] that allocate *)

let takes_local_alloc (_ @ local) =
  exclave_ ((), ())

let local_argument_application_alloc () =
  exclave_ (takes_local_alloc 42)

let rec direct_tail_call_alloc x =
  if x then exclave_ ((), ()) else exclave_ (direct_tail_call_alloc true)

let rec sequand_tail_call_exclave x =
  x && (sequand_tail_call_exclave false)

let rec sequor_tail_call_exclave x =
  x || (sequor_tail_call_exclave true)

let labelled_arguments_alloc ~a ~b ~c =
  exclave_ ((), ())

let omitted_labelled_argument_alloc =
  labelled_arguments_alloc ~a:() ~c:()

let omitted_labelled_argument_alloc_application () =
  exclave_ (omitted_labelled_argument_alloc ~b:())

let call_local_argument_application_alloc () =
  let local_ pair = local_argument_application_alloc () in
  match pair with _x, _y -> ()

let call_direct_tail_call_alloc () =
  let local_ pair = direct_tail_call_alloc false in
  match pair with _x, _y -> ()

let call_omitted_labelled_argument_alloc_application () =
  let local_ pair = omitted_labelled_argument_alloc_application () in
  match pair with _x, _y -> ()

let () =
  call_local_argument_application_alloc ();
  call_direct_tail_call_alloc ();
  ignore (sequand_tail_call_exclave true);
  ignore (sequor_tail_call_exclave false);
  call_omitted_labelled_argument_alloc_application ()
