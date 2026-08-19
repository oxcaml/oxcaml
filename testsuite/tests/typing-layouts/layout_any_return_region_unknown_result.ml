(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

let[@inline never] forward_direct : type (a : any). (unit -> a) -> a =
  fun f ->
    let local_ _x = ref 0 in
    f ()

let[@inline never] forward_if
    : type (a : any). bool -> (unit -> a) -> (unit -> a) -> a =
  fun b f g ->
    let local_ _x = ref 0 in
    if b then f () else g ()

let[@inline never] forward_let_body : type (a : any). (unit -> a) -> a =
  fun f ->
    let local_ _x = ref 0 in
    let _ = Sys.opaque_identity 0 in
    f ()

let[@inline never] forward_sequence_rhs : type (a : any). (unit -> a) -> a =
  fun f ->
    let local_ _x = ref 0 in
    ignore (Sys.opaque_identity 0);
    f ()

let[@inline never] forward_match
    : type (a : any). bool -> (unit -> a) -> (unit -> a) -> a =
  fun b f g ->
    let local_ _x = ref 0 in
    match Sys.opaque_identity b with
    | true -> f ()
    | false -> g ()

external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

let[@inline never] ignore_local (local_ x) =
  let _ = opaque_identity x in
  ()

let[@inline never] forward_nested
    : type (a : any). bool -> (unit -> a) -> a =
  fun b f ->
    let local_ outer = ref 0 in
    ignore_local outer;
    let[@inline always] inner () =
      let local_ inner = ref 1 in
      ignore_local inner;
      if Sys.opaque_identity b then f () else f ()
    in
    inner ()

let[@inline never] assert_false_after_local : type (a : any). unit -> a =
  fun () ->
    let local_ _x = ref 0 in
    assert false

let[@inline never] assert_false_or_forward_after_local
    : type (a : any). bool -> (unit -> a) -> a =
  fun b f ->
    let local_ _x = ref 0 in
    if Sys.opaque_identity b then assert false else f ()

type (_ : any) value_w =
  | Value_int : int value_w
  | Value_string : string value_w

let[@inline never] same_value_shape : type (a : any). a value_w -> a =
  fun w ->
    match w with
    | Value_int -> 1
    | Value_string -> "one"

let expect_raises f =
  try
    f ();
    assert false
  with Assert_failure _ -> ()

let () =
  assert (forward_direct (fun () -> 1) = 1);
  assert (forward_if true (fun () -> "a") (fun () -> "b") = "a");
  assert (forward_let_body (fun () -> 2) = 2);
  assert (forward_sequence_rhs (fun () -> "seq") = "seq");
  assert (forward_match true (fun () -> 10) (fun () -> 20) = 10);
  let _ : float# = forward_if false (fun () -> #1.0) (fun () -> #2.0) in
  let _ : float# =
    forward_match false (fun () -> #3.0) (fun () -> #4.0)
  in
  assert (forward_nested true (fun () -> 3) = 3);
  assert (forward_nested false (fun () -> "nested") = "nested");
  let _ : float# = forward_nested true (fun () -> #5.0) in
  expect_raises (fun () -> let _ : int = assert_false_after_local () in ());
  expect_raises (fun () -> let _ : float# = assert_false_after_local () in ());
  expect_raises (fun () ->
    let _ : int = assert_false_or_forward_after_local true (fun () -> 4) in
    ());
  assert (assert_false_or_forward_after_local false (fun () -> 5) = 5);
  let _ : float# =
    assert_false_or_forward_after_local false (fun () -> #6.0)
  in
  assert (same_value_shape Value_int = 1);
  assert (same_value_shape Value_string = "one");
  ()
