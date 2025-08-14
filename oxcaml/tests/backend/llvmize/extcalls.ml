external print_and_add : int -> int -> int = "" "print_and_add"

external too_many :
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int = "" "too_many"

external int_and_float : int -> float -> int -> float -> unit = "int_and_float"

let[@inline never] [@local never] call_too_many () =
  too_many 1 2 3 4 5 6 7 8 9 10 11 12

let[@inline never] [@local never] call_print_and_add () = print_and_add 9 10

let[@inline never] [@local never] call_int_and_float () =
  int_and_float 1 2.0 3 4.0

(* This test checks that [Call_no_return] is handled properly - copied from ../zero_alloc_checker/test_flambda2_invalind.ml *)

external opaque : 'a -> 'a = "%opaque"
external box : float# -> (float[@local_opt]) = "%box_float"
external unbox : (float[@local_opt]) -> float# = "%unbox_float"

let f _ = assert false

let call_no_return () =
  let a = opaque 0.0 |> unbox in
  List.iter
    (fun [@zero_alloc] _ ->
       let _ : float = box a +. 0.0 |> opaque in
       f #0.0) []

