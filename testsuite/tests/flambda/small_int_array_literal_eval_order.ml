(* TEST *)

(* [y] is bound before [x], so it must be evaluated first, also when both
   calls are substituted into an array literal whose elements are packed
   several to a word. *)

external int8_of_string : string -> int8#
  = "caml_int8_of_string" "caml_int8_of_string_untagged"

external int16_of_string : string -> int16#
  = "caml_int16_of_string" "caml_int16_of_string_untagged"

let[@inline never] right8 r = incr r; #7s

let[@inline never] make8 r s =
  let y = right8 r in
  let x = int8_of_string s in
  [| x; y |]

let[@inline never] right16 r = incr r; #7S

let[@inline never] make16 r s =
  let y = right16 r in
  let x = int16_of_string s in
  [| x; y |]

let () =
  let r = ref 0 in
  (try ignore (make8 r "not a number") with Failure _ -> ());
  Printf.printf "int8#: %d\n" !r;
  r := 0;
  (try ignore (make16 r "not a number") with Failure _ -> ());
  Printf.printf "int16#: %d\n" !r
