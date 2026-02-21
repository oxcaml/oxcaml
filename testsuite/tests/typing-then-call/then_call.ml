(* TEST
   expect;
*)

(* Basic usage: [@then_call f] evaluates e, calls f on the result, returns e *)
let _ =
  let x = 42 [@then_call (fun _ -> ())] in
  x;;
[%%expect{|
- : int = 42
|}]

(* let rec compatibility: [@then_call f] is transparent to the rec validity check *)
let rec x = [1;2;3] [@then_call (fun _ -> ())];;
[%%expect{|
val x : int list = [1; 2; 3]
|}]

(* let rec compatibility: arbitrary code is not permitted *)
(* This test should fail! *)
let result =
  let result = ref [] in
  let rec x = 1 :: y [@then_call result := (List.tl y); ignore] and y = 2 :: x in
  x, y, !result
;;
[%%expect{|
val x : int list = [1; 2; 3]
|}]

(* mode compatibility: this is accepted *)
let _ =
  let _ = stack_ [1;2;3] [@then_call ((fun (_ @ local) -> ()))] in
  ()
;;
[%%expect{|
val x : int list = [1; 2; 3]
|}]

(* mode compatibility: this is rejected *)
(* This test should fail! *)
let _ =
  let _ = stack_ [1;2;3] [@then_call ((fun (_ @ global) -> ()))] in
  ()
;;
[%%expect{|
val x : int list = [1; 2; 3]
|}]

(* Type error: [@then_call print_int] rejected because print_int : int -> unit
   is not polymorphic in its argument *)
let _ = 1 [@then_call print_int];;
[%%expect{|
Line 1, characters 22-31:
1 | let _ = 1 [@then_call print_int];;
                          ^^^^^^^^^
Error: The function argument to "[@then_call]" must have type "'a -> unit" for all "'a",
       but here it has type "int -> unit".
|}]

(* Type error: [@then_call Fun.id] rejected because Fun.id : 'a -> 'a
   does not return unit *)
let _ = 1 [@then_call Fun.id];;
[%%expect{|
Line 1, characters 22-28:
1 | let _ = 1 [@then_call Fun.id];;
                          ^^^^^^
Error: The function argument to "[@then_call]" must have type "'a -> unit" for all "'a",
       but here it has type "'a -> 'a".
|}]
