(* TEST
 expect;
*)

let (~-) x = x + 1;;
- - 4;;
- - (4);;
- - (2 + 2);;
[%%expect{|
val ( ~- ) : int -> int = <fun>
- : int = 4
- : int = 4
- : int = 6
|}]


let _ = 4611686018427387904;;
let _ = 2147483648l;;
let _ = 9223372036854775808L;;
[%%expect{|
- : int = -4611686018427387904
- : int32 = -2147483648l
- : int64 = -9223372036854775808L
|}]


module Array = struct let get _ _ = "hello, world!" end
let _ = [| 1; 2; 3 |].(1)
[%%expect{|
module Array : sig val get : 'a -> 'b -> string end
- : string = "hello, world!"
|}]


let x =
  let x = ref 5 in
  (* " *)
  x := 10;
  (* " *)
  !x
;;
[%%expect{|
val x : int = 5
|}]
