(* TEST
 expect;
*)

(* Warning 222: the [@table] attribute cannot be honored. *)

let huge x = match[@table] x with 0 -> 1 | 1_000_000_000 -> 2 | _ -> 0
[%%expect {|
Line 1, characters 34-35:
1 | let huge x = match[@table] x with 0 -> 1 | 1_000_000_000 -> 2 | _ -> 0
                                      ^
Warning 222 [match-table-unsupported]: the "[@table]" attribute could not be honored: the range of the constants is too large (0 to 1000000000)

val huge : int -> int = <fun>
|}]

let strings x = match[@table] x with "a" -> 1 | "b" -> 2 | _ -> 0
[%%expect {|
Line 1, characters 37-40:
1 | let strings x = match[@table] x with "a" -> 1 | "b" -> 2 | _ -> 0
                                         ^^^
Warning 222 [match-table-unsupported]: the "[@table]" attribute could not be honored: a match on string constants cannot be dispatched through a table

val strings : string -> int = <fun>
|}]

(* No warning when the attribute is honored. *)
let ok x = match[@table] x with 0 -> 1 | 7 -> 2 | 15 -> 3 | _ -> 0
[%%expect {|
val ok : int -> int = <fun>
|}]
