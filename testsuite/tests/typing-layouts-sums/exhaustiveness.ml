(* TEST
   flags = "-w +8+11";
   expect;
*)

type gate = Shut | Open of bool [@@unboxed]
[%%expect{|
type gate = Shut | Open of bool [@@unboxed]
|}]

let exhaustive = function
  | #(Shut, _) -> 0
  | #(Open false, _) -> 1
  | #(Open true, false) -> 2
  | #(Open true, true) -> 3
[%%expect{|
val exhaustive : #(gate * bool) -> int = <fun>
|}]

let incomplete = function
  | #(Shut, _) -> 0
  | #(Open false, _) -> 1
  | #(Open true, false) -> 2
[%%expect{|
Lines 1-4, characters 17-28:
1 | .................function
2 |   | #(Shut, _) -> 0
3 |   | #(Open false, _) -> 1
4 |   | #(Open true, false) -> 2
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "#(Open true, true)"

val incomplete : #(gate * bool) -> int = <fun>
|}]

let redundant = function
  | #(Open _, true) -> 0
  | #(Open false, true) -> 1
  | _ -> 2
[%%expect{|
Line 3, characters 4-23:
3 |   | #(Open false, true) -> 1
        ^^^^^^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val redundant : #(gate * bool) -> int = <fun>
|}]
