(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

type unchanged =
  | U0 [@tag 0]
  | U1

[%%expect {|
type unchanged = U0 [@tag 0] | U1
|}]

type changed =
  | C0 [@tag 1]
  | C1

[%%expect {|
Line 2, characters 7-15:
2 |   | C0 [@tag 1]
           ^^^^^^^^
Warning 187 [incompatible-with-upstream]: This [@tag] constructor tag changes runtime representation
  and is not upstream compatible.

type changed = C0 [@tag 1] | C1
|}]

type later_changed =
  | L0 [@tag 0]
  | L1 [@tag 2]
  | L2

[%%expect {|
Line 3, characters 7-15:
3 |   | L1 [@tag 2]
           ^^^^^^^^
Warning 187 [incompatible-with-upstream]: This [@tag] constructor tag changes runtime representation
  and is not upstream compatible.

type later_changed = L0 [@tag 0] | L1 [@tag 2] | L2
|}]

type negative_changed =
  | N0 [@tag (-1)]
  | N1

[%%expect {|
Line 2, characters 7-18:
2 |   | N0 [@tag (-1)]
           ^^^^^^^^^^^
Warning 187 [incompatible-with-upstream]: This [@tag] constructor tag changes runtime representation
  and is not upstream compatible.

type negative_changed = N0 [@tag -1] | N1
|}]
