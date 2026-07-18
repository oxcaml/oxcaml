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
