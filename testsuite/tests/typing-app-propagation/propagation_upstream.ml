(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* Expected-type propagation into applications is a non-erasable typing
   change, so it is disabled when only erasable extensions are allowed:
   these programs must be rejected exactly as upstream rejects them. *)

type t = A | B
type s = A | C

let id x = x

let _ = (id A : t), (id A : s)
[%%expect{|
type t = A | B
type s = A | C
val id : 'a -> 'a = <fun>
Line 6, characters 9-13:
6 | let _ = (id A : t), (id A : s)
             ^^^^
Error: This expression has type "s" but an expression was expected of type "t"
|}]
