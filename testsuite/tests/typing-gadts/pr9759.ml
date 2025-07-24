(* TEST
   expect;
*)

(* #9759 by Thomas Refis *)

type 'a general =
  { indir : 'a desc;
    unit : unit
  }

and 'a desc = C : unit general -> unit desc

[%%expect
{|
type 'a general = { indir : 'a desc; unit : unit; }
and 'a desc = C : unit general -> unit desc
|}]

let rec foo : type k. k general -> k general =
 fun g ->
  match g.indir with
  | C g' ->
    let new_g' = foo g' in
    if true then { g with indir = C new_g' } else new_g'
  | indir -> { g with indir }

[%%expect
{|
Line 7, characters 4-9:
7 |   | indir -> { g with indir }
        ^^^^^
Warning 11 [redundant-case]: this match case is unused.

val foo : 'k general -> 'k general = <fun>
|}]
