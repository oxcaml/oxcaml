(* TEST
   expect;
*)

type 'a atomic = { mutable contents : 'a [@atomic] }

[%%expect{|
type 'a atomic = { mutable contents : 'a [@atomic]; }
|}]

let contents_loc t = [%atomic.loc t.contents]
[%%expect{|
val contents_loc : 'a atomic -> 'a atomic_loc = <fun>
|}]

let contents_loc_local (t @ local) = exclave_ [%atomic.loc t.contents]
[%%expect{|
val contents_loc_local : local_ 'a atomic -> local_ 'a atomic_loc = <fun>
|}]

(* This is allowed because mutable implies global *)
let regular_field_local (t : _ atomic @ local) = t.contents
[%%expect{|
val regular_field_local : local_ 'a atomic -> 'a = <fun>
|}]


let contents_loc_escape (t @ local) = [%atomic.loc t.contents]
[%%expect{|
Line 1, characters 38-62:
1 | let contents_loc_escape (t @ local) = [%atomic.loc t.contents]
                                          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]


let contents_can't_escape_by_mode_crossing
  : int atomic @ local -> int atomic_loc @ global
= fun t -> [%atomic.loc t.contents]
[%%expect{|
Line 3, characters 11-35:
3 | = fun t -> [%atomic.loc t.contents]
               ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]
