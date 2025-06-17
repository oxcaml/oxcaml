(* TEST
   expect;
*)

type 'a atomic = { mutable contents : 'a @@ local [@atomic] }

[%%expect{|
type 'a atomic = { mutable contents : 'a @@ local [@atomic]; }
|}]

let contents_loc t = [%atomic.loc t.contents]
[%%expect{|
val contents_loc : 'a atomic -> 'a atomic_loc = <fun>
|}]

let contents_loc_local (t @ local) = exclave_ [%atomic.loc t.contents]
[%%expect{|
val contents_loc_local : local_ 'a atomic -> local_ 'a atomic_loc = <fun>
|}]

let regular_field_escape (t @ local) = t.contents
[%%expect{|
val regular_field_escape : local_ 'a atomic -> local_ 'a = <fun>
|}]


let contents_loc_escape (t @ local) = [%atomic.loc t.contents]
[%%expect{|
Line 1, characters 38-62:
1 | let contents_loc_escape (t @ local) = [%atomic.loc t.contents]
                                          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]
