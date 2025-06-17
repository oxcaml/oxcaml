(* TEST
   expect;
   flags += "-no-mutable-implied-modalities";
*)

type 'a atomic = { mutable contents : 'a [@atomic] }
[%%expect{|
type 'a atomic = { mutable(<non-legacy>) contents : 'a; }
|}]

let atomic_loc_contended (t @ contended) = [%atomic.loc t.contents]
[%%expect{|
val atomic_loc_contended : 'a atomic @ contended -> 'a atomic_loc @ contended =
  <fun>
|}]

let atomic_loc_portable (t @ portable) : _ @ portable = [%atomic.loc t.contents]
[%%expect{|
val atomic_loc_portable : 'a atomic @ portable -> 'a atomic_loc @ portable =
  <fun>
|}]

let uses_unique (t @ unique) : _ @ unique =
  [%atomic.loc t.contents], [%atomic.loc t.contents]
[%%expect{|
Line 2, characters 2-26:
2 |   [%atomic.loc t.contents], [%atomic.loc t.contents]
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "aliased" but expected to be "unique".
|}]
