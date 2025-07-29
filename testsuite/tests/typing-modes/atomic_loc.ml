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

let atomic_loc_portable (t @ portable) : _ @ portable = [%atomic.loc t.contents]
[%%expect{|
val atomic_loc_portable : 'a atomic @ portable -> 'a atomic_loc @ portable =
  <fun>
|}]

let uses_unique (t @ unique) : _ @ unique =
  [%atomic.loc t.contents], [%atomic.loc t.contents]
(* This is allowed because atomic mutable implies aliased *)
[%%expect{|
val uses_unique :
  'a atomic @ unique -> 'a atomic_loc * 'a atomic_loc @ unique = <fun>
|}]

(* Test for forbidding non-identity comonadic modalities in [%atomic.loc] *)

(* This is allowed... *)
type 'a portable_atomic = { mutable contents : 'a @@ portable [@atomic] }
[%%expect{|
type 'a portable_atomic = { mutable contents : 'a @@ portable [@atomic]; }
|}]

(* ...but you can't make an [%atomic.loc] to the field *)
let foo (t : _ portable_atomic) = [%atomic.loc t.contents]
[%%expect{|
Line 1, characters 34-58:
1 | let foo (t : _ portable_atomic) = [%atomic.loc t.contents]
                                      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Fields with modalities are not allowed in "[%atomic.loc]"
|}]

(* This is allowed... *)
type 'a local_atomic = { mutable contents : 'a @@ global [@atomic] }
[%%expect{|
type 'a local_atomic = { mutable global_ contents : 'a [@atomic]; }
|}]

(* ...but you can't make an [%atomic.loc] to the field *)
let foo (t : _ local_atomic) = [%atomic.loc t.contents]
[%%expect{|
Line 1, characters 31-55:
1 | let foo (t : _ local_atomic) = [%atomic.loc t.contents]
                                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Fields with modalities are not allowed in "[%atomic.loc]"
|}]

(* Test for forbidding non-legacy monadic modalities in [%atomic.loc] *)

(* This is allowed... *)
type 'a aliased_atomic = { mutable contents : 'a @@ unique [@atomic] }
[%%expect{|
type 'a aliased_atomic = { mutable contents : 'a @@ unique [@atomic]; }
|}]

(* ...but you can't make an [%atomic.loc] to the field *)
let foo (t : _ aliased_atomic) = [%atomic.loc t.contents]
[%%expect{|
Line 1, characters 33-57:
1 | let foo (t : _ aliased_atomic) = [%atomic.loc t.contents]
                                     ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Fields with modalities are not allowed in "[%atomic.loc]"
|}]

(* This is allowed... *)
type 'a contended_atomic = { mutable contents : 'a @@ contended [@atomic] }
[%%expect{|
type 'a contended_atomic = { mutable contents : 'a @@ contended [@atomic]; }
|}]

(* ...but you can't make an [%atomic.loc] to the field *)
let foo (t : _ contended_atomic) = [%atomic.loc t.contents]
[%%expect{|
Line 1, characters 35-59:
1 | let foo (t : _ contended_atomic) = [%atomic.loc t.contents]
                                       ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Fields with modalities are not allowed in "[%atomic.loc]"
|}]
