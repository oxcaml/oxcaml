(* TEST
   expect;
   flags += "-no-mutable-implied-modalities";
*)

type 'a atomic = { mutable contents : 'a [@atomic] }
[%%expect{|
type 'a atomic = { mutable contents : 'a [@atomic]; }
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
Line 2, characters 41-42:
2 |   [%atomic.loc t.contents], [%atomic.loc t.contents]
                                             ^
Error: This value is used here, but it is already being used as unique:
Line 2, characters 15-16:
2 |   [%atomic.loc t.contents], [%atomic.loc t.contents]
                   ^

|}]

let aliased_locs (t @ unique) =
  [%atomic.loc t.contents], [%atomic.loc t.contents]
[%%expect{|
val aliased_locs : 'a atomic @ unique -> 'a atomic_loc * 'a atomic_loc =
  <fun>
|}]

type ('a, 'b) two_fields =
  { mutable a : 'a [@atomic]; mutable b : 'b [@atomic] }
[%%expect{|
type ('a, 'b) two_fields = {
  mutable a : 'a [@atomic];
  mutable b : 'b [@atomic];
}
|}]

let locs_to_two_fields_in_unique_record (t : (_, _) two_fields @ unique) =
  [%atomic.loc t.a], [%atomic.loc t.b]
[%%expect{|
val locs_to_two_fields_in_unique_record :
  ('a, 'b) two_fields @ unique -> 'a atomic_loc * 'b atomic_loc = <fun>
|}]

type ('a, 'b, 'c) two_atomic_fields_and_a_regular_one =
  { mutable a : 'a [@atomic]; mutable b : 'b [@atomic]; c : 'c }

let consume_all_fields
      (t : (_, _, _) two_atomic_fields_and_a_regular_one @ unique) =
  [%atomic.loc t.a], [%atomic.loc t.b], t.c
[%%expect{|
type ('a, 'b, 'c) two_atomic_fields_and_a_regular_one = {
  mutable a : 'a [@atomic];
  mutable b : 'b [@atomic];
  c : 'c;
}
val consume_all_fields :
  ('a, 'b, 'c) two_atomic_fields_and_a_regular_one @ unique ->
  'a atomic_loc * 'b atomic_loc * 'c = <fun>
|}]

let atomic_loc_consumes_record (t : _ atomic @ unique) : _ @ unique =
  [%atomic.loc t.contents], t
[%%expect{|
Line 2, characters 28-29:
2 |   [%atomic.loc t.contents], t
                                ^
Error: This value is used here, but it is already being used as unique:
Line 2, characters 15-16:
2 |   [%atomic.loc t.contents], t
                   ^

|}]

(* Test for forbidding non-legacy comonadic modalities in [%atomic.loc] *)

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
Error: Modalities are not allowed on fields given to "[%atomic.loc]" (here, "contents")
|}]

(* This is allowed... *)
type 'a local_atomic = { mutable contents : 'a @@ local [@atomic] }
[%%expect{|
type 'a local_atomic = { mutable contents : 'a @@ local [@atomic]; }
|}]

(* ...but you can't make an [%atomic.loc] to the field *)
let foo (t : _ local_atomic) = [%atomic.loc t.contents]
[%%expect{|
Line 1, characters 31-55:
1 | let foo (t : _ local_atomic) = [%atomic.loc t.contents]
                                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modalities are not allowed on fields given to "[%atomic.loc]" (here, "contents")
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
Error: Modalities are not allowed on fields given to "[%atomic.loc]" (here, "contents")
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
Error: Modalities are not allowed on fields given to "[%atomic.loc]" (here, "contents")
|}]
