(* TEST
 flags = "-w -220";
 expect;
*)

(* A uniquely-held value has no other live reference, so nothing can contend for
   it; a [unique contended] value may therefore be used where [uncontended] is
   required, for the type shapes where unique ownership is what makes uncontended
   use sound. This is the modes-level (spec-numbered) companion to the
   ikind-level suite in
   testsuite/tests/typing-jkind-bounds/unique_implies_uncontended_ikinds.ml. *)

let use_unique      : 'a @ unique      -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable    : 'a @ portable    -> unit = fun _ -> ()
[%%expect{|
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
|}]

(* Shared type declarations. *)
type ref_record = { ref_field : int ref }
type abstract_contended : value mod contended
type abstract_value : value
type abstract_aliased : value mod aliased
type abstract_mutable_data : mutable_data
type crosses_contention_payload = int option
type aliased_many_contention_crossing_field = { xa : int option @@ aliased many }
type inner_aliased_cc = { xi : int option @@ aliased }
type outer_with_aliased_contention_crossing = { yo : inner_aliased_cc }
type aliased_crossing_payload = { xb : crosses_contention_payload @@ aliased }
type unique_crossing_payload = { xc : crosses_contention_payload @@ unique }
type mutable_int_field = { mutable xm : int }
type 'a contended_mutable_box = { mutable contents_c : 'a @@ contended }
type atomic_ref_field = { mutable xar : int ref [@atomic] }
type atomic_int_field = { mutable xai : int [@atomic] }
type inner_ref = { ir : int ref }
type outer_atomic_ref_field = { mutable xoa : inner_ref [@atomic] }
type aliased_ref_field = { xr : int ref @@ aliased }
type aliased_many_ref_field = { xrm : int ref @@ aliased many }
type unique_abstract_payload = { xu : abstract_value @@ unique }
type mutable_ref_field = { mutable xmr : int ref }
type 'a mutable_box = { mutable contents_m : 'a }
type outer_with_aliased_ref = { wo : aliased_ref_field }
type inner_m = { mutable xim : int ref }
type outer_mutable_ref_field = { mutable ym : inner_m }
type exist_row = Mk : ([> `A | `B of int ref ] as 'a) -> exist_row
[%%expect{|
type ref_record = { ref_field : int ref; }
type abstract_contended : value mod contended
type abstract_value
type abstract_aliased : value mod aliased
type abstract_mutable_data : mutable_data
type crosses_contention_payload = int option
type aliased_many_contention_crossing_field = {
  xa : int option @@ many aliased;
}
type inner_aliased_cc = { xi : int option @@ aliased; }
type outer_with_aliased_contention_crossing = { yo : inner_aliased_cc; }
type aliased_crossing_payload = {
  xb : crosses_contention_payload @@ aliased;
}
type unique_crossing_payload = { xc : crosses_contention_payload; }
type mutable_int_field = { mutable xm : int; }
type 'a contended_mutable_box = { mutable contents_c : 'a @@ contended; }
type atomic_ref_field = { mutable xar : int ref [@atomic]; }
type atomic_int_field = { mutable xai : int [@atomic]; }
type inner_ref = { ir : int ref; }
type outer_atomic_ref_field = { mutable xoa : inner_ref [@atomic]; }
type aliased_ref_field = { xr : int ref @@ aliased; }
type aliased_many_ref_field = { xrm : int ref @@ many aliased; }
type unique_abstract_payload = { xu : abstract_value; }
type mutable_ref_field = { mutable xmr : int ref; }
type 'a mutable_box = { mutable contents_m : 'a; }
type outer_with_aliased_ref = { wo : aliased_ref_field; }
type inner_m = { mutable xim : int ref; }
type outer_mutable_ref_field = { mutable ym : inner_m; }
type exist_row = Mk : [> `A | `B of int ref ] -> exist_row
|}]

(* ---------- Accepted: unique -> uncontended granted (spec section 3) ---------- *)

(* 1 *)
let f (x : int ref @ unique contended) = use_uncontended x
[%%expect{|
val f : int ref @ unique contended -> unit = <fun>
|}]

(* 2 *)
let f (x : ref_record @ unique contended) = use_uncontended x
[%%expect{|
val f : ref_record @ unique contended -> unit = <fun>
|}]

(* 3 *)
let f (x : abstract_contended @ unique contended) = use_uncontended x
[%%expect{|
val f : abstract_contended @ unique contended -> unit = <fun>
|}]

(* 4 *)
let f (x : aliased_many_contention_crossing_field @ unique contended) =
  use_uncontended x
[%%expect{|
val f : aliased_many_contention_crossing_field @ unique contended -> unit =
  <fun>
|}]

(* 5 *)
let f (x : outer_with_aliased_contention_crossing @ unique contended) =
  use_uncontended x
[%%expect{|
val f : outer_with_aliased_contention_crossing @ unique contended -> unit =
  <fun>
|}]

(* 6 *)
let f (x : aliased_crossing_payload @ unique contended) = use_uncontended x
[%%expect{|
val f : aliased_crossing_payload @ unique contended -> unit = <fun>
|}]

(* 7 *)
let f (x : unique_crossing_payload @ unique contended) = use_uncontended x
[%%expect{|
val f : unique_crossing_payload @ unique contended -> unit = <fun>
|}]

(* 8 *)
let f (x : mutable_int_field @ unique contended) = use_uncontended x
[%%expect{|
val f : mutable_int_field @ unique contended -> unit = <fun>
|}]

(* 9 *)
let f (x : abstract_value contended_mutable_box @ unique contended) =
  use_uncontended x
[%%expect{|
val f : abstract_value contended_mutable_box @ unique contended -> unit =
  <fun>
|}]

(* 10 *)
let f (x : atomic_ref_field @ unique contended) = use_uncontended x
[%%expect{|
val f : atomic_ref_field @ unique contended -> unit = <fun>
|}]

(* 11 *)
let f (x : atomic_int_field @ unique contended) = use_uncontended x
[%%expect{|
val f : atomic_int_field @ unique contended -> unit = <fun>
|}]

(* 12 *)
let f (x : outer_atomic_ref_field @ unique contended) = use_uncontended x
[%%expect{|
val f : outer_atomic_ref_field @ unique contended -> unit = <fun>
|}]

(* 13 *)
let f (x : [< `A of int | `B of string] @ unique contended) = use_uncontended x
[%%expect{|
val f : [< `A of int | `B of string ] @ unique contended -> unit = <fun>
|}]

(* 14 *)
let f (x : exist_row @ unique contended) = use_uncontended x
[%%expect{|
val f : exist_row @ unique contended -> unit = <fun>
|}]

(* ---------- Rejected: broad / abstract types never qualify (16-17) ---------- *)

(* 16 *)
let f (x : abstract_mutable_data @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 71-72:
1 | let f (x : abstract_mutable_data @ unique contended) = use_uncontended x
                                                                           ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 17 *)
let f (x : abstract_value @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 64-65:
1 | let f (x : abstract_value @ unique contended) = use_uncontended x
                                                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let f (x : abstract_value @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 57-58:
1 | let f (x : abstract_value @ contended) = use_uncontended x
                                                             ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* ---------- Killer direction: non-unique must NOT gain uncontended (18-20) ---------- *)

(* 18 *)
let f (x : int ref @ aliased contended) = use_uncontended x
[%%expect{|
Line 1, characters 58-59:
1 | let f (x : int ref @ aliased contended) = use_uncontended x
                                                              ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 19 *)
let f (x : ref_record @ aliased contended) = use_uncontended x
[%%expect{|
Line 1, characters 61-62:
1 | let f (x : ref_record @ aliased contended) = use_uncontended x
                                                                 ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 20 *)
let f (x : outer_with_aliased_ref @ aliased contended) = use_uncontended x
[%%expect{|
Line 1, characters 73-74:
1 | let f (x : outer_with_aliased_ref @ aliased contended) = use_uncontended x
                                                                             ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* ---------- Artificial uniqueness is not enough (21-24) ---------- *)

(* 21 *)
let f (x : abstract_aliased @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 66-67:
1 | let f (x : abstract_aliased @ unique contended) = use_uncontended x
                                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 21, positive control: the type does cross uniqueness, accepted by use_unique *)
let f (x : abstract_aliased @ aliased) = use_unique x
[%%expect{|
val f : abstract_aliased -> unit = <fun>
|}]

(* 22 *)
let f (x : aliased_ref_field @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 67-68:
1 | let f (x : aliased_ref_field @ unique contended) = use_uncontended x
                                                                       ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 23 *)
let f (x : aliased_many_ref_field @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 72-73:
1 | let f (x : aliased_many_ref_field @ unique contended) = use_uncontended x
                                                                            ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 24 *)
let f (x : unique_abstract_payload @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 73-74:
1 | let f (x : unique_abstract_payload @ unique contended) = use_uncontended x
                                                                             ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* ---------- Reference cells with non-contention-safe payloads (25-30) ---------- *)

(* 25 *)
let f (x : int ref ref @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 61-62:
1 | let f (x : int ref ref @ unique contended) = use_uncontended x
                                                                 ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 26 *)
let f (x : mutable_ref_field @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 67-68:
1 | let f (x : mutable_ref_field @ unique contended) = use_uncontended x
                                                                       ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 27 *)
let f (x : abstract_value mutable_box @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 76-77:
1 | let f (x : abstract_value mutable_box @ unique contended) = use_uncontended x
                                                                                ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 28 *)
let f (x : outer_with_aliased_ref @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 72-73:
1 | let f (x : outer_with_aliased_ref @ unique contended) = use_uncontended x
                                                                            ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 29 *)
let f (x : outer_mutable_ref_field @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 73-74:
1 | let f (x : outer_mutable_ref_field @ unique contended) = use_uncontended x
                                                                             ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* 30 *)
let f (x : mutable_ref_field ref @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 71-72:
1 | let f (x : mutable_ref_field ref @ unique contended) = use_uncontended x
                                                                           ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* ---------- Unrelated-axis guard: does not leak to portability (31) ---------- *)

(* 31 *)
let f (x : (int -> int) ref @ unique nonportable) = use_portable x
[%%expect{|
Line 1, characters 65-66:
1 | let f (x : (int -> int) ref @ unique nonportable) = use_portable x
                                                                     ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* ---------- Behavior-neutrality guards: @ contended (no unique) unchanged (section 6) ---------- *)

type 'a box = { bc : 'a }
type ('a, 'b) box2 = { b2a : 'a; b2b : 'b @@ contended }
[%%expect{|
type 'a box = { bc : 'a; }
type ('a, 'b) box2 = { b2a : 'a; b2b : 'b @@ contended; }
|}]

let f (x : int box @ contended) = use_uncontended x
[%%expect{|
val f : int box @ contended -> unit = <fun>
|}]

let f (x : (int, int ref) box2 @ contended) = use_uncontended x
[%%expect{|
val f : (int, int ref) box2 @ contended -> unit = <fun>
|}]

let f (x : int ref box @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 54-55:
1 | let f (x : int ref box @ contended) = use_uncontended x
                                                          ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let f (x : (int ref, int ref) box2 @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 66-67:
1 | let f (x : (int ref, int ref) box2 @ contended) = use_uncontended x
                                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let f (x : (int ref, int) box2 @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 62-63:
1 | let f (x : (int ref, int) box2 @ contended) = use_uncontended x
                                                                  ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* ---------- Known false-negatives (spec section 7): default uniqueness not
   visible at the check point, so REJECTED today; the explicit "@ unique"
   forms above are accepted. ---------- *)

let f (x : int ref @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 50-51:
1 | let f (x : int ref @ contended) = use_uncontended x
                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let f (x : ref_record @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 53-54:
1 | let f (x : ref_record @ contended) = use_uncontended x
                                                         ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let f (x : exist_row @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 52-53:
1 | let f (x : exist_row @ contended) = use_uncontended x
                                                        ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* ---------- Exception-constructor interaction (spec section 3, example 15):
   raising an exception whose argument is a freshly-owned (unique) ref inside a
   portable function is ACCEPTED.  The kind/ikind UIC predicate carries the
   permission into exception-constructor arguments, so this shape now type
   checks.  (This is the one spec-accepted case a purely structural type-walk
   predicate misses; the kind-based predicate handles it.) ---------- *)

exception Contended of int ref
let (foo @ portable) () = raise (Contended (ref 42))
[%%expect{|
exception Contended of int ref
val foo : unit -> 'a = <fun>
|}]

(* ---------- Boxed tuples (a uniquely-held tuple reads each immutable element at
   its own uniqueness, exactly like immutable record fields).  These pin the
   boxed-tuple handling of the UIC predicate and the killer direction on it. ---------- *)

(* [int ref * int]: element [int ref] is UIC-safe under unique, [int] crosses
   contention -- accepted, just as [ref_record]/[mutable_int_field] are. *)
let f (x : (int ref * int) @ unique contended) = use_uncontended x
[%%expect{|
val f : int ref * int @ unique contended -> unit = <fun>
|}]

(* Killer direction: an [aliased] tuple must NOT gain uncontended. *)
let f (x : (int ref * int) @ aliased contended) = use_uncontended x
[%%expect{|
Line 1, characters 66-67:
1 | let f (x : (int ref * int) @ aliased contended) = use_uncontended x
                                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* A ref whose contents are themselves a ref is not UIC-safe, so a tuple
   exposing one is rejected even when unique. *)
let f (x : (int ref ref * int) @ unique contended) = use_uncontended x
[%%expect{|
Line 1, characters 69-70:
1 | let f (x : (int ref ref * int) @ unique contended) = use_uncontended x
                                                                         ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* ---------- Killer-direction negatives for the atomic / row / existential
   shapes (pin that a future precision change cannot silently start accepting
   the [aliased] variants). ---------- *)

let f (x : atomic_ref_field @ aliased contended) = use_uncontended x
[%%expect{|
Line 1, characters 67-68:
1 | let f (x : atomic_ref_field @ aliased contended) = use_uncontended x
                                                                       ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let f (x : [< `A of int | `B of string] @ aliased contended) = use_uncontended x
[%%expect{|
Line 1, characters 79-80:
1 | let f (x : [< `A of int | `B of string] @ aliased contended) = use_uncontended x
                                                                                   ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let f (x : exist_row @ aliased contended) = use_uncontended x
[%%expect{|
Line 1, characters 60-61:
1 | let f (x : exist_row @ aliased contended) = use_uncontended x
                                                                ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* An [@atomic] field does NOT cross the uniqueness axis, which is what makes the
   atomic UIC exception sound: an [aliased] atomic value may not be used
   [unique]. *)
let f (x : atomic_ref_field @ aliased) = use_unique x
[%%expect{|
Line 1, characters 52-53:
1 | let f (x : atomic_ref_field @ aliased) = use_unique x
                                                        ^
Error: This value is "aliased" but is expected to be "unique".
|}]
