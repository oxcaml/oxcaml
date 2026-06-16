(* TEST
   expect;
*)

let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
[%%expect{|
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
|}]

(****************************************)
(* The broad [value] and [mutable_data] kinds are not UIC. *)

type abstract_immutable_data : immutable_data
[%%expect{|
type abstract_immutable_data : immutable_data
|}]

(* [immutable_data] crosses contention directly, so this acceptance is not
   evidence that [immutable_data] has UIC. *)
let immutable_data_with_unique
    (x : abstract_immutable_data @ unique contended) =
  use_uncontended x
[%%expect{|
val immutable_data_with_unique :
  abstract_immutable_data @ unique contended -> unit = <fun>
|}]

type abstract_mutable_data : mutable_data
[%%expect{|
type abstract_mutable_data : mutable_data
|}]

let mutable_data_with_unique (x : abstract_mutable_data @ unique contended) =
  use_uncontended x
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type abstract_value : value
[%%expect{|
type abstract_value
|}]

let abstract_without_unique (x : abstract_value @ contended) =
  use_uncontended x
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let abstract_with_unique (x : abstract_value @ unique contended) =
  use_uncontended x
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(*************************************)
(* Ref-shaped data is UIC. *)

let int_ref_with_default_unique (x : int ref @ contended) =
  use_uncontended x
[%%expect{|
val int_ref_with_default_unique : int ref @ contended -> unit = <fun>
|}]

let int_ref_with_unique (x : int ref @ unique contended) =
  use_uncontended x
[%%expect{|
val int_ref_with_unique : int ref @ unique contended -> unit = <fun>
|}]

let int_ref_with_aliased (x : int ref @ aliased contended) =
  use_uncontended x
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" because it crosses with something
         which is "aliased,contended,read_write,static".
       However, the highlighted expression is expected to be "uncontended".
|}]

type ref_record = { ref_field : int ref }
[%%expect{|
type ref_record = { ref_field : int ref; }
|}]

let ref_record_with_default_unique (x : ref_record @ contended) =
  use_uncontended x
[%%expect{|
val ref_record_with_default_unique : ref_record @ contended -> unit = <fun>
|}]

let ref_record_with_unique (x : ref_record @ unique contended) =
  use_uncontended x
[%%expect{|
val ref_record_with_unique : ref_record @ unique contended -> unit = <fun>
|}]

let ref_record_with_aliased (x : ref_record @ aliased contended) =
  use_uncontended x
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" because it crosses with something
         which is "aliased,contended,read_write,static".
       However, the highlighted expression is expected to be "uncontended".
|}]

(********************************************************)
(* Artificial uniqueness from [@@ aliased] is not enough. *)

type abstract_aliased : value mod aliased
[%%expect{|
type abstract_aliased : value mod aliased
|}]

let abstract_aliased_crosses_uniqueness (x : abstract_aliased @ aliased) =
  use_unique x
[%%expect{|
val abstract_aliased_crosses_uniqueness : abstract_aliased -> unit = <fun>
|}]

let abstract_aliased_does_not_imply_uncontended
    (x : abstract_aliased @ unique contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type aliased_ref_field = { x : int ref @@ aliased }
[%%expect{|
type aliased_ref_field = { x : int ref @@ aliased; }
|}]

let aliased_field_does_not_imply_uncontended
    (x : aliased_ref_field @ unique contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(*************************************************************)
(* A contention modality allows UIC regardless of its payload. *)

type abstract_contended : value mod contended
[%%expect{|
type abstract_contended : value mod contended
|}]

let abstract_contended_crosses_contention
    (x : abstract_contended @ contended) =
  use_uncontended x
[%%expect{|
val abstract_contended_crosses_contention :
  abstract_contended @ contended -> unit = <fun>
|}]

let abstract_contended_with_unique (x : abstract_contended @ unique contended) =
  use_uncontended x
[%%expect{|
val abstract_contended_with_unique :
  abstract_contended @ unique contended -> unit = <fun>
|}]

type inner_aliased_ref = { x : int ref @@ aliased }
type outer_with_aliased_ref = { x : inner_aliased_ref }
[%%expect{|
type inner_aliased_ref = { x : int ref @@ aliased; }
type outer_with_aliased_ref = { x : inner_aliased_ref; }
|}]

(* The nested [@@ aliased] field stores an [int ref], which does not cross
   contention directly. It does not make the outer record UIC. *)
let outer_with_aliased_ref (x : outer_with_aliased_ref @ unique contended) =
  use_uncontended x
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let outer_aliased_does_not_imply_uncontended
    (x : outer_with_aliased_ref @ aliased contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type inner_aliased_contention_crossing = { x : int option @@ aliased }
type outer_with_aliased_contention_crossing =
  { x : inner_aliased_contention_crossing }
[%%expect{|
type inner_aliased_contention_crossing = { x : int option @@ aliased; }
type outer_with_aliased_contention_crossing = {
  x : inner_aliased_contention_crossing;
}
|}]

let outer_with_aliased_contention_crossing
    (x : outer_with_aliased_contention_crossing @ unique contended) =
  use_uncontended x
[%%expect{|
val outer_with_aliased_contention_crossing :
  outer_with_aliased_contention_crossing @ unique contended -> unit = <fun>
|}]

(************************************************************)
(* [@@ unique] does not enable UIC for abstract contents. *)

type unique_abstract_payload = { x : abstract_value @@ unique }
[%%expect{|
type unique_abstract_payload = { x : abstract_value; }
|}]

let unique_payload_is_uncontended
    (x : unique_abstract_payload @ unique contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(****************************************************)
(* A uniqueness modality preserves existing crossing. *)

type crosses_contention_payload = int option
type aliased_crossing_payload = { x : crosses_contention_payload @@ aliased }
type unique_crossing_payload = { x : crosses_contention_payload @@ unique }
[%%expect{|
type crosses_contention_payload = int option
type aliased_crossing_payload = {
  x : crosses_contention_payload @@ aliased;
}
type unique_crossing_payload = { x : crosses_contention_payload; }
|}]

let aliased_payload_still_crosses_contention
    (x : aliased_crossing_payload @ contended) =
  use_uncontended x
[%%expect{|
val aliased_payload_still_crosses_contention :
  aliased_crossing_payload @ contended -> unit = <fun>
|}]

let aliased_payload_with_unique_is_uncontended
    (x : aliased_crossing_payload @ unique contended) =
  use_uncontended x
[%%expect{|
val aliased_payload_with_unique_is_uncontended :
  aliased_crossing_payload @ unique contended -> unit = <fun>
|}]

let unique_payload_still_crosses_contention
    (x : unique_crossing_payload @ contended) =
  use_uncontended x
[%%expect{|
val unique_payload_still_crosses_contention :
  unique_crossing_payload @ contended -> unit = <fun>
|}]

let unique_payload_with_unique_is_uncontended
    (x : unique_crossing_payload @ unique contended) =
  use_uncontended x
[%%expect{|
val unique_payload_with_unique_is_uncontended :
  unique_crossing_payload @ unique contended -> unit = <fun>
|}]

(******************************************************)
(* UIC strengthens contention, not unrelated mode axes. *)

let unique_does_not_imply_portable (x : (int -> int) ref @ unique nonportable) =
  use_portable x
[%%expect{|
Line 2, characters 15-16:
2 |   use_portable x
                   ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(***********************************)
(* Nested mutable reference shapes. *)

(* A reference's mutable contents behave like they are behind an implicit
   [@@ aliased] modality. Since [int ref] does not cross contention directly,
   [int ref ref] is not UIC. *)
let int_ref_ref_with_unique (x : int ref ref @ unique contended) =
  use_uncontended x
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type mutable_ref_field = { mutable x : int ref }
[%%expect{|
type mutable_ref_field = { mutable x : int ref; }
|}]

(* Like [int ref ref], mutable field contents are implicitly [@@ aliased]. *)
let mutable_ref_field_with_unique
    (x : mutable_ref_field @ unique contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type mutable_int_field = { mutable x : int }
[%%expect{|
type mutable_int_field = { mutable x : int; }
|}]

(* This is the positive mutable-field case: the implicitly aliased contents are
   [int], which crosses contention directly. *)
let mutable_int_field_with_unique
    (x : mutable_int_field @ unique contended) =
  use_uncontended x
[%%expect{|
val mutable_int_field_with_unique :
  mutable_int_field @ unique contended -> unit = <fun>
|}]

type ('a : value_or_null) mutable_box = { mutable contents : 'a }
[%%expect{|
type ('a : value_or_null) mutable_box = { mutable contents : 'a; }
|}]

let mutable_box_with_abstract_value
    (x : abstract_value mutable_box @ unique contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type ('a : value_or_null) contended_mutable_box =
  { mutable contents : 'a @@ contended }
[%%expect{|
type ('a : value_or_null) contended_mutable_box = {
  mutable contents : 'a @@ contended;
}
|}]

let contended_mutable_box_with_abstract_value
    (x : abstract_value contended_mutable_box @ unique contended) =
  use_uncontended x
[%%expect{|
val contended_mutable_box_with_abstract_value :
  abstract_value contended_mutable_box @ unique contended -> unit = <fun>
|}]

type outer_mutable_ref_field = { mutable x : mutable_ref_field }
[%%expect{|
type outer_mutable_ref_field = { mutable x : mutable_ref_field; }
|}]

(* The mutable field stores a record whose mutable contents eventually store
   [int ref]. *)
let outer_mutable_ref_field_with_unique
    (x : outer_mutable_ref_field @ unique contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* The reference stores [mutable_ref_field], whose mutable contents eventually
   store [int ref]. *)
let mutable_ref_field_ref_with_unique
    (x : mutable_ref_field ref @ unique contended) =
  use_uncontended x
[%%expect{|
Line 3, characters 18-19:
3 |   use_uncontended x
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

type atomic_ref_field = { mutable x : int ref [@atomic] }
[%%expect{|
type atomic_ref_field = { mutable x : int ref [@atomic]; }
|}]

(* Atomic fields cross contention directly, so the stored [int ref] does not
   disable UIC for the outer record. *)
let atomic_ref_field_with_unique
    (x : atomic_ref_field @ unique contended) =
  use_uncontended x
[%%expect{|
val atomic_ref_field_with_unique :
  atomic_ref_field @ unique contended -> unit = <fun>
|}]

type atomic_int_field = { mutable x : int [@atomic] }
[%%expect{|
type atomic_int_field = { mutable x : int [@atomic]; }
|}]

(* This is the positive atomic-field case: the stored [int] crosses contention
   directly. *)
let atomic_int_field_with_unique
    (x : atomic_int_field @ unique contended) =
  use_uncontended x
[%%expect{|
val atomic_int_field_with_unique :
  atomic_int_field @ unique contended -> unit = <fun>
|}]

type outer_atomic_ref_field = { mutable x : mutable_ref_field [@atomic] }
[%%expect{|
type outer_atomic_ref_field = { mutable x : mutable_ref_field [@atomic]; }
|}]

(* Atomic fields cross contention directly even when they store a mutable
   record. *)
let outer_atomic_ref_field_with_unique
    (x : outer_atomic_ref_field @ unique contended) =
  use_uncontended x
[%%expect{|
val outer_atomic_ref_field_with_unique :
  outer_atomic_ref_field @ unique contended -> unit = <fun>
|}]

(*********************************)
(* Row and existential examples. *)

let row_unique_implies_uncontended
    (x : [< `A of int | `B of string] @ unique contended) =
  use_uncontended x
[%%expect{|
val row_unique_implies_uncontended :
  [< `A of int | `B of string ] @ unique contended -> unit = <fun>
|}]

type exist_row = Mk : ([> `A | `B of int ref] as 'a) -> exist_row
[%%expect{|
type exist_row = Mk : [> `A | `B of int ref ] -> exist_row
|}]

let exist_row_without_unique (x : exist_row @ contended) =
  use_uncontended x
[%%expect{|
val exist_row_without_unique : exist_row @ contended -> unit = <fun>
|}]

let exist_row_with_unique (x : exist_row @ unique contended) =
  use_uncontended x
[%%expect{|
val exist_row_with_unique : exist_row @ unique contended -> unit = <fun>
|}]
