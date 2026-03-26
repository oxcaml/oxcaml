(* TEST
    expect;
*)

(*************)
(* TEST: kind *)

type 'a aliased_modality = { aliased : 'a @@ aliased }
[%%expect{|
type 'a aliased_modality = { aliased : 'a @@ aliased; }
|}]

type 'a contended_modality = { contended : 'a @@ contended }
[%%expect{|
type 'a contended_modality = { contended : 'a @@ contended; }
|}]

type mutable_record = { mutable x : int }
[%%expect{|
type mutable_record = { mutable x : int; }
|}]

(*************************************)
(* Types that should satisfy the axis *)

type good_int : value mod unique_implies_uncontended = int
(* CR jujacobs: Plain immutable data should satisfy this axis. *)
[%%expect{|
type good_int = int
|}]

type good_ref : value mod unique_implies_uncontended = int ref
(* CR jujacobs: Refs should satisfy this axis. *)
[%%expect{|
type good_ref = int ref
|}]

type good_mut : value mod unique_implies_uncontended = mutable_record
(* CR jujacobs: Mutable records should satisfy this axis. *)
[%%expect{|
type good_mut = mutable_record
|}]

type good_contended_int : value mod unique_implies_uncontended =
  int contended_modality
(* CR jujacobs: Explicitly contended immutable data should satisfy this axis. *)
[%%expect{|
type good_contended_int = int contended_modality
|}]

type good_contended_ref : value mod unique_implies_uncontended =
  int ref contended_modality
(* CR jujacobs: Explicitly contended refs should satisfy this axis. *)
[%%expect{|
type good_contended_ref = int ref contended_modality
|}]

type good_contended_mut : value mod unique_implies_uncontended =
  mutable_record contended_modality
(* CR jujacobs: Explicitly contended mutable records should satisfy this axis. *)
[%%expect{|
type good_contended_mut = mutable_record contended_modality
|}]

type good_aliased_contended_ref : value mod unique_implies_uncontended =
  int ref Modes.Aliased.t contended_modality
(* CR jujacobs: Explicitly contended aliased refs should satisfy this axis. *)
[%%expect{|
type good_aliased_contended_ref = int ref Modes.Aliased.t contended_modality
|}]

type good_aliased_contended_mut : value mod unique_implies_uncontended =
  mutable_record Modes.Aliased.t contended_modality
(* CR jujacobs: Explicitly contended aliased mutable records should satisfy this axis. *)
[%%expect{|
type good_aliased_contended_mut =
    mutable_record Modes.Aliased.t contended_modality
|}]

(*****************************************)
(* Types that should not satisfy the axis *)

type bad_ref : value mod unique_implies_uncontended = int ref Modes.Aliased.t
(* Control: Aliased refs should not satisfy this axis. *)
[%%expect{|
Line 1, characters 0-77:
1 | type bad_ref : value mod unique_implies_uncontended = int ref Modes.Aliased.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref Modes.Aliased.t" is value_or_null mod aliased.
       But the kind of type "int ref Modes.Aliased.t" must be a subkind of
           value mod unique_implies_uncontended
         because of the definition of bad_ref at line 1, characters 0-77.
|}]

type bad_mut : value mod unique_implies_uncontended =
  mutable_record Modes.Aliased.t
(* Control: Aliased mutable records should not satisfy this axis. *)
[%%expect{|
Lines 1-2, characters 0-32:
1 | type bad_mut : value mod unique_implies_uncontended =
2 |   mutable_record Modes.Aliased.t
Error: The kind of type "mutable_record Modes.Aliased.t" is
           value_or_null mod aliased.
       But the kind of type "mutable_record Modes.Aliased.t" must be a subkind of
         value mod unique_implies_uncontended
         because of the definition of bad_mut at lines 1-2, characters 0-32.
|}]

(**********************)
(* TEST: mode crossing *)

let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
[%%expect{|
val use_uncontended : 'a -> unit = <fun>
|}]

(*********************************)
(* Representative abstract kinds *)

module type Abstract_value = sig
  type t : value
end

module Cross_abstract_value (X : Abstract_value) = struct
  let f (x : X.t @ unique contended) = use_uncontended x
end
(* CR jujacobs: Abstract value types should cross to uncontended when unique. *)
[%%expect{|
module type Abstract_value = sig type t end
module Cross_abstract_value :
  functor (X : Abstract_value) ->
    sig val f : X.t @ unique contended -> unit end
|}]

module type Abstract_contended = sig
  type t : value mod contended
end

module Cross_abstract_contended (X : Abstract_contended) = struct
  let f (x : X.t @ unique contended) = use_uncontended x
end
[%%expect{|
module type Abstract_contended = sig type t : value mod contended end
module Cross_abstract_contended :
  functor (X : Abstract_contended) ->
    sig val f : X.t @ unique contended -> unit end
|}]

module type Abstract_unique = sig
  type t : value mod unique
end

module Cross_abstract_unique (X : Abstract_unique) = struct
  let f (x : X.t @ unique contended) = use_uncontended x
end
(* CR jujacobs: Abstract unique types should cross to uncontended when unique. *)
[%%expect{|
module type Abstract_unique = sig type t end
module Cross_abstract_unique :
  functor (X : Abstract_unique) ->
    sig val f : X.t @ unique contended -> unit end
|}]

module type Abstract_contended_unique = sig
  type t : value mod contended unique
end

module Cross_abstract_contended_unique
    (X : Abstract_contended_unique) =
struct
  let f (x : X.t @ unique contended) = use_uncontended x
end
[%%expect{|
module type Abstract_contended_unique = sig type t : value mod contended end
module Cross_abstract_contended_unique :
  functor (X : Abstract_contended_unique) ->
    sig val f : X.t @ unique contended -> unit end
|}]

let foo (t : int ref @ unique contended) =
  use_uncontended t
(* CR jujacobs: Unique refs should become uncontended. *)
[%%expect{|
val foo : int ref @ unique contended -> unit = <fun>
|}]

let foo (t : int ref Modes.Aliased.t @ unique contended) =
  use_uncontended t
(* Control: an aliased wrapper should not enable uncontended crossing. *)
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended t
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo (t : int ref Modes.Aliased.t contended_modality @ unique contended) =
  use_uncontended t
[%%expect{|
val foo :
  int ref Modes.Aliased.t contended_modality @ unique contended -> unit =
  <fun>
|}]

let foo (t : mutable_record @ unique contended) =
  use_uncontended t
(* CR jujacobs: Unique mutable records should become uncontended. *)
[%%expect{|
val foo : mutable_record @ unique contended -> unit = <fun>
|}]

let foo (t : mutable_record Modes.Aliased.t @ unique contended) =
  use_uncontended t
(* Control: an aliased wrapper should not enable uncontended crossing. *)
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended t
                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let foo
    (t :
       mutable_record Modes.Aliased.t contended_modality
       @ unique contended) =
  use_uncontended t
[%%expect{|
val foo :
  mutable_record Modes.Aliased.t contended_modality @ unique contended ->
  unit = <fun>
|}]
