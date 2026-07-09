(* TEST
 flags = "-no-ikinds";
 expect;
*)

(* Unboxed versions of records with mutable fields. An unboxed record has no
   heap identity to mutate through, so field mutability does not affect its
   kind, unlike the boxed version. The boxed and unboxed forms therefore have
   different kinds, and both kind engines must agree on them (see the
   [_ikinds] variant, which runs this same file with ikinds enabled). *)

type t = { mutable s : string }
[%%expect{|
type t = { mutable s : string; }
|}]

(* The boxed record is [mutable_data] and does not cross contention. *)
type t_boxed : mutable_data = t
[%%expect{|
type t_boxed = t
|}]

type t_boxed_bad : value mod contended = t
[%%expect{|
Line 1, characters 0-42:
1 | type t_boxed_bad : value mod contended = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because of the definition of t at line 1, characters 0-31.
       But the kind of type "t" must be a subkind of value mod contended
         because of the definition of t_boxed_bad at line 1, characters 0-42.
|}]

(* The unboxed version ignores mutability: it is [immutable_data with string]
   and does cross contention. *)
type t_unboxed : immutable_data with string = t#
[%%expect{|
type t_unboxed = t#
|}]

type t_unboxed_contended : value mod contended = t#
[%%expect{|
type t_unboxed_contended = t#
|}]

(* A mutable immediate field gives an immediate unboxed version. *)
type m = { mutable x : int }
type m_unboxed : immediate = m#
[%%expect{|
type m = { mutable x : int; }
type m_unboxed = m#
|}]

(* Parameterized: the unboxed version's crossing follows the argument. *)
type 'a r = { mutable a : 'a }
type r_int : immediate = int r#
[%%expect{|
type 'a r = { mutable a : 'a; }
type r_int = int r#
|}]

type r_string : immutable_data with string = string r#
[%%expect{|
type r_string = string r#
|}]
