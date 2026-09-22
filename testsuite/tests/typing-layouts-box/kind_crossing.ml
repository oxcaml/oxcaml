(* TEST
 {
   expect;
 }{
   flags = "-no-ikinds";
   expect;
 }
*)

(* Tests for the mode crossing of box kinds. *)

let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_global : 'a @ global -> unit = fun _ -> ()
[%%expect{|
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_global : 'a -> unit = <fun>
|}]

(**** [immediate box] crosses portability, but not contention (the kind
      does not rule out a mutable payload), externality, or locality ****)

module M : sig
  type t : value non_float mod portable
end = struct
  type t : immediate box
end
[%%expect{|
module M : sig type t : value non_float mod portable end
|}]

module M : sig
  type t : value non_float mod contended
end = struct
  type t : immediate box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immediate box end
       is not included in
         sig type t : value non_float mod contended end
       Type declarations do not match:
         type t : immediate box
       is not included in
         type t : value non_float mod contended
       The kind of the first is mutable_data
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of
           value non_float mod contended
         because of the definition of t at line 2, characters 2-40.
|}]

module M : sig
  type t : value non_float mod external_
end = struct
  type t : immediate box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immediate box end
       is not included in
         sig type t : value non_float mod external_ end
       Type declarations do not match:
         type t : immediate box
       is not included in
         type t : value non_float mod external_
       The kind of the first is mutable_data
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of
           value non_float mod external_
         because of the definition of t at line 2, characters 2-40.
|}]

module M : sig
  type t : value non_float mod global
end = struct
  type t : immediate box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immediate box end
       is not included in
         sig type t : value non_float mod global end
       Type declarations do not match:
         type t : immediate box
       is not included in
         type t : value non_float mod global
       The kind of the first is mutable_data
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of
           value non_float mod global
         because of the definition of t at line 2, characters 2-37.
|}]

(**** [value box] does not cross portability: the contents might not ****)

module M : sig
  type t : value non_float mod portable
end = struct
  type t : value box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : value box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value box end
       is not included in
         sig type t : value non_float mod portable end
       Type declarations do not match:
         type t : value box
       is not included in
         type t : value non_float mod portable
       The kind of the first is value non_float
         because of the definition of t at line 4, characters 2-20.
       But the kind of the first must be a subkind of
           value non_float mod portable
         because of the definition of t at line 2, characters 2-39.
|}]

(**** [(value mod portable) box] does ****)

module M : sig
  type t : value non_float mod portable
end = struct
  type t : (value mod portable) box
end
[%%expect{|
module M : sig type t : value non_float mod portable end
|}]

(**** [immediate box] crosses as [mutable_data] ****)

module M : sig
  type t : mutable_data
end = struct
  type t : immediate box
end
[%%expect{|
module M : sig type t : mutable_data end
|}]

(**** Box kinds also allow values to cross ****)

type t : immediate box
let cross (x : t @ nonportable) : _ @ portable = x
[%%expect{|
type t : immediate box
val cross : t -> t @ portable = <fun>
|}]

type u : value box
let no_cross (x : u @ nonportable) : _ @ portable = x
[%%expect{|
type u : value box
Line 2, characters 52-53:
2 | let no_cross (x : u @ nonportable) : _ @ portable = x
                                                        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(**** Payload crossing flows through, up to [mutable_data]'s ceiling ****)

module M : sig
  type t : value non_float mod portable
end = struct
  type t : (bits64 mod everything) box
end
[%%expect{|
module M : sig type t : value non_float mod portable end
|}]

module M : sig
  type t : value non_float mod contended
end = struct
  type t : (bits64 mod everything) box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : (bits64 mod everything) box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : (bits64 mod everything) box end
       is not included in
         sig type t : value non_float mod contended end
       Type declarations do not match:
         type t : (bits64 mod everything) box
       is not included in
         type t : value non_float mod contended
       The kind of the first is mutable_data
         because of the definition of t at line 4, characters 2-38.
       But the kind of the first must be a subkind of
           value non_float mod contended
         because of the definition of t at line 2, characters 2-40.
|}]

(**** With-bounds survive under [box] ****)

type 'a t : (immutable_data with 'a) box
[%%expect{|
type 'a t : mutable_data box with 'a
|}]

(* The box crosses portability when the payload does *)
type ('a : value mod portable) port_req
type ok = int t port_req
type bad = (int -> int) t port_req
[%%expect{|
type ('a : value mod portable) port_req
type ok = int t port_req
Line 3, characters 11-25:
3 | type bad = (int -> int) t port_req
               ^^^^^^^^^^^^^^
Error: This type "(int -> int) t" should be an instance of type
         "('a : value mod portable)"
       The kind of (int -> int) t is value non_float
         because of the definition of t at line 1, characters 0-40.
       But the kind of (int -> int) t must be a subkind of value mod portable
         because of the definition of port_req at line 1, characters 0-39.
|}, Principal{|
type ('a : value mod portable) port_req
Line 2, characters 10-15:
2 | type ok = int t port_req
              ^^^^^
Error: This type "int t" should be an instance of type
         "('a : value mod portable)"
       The kind of int t is mutable_data with int
         because of the definition of t at line 1, characters 0-40.
       But the kind of int t must be a subkind of value mod portable
         because of the definition of port_req at line 1, characters 0-39.
|}]

(* But never contention, unlike [immutable_data with _] *)
type ('a : value mod contended) cont_req
type bad = int t cont_req
[%%expect{|
type ('a : value mod contended) cont_req
Line 2, characters 11-16:
2 | type bad = int t cont_req
               ^^^^^
Error: This type "int t" should be an instance of type
         "('a : value mod contended)"
       The kind of int t is mutable_data
         because of the definition of t at line 1, characters 0-40.
       But the kind of int t must be a subkind of value mod contended
         because of the definition of cont_req at line 1, characters 0-40.
|}, Principal{|
type ('a : value mod contended) cont_req
Line 2, characters 11-16:
2 | type bad = int t cont_req
               ^^^^^
Error: This type "int t" should be an instance of type
         "('a : value mod contended)"
       The kind of int t is mutable_data with int
         because of the definition of t at line 1, characters 0-40.
       But the kind of int t must be a subkind of value mod contended
         because of the definition of cont_req at line 1, characters 0-40.
|}]

(* [immediate box] does not cross externality *)
type t : immediate box
type ('a : value mod external_) ext_req
type bad = t ext_req
[%%expect{|
type t : immediate box
type ('a : value mod external_) ext_req
Line 3, characters 11-12:
3 | type bad = t ext_req
               ^
Error: This type "t" should be an instance of type "('a : value mod external_)"
       The kind of t is mutable_data
         because of the definition of t at line 1, characters 0-22.
       But the kind of t must be a subkind of value mod external_
         because of the definition of ext_req at line 2, characters 0-39.
|}]

(**** The box type constructor crosses with its payload, except on the
      axes [mutable_data] withholds ****)

let f (x : int box @ nonportable) = use_portable x
[%%expect{|
val f : int box -> unit = <fun>
|}]

let f (x : int box @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 50-51:
1 | let f (x : int box @ contended) = use_uncontended x
                                                      ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* The box of a function does not cross portability *)
let f (x : (int -> int) box @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 58-59:
1 | let f (x : (int -> int) box @ nonportable) = use_portable x
                                                              ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Boxes never cross locality *)
let f (x : int box @ local) = use_global x
[%%expect{|
Line 1, characters 41-42:
1 | let f (x : int box @ local) = use_global x
                                             ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* The same, at the type level *)
type ('a : value mod portable) portable_req
type ok = int box portable_req
type also_ok = string box portable_req
type bad = (int -> int) box portable_req
[%%expect{|
type ('a : value mod portable) portable_req
type ok = int box portable_req
type also_ok = string box portable_req
Line 4, characters 11-27:
4 | type bad = (int -> int) box portable_req
               ^^^^^^^^^^^^^^^^
Error: This type "(int -> int) box" should be an instance of type
         "('a : value mod portable)"
       The kind of (int -> int) box is value non_float
         because it's a boxed type.
       But the kind of (int -> int) box must be a subkind of
           value mod portable
         because of the definition of portable_req at line 1, characters 0-43.
|}, Principal{|
type ('a : value mod portable) portable_req
Line 2, characters 10-17:
2 | type ok = int box portable_req
              ^^^^^^^
Error: This type "int box" should be an instance of type
         "('a : value mod portable)"
       The kind of int box is mutable_data with int
         because it's a boxed type.
       But the kind of int box must be a subkind of value mod portable
         because of the definition of portable_req at line 1, characters 0-43.
|}]
(* CR layouts v2.8: fix principal mode. Internal ticket 5111 *)

(**** Reducible box types get their boxed type's kind: [t# box] is equal to
      [t], so it crosses contention exactly when [t] is not mutable ****)

type t = { contents : int }
let f (x : t# box @ contended) = use_uncontended x
[%%expect{|
type t = { contents : int; }
val f : t @ contended -> unit = <fun>
|}]

let f (x : #(int * int) box @ contended) = use_uncontended x
[%%expect{|
val f : int * int @ contended -> unit = <fun>
|}]

type r = { mutable contents : int }
let f (x : r# box @ contended) = use_uncontended x
[%%expect{|
type r = { mutable contents : int; }
Line 2, characters 49-50:
2 | let f (x : r# box @ contended) = use_uncontended x
                                                     ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* The standard-library conversions preserve the boxed type. *)
let boxed_float : float = Stdlib.box #3.5
[%%expect{|
val boxed_float : float = 3.5
|}]

let unboxed_float : float# = Stdlib.unbox 3.5
[%%expect{|
val unboxed_float : float# = #3.5
|}]

let boxed_tuple : int * string = Stdlib.box #(7, "seven")
[%%expect{|
val boxed_tuple : int * string = (7, "seven")
|}]

let unboxed_tuple : #(int * string) = Stdlib.unbox (7, "seven")
[%%expect{|
val unboxed_tuple : #(int * string) = #(7, "seven")
|}]

let copy_ref (x : 'a ref) : 'a ref =
  Stdlib.box (Stdlib.unbox x : 'a ref#)
[%%expect{|
val copy_ref : 'a ref -> 'a ref = <fun>
|}]

let copy_contents () =
  let original = ref 7 in
  let copy : int ref = Stdlib.box (Stdlib.unbox original : int ref#) in
  original := 9;
  !copy, !original
let copied_contents = copy_contents ()
[%%expect{|
val copy_contents : unit -> int * int = <fun>
val copied_contents : int * int = (7, 9)
|}]

(* Local payloads remain local through either conversion. *)
let local_box (x : 'a @ local) : 'a box @ local =
  exclave_ Stdlib.box x
[%%expect{|
val local_box : 'a @ local -> 'a box @ local = <fun>
|}]

let local_unbox (x : 'a box @ local) : 'a @ local =
  exclave_ Stdlib.unbox x
[%%expect{|
val local_unbox : 'a box @ local -> 'a @ local = <fun>
|}]

let bad_box_escape (x : 'a @ local) : 'a box = Stdlib.box x
[%%expect{|
Line 1, characters 47-59:
1 | let bad_box_escape (x : 'a @ local) : 'a box = Stdlib.box x
                                                   ^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let bad_unbox_escape (x : 'a box @ local) : 'a = Stdlib.unbox x
[%%expect{|
Line 1, characters 49-63:
1 | let bad_unbox_escape (x : 'a box @ local) : 'a = Stdlib.unbox x
                                                     ^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* Unboxing a mutable record reads its fields without granting access. *)
let bad_unbox_contended (x : int ref @ contended) : int ref# = Stdlib.unbox x
[%%expect{|
Line 1, characters 76-77:
1 | let bad_unbox_contended (x : int ref @ contended) : int ref# = Stdlib.unbox x
                                                                                ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let bad_unbox_read (x : bytes ref @ read uncontended) : bytes ref# =
  Stdlib.unbox x
[%%expect{|
Line 2, characters 15-16:
2 |   Stdlib.unbox x
                   ^
Error: This value is "read" but is expected to be "read_write".
|}]

let unbox_immutable_contended (x : int * int @ contended) : #(int * int) =
  Stdlib.unbox x
[%%expect{|
val unbox_immutable_contended : int * int @ contended -> #(int * int) = <fun>
|}]

let portable_box : (int -> int box) @ portable = Stdlib.box
let portable_unbox : (int box -> int) @ portable = Stdlib.unbox
[%%expect{|
val portable_box : int -> int box = <fun>
val portable_unbox : int box -> int = <fun>
|}]
