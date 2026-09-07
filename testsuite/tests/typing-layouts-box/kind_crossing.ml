(* TEST
 {
   expect;
 }{
   flags = "-no-ikinds";
   expect;
 }
*)

(* Tests for the mode crossing of box kinds. *)

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
       The kind of the first is immediate box
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
       The kind of the first is immediate box
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
       The kind of the first is immediate box
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
       The kind of the first is value box
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
       The kind of the first is (bits64 mod everything) box
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
       The kind of (int -> int) t is value non_float box
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
       The kind of int t is mutable_data box with int
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
       The kind of int t is mutable_data box
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
       The kind of int t is mutable_data box with int
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
       The kind of t is immediate box
         because of the definition of t at line 1, characters 0-22.
       But the kind of t must be a subkind of value mod external_
         because of the definition of ext_req at line 2, characters 0-39.
|}]
