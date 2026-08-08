(* TEST
 flags = "-extension layouts_alpha -no-ikinds";
 expect;
*)

(* Tests for the mode crossing of box kinds: [k box] crosses like
   [mutable_data with] the contents, since the payload could be the unboxed
   version of a mutable record; when the boxed type reduces, it gets the
   payload's boxed type's more accurate kind. This file is duplicated as
   [kind_crossing_ikinds.ml], without [-no-ikinds], to check that the
   engines agree. *)

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
         sig type t : bits64 mod everything box end
       is not included in
         sig type t : value non_float mod contended end
       Type declarations do not match:
         type t : bits64 mod everything box
       is not included in
         type t : value non_float mod contended
       The kind of the first is bits64 mod everything box
         because of the definition of t at line 4, characters 2-38.
       But the kind of the first must be a subkind of
           value non_float mod contended
         because of the definition of t at line 2, characters 2-40.
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
