(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(** Test that quotes in kinds work **)

(* Annotated quote-kinded abstract types *)
type t0 : value
type t1 : <[value]>
type t2 : <[<[value]>]>
[%%expect {|
type t0
type t1 : <[value]>
type t2 : <[<[value]>]>
|}]
type t0 : immediate
type t1 : <[immediate]>
type t2 : <[<[immediate]>]>
[%%expect {|
type t0 : immediate
type t1 : <[immediate]>
type t2 : <[<[immediate]>]>
|}]
type t0 : value & immediate & bits64
type t1 : <[value & immediate & bits64]>
type t2 : <[<[value & immediate & bits64]>]>
[%%expect {|
type t0 : value & value & bits64
type t1 : <[value & value & bits64]>
type t2 : <[<[value & value & bits64]>]>
|}]
type t0 : value mod portable
type t1 : <[value]> mod portable
type t2 : <[<[value]>]> mod portable
[%%expect {|
type t0 : value mod portable
type t1 : <[value mod portable]>
type t2 : <[<[value mod portable]>]>
|}]
type t0 : value mod portable
type t1 : <[value mod portable]>
type t2 : <[<[value mod portable]>]>
[%%expect {|
type t0 : value mod portable
type t1 : <[value mod portable]>
type t2 : <[<[value mod portable]>]>
|}]

(* No quoted kinds in products *)
type t : value & <[value]>
[%%expect {|
Line 1, characters 9-26:
1 | type t : value & <[value]>
             ^^^^^^^^^^^^^^^^^
Error: Quoted kinds cannot occur in products.
|}]
type t : <[value]> & <[value]>
[%%expect {|
Line 1, characters 9-30:
1 | type t : <[value]> & <[value]>
             ^^^^^^^^^^^^^^^^^^^^^
Error: Quoted kinds cannot occur in products.
|}]

(* Annotated quoted types *)
type t0 : value = bytes
type t1 : <[value]> = <[bytes]>
type t2 : <[<[value]>]> = <[<[bytes]>]>
[%%expect {|
type t0 = bytes
type t1 = <[bytes]>
type t2 = <[<[bytes]>]>
|}]
type t0 : immediate = int
type t1 : <[immediate]> = <[int]>
type t2 : <[<[immediate]>]> = <[<[int]>]>
[%%expect {|
type t0 = int
type t1 = <[int]>
type t2 = <[<[int]>]>
|}]

(* Abstracting quoted types *)
module Values : sig
  type t0 : value
  type t1 : <[value]>
  type t2 : <[<[value]>]>
end = struct
  type t0 = bytes
  type t1 = <[bytes]>
  type t2 = <[<[bytes]>]>
end
[%%expect {|
module Values : sig type t0 type t1 : <[value]> type t2 : <[<[value]>]> end
|}]
module Immediates : sig
  type t0 : immediate
  type t1 : <[immediate]>
  type t2 : <[<[immediate]>]>
end = struct
  type t0 = int
  type t1 = <[int]>
  type t2 = <[<[int]>]>
end
[%%expect {|
module Immediates :
  sig
    type t0 : immediate
    type t1 : <[immediate]>
    type t2 : <[<[immediate]>]>
  end
|}]

(* Stage annotation errors *)
type t : value = <[bytes]>
[%%expect {|
Line 1, characters 0-26:
1 | type t : value = <[bytes]>
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "<[bytes]>" is <[mutable_data]>
         because it is the primitive type bytes.
       But the kind of type "<[bytes]>" must be a subkind of value
         because of the definition of t at line 1, characters 0-26.
|}]
type t : <[value]> = bytes
[%%expect {|
Line 1, characters 0-26:
1 | type t : <[value]> = bytes
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "bytes" is mutable_data
         because it is the primitive type bytes.
       But the kind of type "bytes" must be a subkind of <[value]>
         because of the definition of t at line 1, characters 0-26.
|}]

(* Inclusion check errors with quoted kinds *)
module M : sig
  type t
end = struct
  type t = <[bytes]>
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <[bytes]>
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = <[bytes]> end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = <[bytes]>
       is not included in
         type t
       The kind of the first is <[mutable_data]>
         because it is the primitive type bytes.
       But the kind of the first must be a subkind of value
         because of the definition of t at line 2, characters 2-8.
|}]
module M : sig
  type t : value
end = struct
  type t = <[bytes]>
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <[bytes]>
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = <[bytes]> end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = <[bytes]>
       is not included in
         type t
       The kind of the first is <[mutable_data]>
         because it is the primitive type bytes.
       But the kind of the first must be a subkind of value
         because of the definition of t at line 2, characters 2-16.
|}]
module M : sig
  type t : <[value]>
end = struct
  type t = bytes
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = bytes
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = bytes end
       is not included in
         sig type t : <[value]> end
       Type declarations do not match:
         type t = bytes
       is not included in
         type t : <[value]>
       The kind of the first is mutable_data
         because it is the primitive type bytes.
       But the kind of the first must be a subkind of <[value]>
         because of the definition of t at line 2, characters 2-20.
|}]

(* Inclusion checks under quotes happen as normal *)
(* fine: int is immediate *)
module M : sig
  type t : <[immediate]>
end = struct
  type t = <[int]>
end
[%%expect {|
module M : sig type t : <[immediate]> end
|}]
(* error: bytes is not immediate *)
module M : sig
  type t : <[immediate]>
end = struct
  type t = <[bytes]>
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <[bytes]>
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = <[bytes]> end
       is not included in
         sig type t : <[immediate]> end
       Type declarations do not match:
         type t = <[bytes]>
       is not included in
         type t : <[immediate]>
       The kind of the first is <[mutable_data]>
         because it is the primitive type bytes.
       But the kind of the first must be a subkind of <[immediate]>
         because of the definition of t at line 2, characters 2-24.
|}]
