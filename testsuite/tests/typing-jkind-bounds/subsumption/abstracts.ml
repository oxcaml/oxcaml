(* TEST
    flags = "-extension layouts_alpha -ikinds";
    expect;
*)

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t = (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

type t1 : value

module type S = sig
  type t : immutable_data with t1
end

module M : S = struct
  type t = Foo of (int * t1)
end
[%%expect{|
type t1
module type S = sig type t : immutable_data with t1 end
module M : S
|}]


type t1 : value

module type S = sig
  type t : value mod portable with t1
end

module M : S = struct
  type t = Foo of (int * t1)
end
[%%expect {|
type t1
module type S = sig type t : value mod portable with t1 end
module M : S
|}]

type t1 : value

module type S = sig
  type t : value with t1
end

module M : S = struct
  type t = (int * t1)
end
[%%expect{|
type t1
module type S = sig type t end
module M : S
|}]


type t1 : value mod portable

module type S = sig
  type t : value mod portable with t1
end

module M : S = struct
  type t = t1
end
[%%expect{|
type t1 : value mod portable
module type S = sig type t : value mod portable with t1 end
module M : S
|}]

type t1 : value mod portable

module type S = sig
  type t : value with t1
end

module M : S = struct
  type t = t1
end
[%%expect{|
type t1 : value mod portable
module type S = sig type t end
module M : S
|}]

type t1 : value mod portable

module type S = sig
  type t : value mod portable with t1
end

module M : S = struct
  type t = (t1 * int)
end
[%%expect{|
type t1 : value mod portable
module type S = sig type t : value mod portable with t1 end
module M : S
|}]


module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t = (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t : value mod portable = (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t : value mod portable = Foo of (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

module type S = sig
  type t : immutable_data
end

module M : S = struct
  type t = (int * int)
end
[%%expect{|
module type S = sig type t : immutable_data end
module M : S
|}]

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t = int
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

type t : value mod portable = (int * int)
[%%expect{|
type t = int * int
|}]


(* Failure case extracted from DLS *)

module Portable = struct
  type 'a t = { portable : 'a @@ portable } [@@unboxed]
end

module type S = sig
  type 'a key : value mod portable contended
end

module M : S = struct
  type 'a key = int * (unit -> 'a) Portable.t
end
[%%expect{|
module Portable :
  sig type 'a t = { portable : 'a @@ portable; } [@@unboxed] end
module type S = sig type 'a key : value mod contended portable end
Lines 9-11, characters 15-3:
 9 | ...............struct
10 |   type 'a key = int * (unit -> 'a) Portable.t
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a key = int * (unit -> 'a) Portable.t end
       is not included in
         S
       Type declarations do not match:
         type 'a key = int * (unit -> 'a) Portable.t
       is not included in
         type 'a key : value mod contended portable
       The kind of the first is value mod portable immutable non_float
         because it's a tuple type.
       But the kind of the first must be a subkind of
           value mod contended portable
         because of the definition of key at line 6, characters 2-44.
|}]


module Portable = struct
  type 'a t = { portable : 'a @@ portable }
end

module type S = sig
  type 'a key : value mod portable contended
end

module M : S = struct
  type 'a key = int * (unit -> 'a) Portable.t
end
[%%expect{|
module Portable : sig type 'a t = { portable : 'a @@ portable; } end
module type S = sig type 'a key : value mod contended portable end
module M : S
|}]


module Portable = struct
  type 'a t = { portable : 'a @@ portable }
end

module type S = sig
  type 'a key : value mod portable contended
end

module M : S = struct
  type 'a key = int * int Portable.t
end
[%%expect{|
module Portable : sig type 'a t = { portable : 'a @@ portable; } end
module type S = sig type 'a key : value mod contended portable end
module M : S
|}]

module Portable = struct
  type 'a t = { portable : 'a @@ portable contended }
end

module type S = sig
  type 'a key : value mod portable contended
end

module M : S = struct
  type 'a key = int * (unit -> 'a) Portable.t
end
[%%expect{|
module Portable :
  sig type 'a t = { portable : 'a @@ portable contended; } end
module type S = sig type 'a key : value mod contended portable end
module M : S
|}]


module Portable = struct
  type 'a t = { portable : 'a @@ portable }
end

module type S = sig
  type 'a key : value mod portable
end

type t

module M : S = struct
  type 'a key = int * t Portable.t
end

[%%expect{|
module Portable : sig type 'a t = { portable : 'a @@ portable; } end
module type S = sig type 'a key : value mod portable end
type t
module M : S
|}]

module Contended = struct
  type 'a t = { contended : 'a @@ contended }
end

module type S = sig
  type 'a key : value mod contended
end

type t

module M : S = struct
  type 'a key = int * t Contended.t
end

[%%expect{|
module Contended : sig type 'a t = { contended : 'a @@ contended; } end
module type S = sig type 'a key : value mod contended end
type t
module M : S
|}]

module type S = sig
  type 'a key : value mod contended portable
end

type t

module M : S = struct
  type 'a key = int * t Contended.t Portable.t
end

[%%expect{|
module type S = sig type 'a key : value mod contended portable end
type t
module M : S
|}]


module M : S = struct
  type 'a key = int * t Portable.t Contended.t
end

[%%expect{|
module M : S
|}]


module Contended = struct
  type 'a t = { contended : 'a @@ contended } [@@unboxed]
end

module type S = sig
  type 'a key : value mod portable contended
end

type t : value mod portable

module M : S = struct
  type 'a key = int * t Contended.t
end
[%%expect{|
module Contended :
  sig type 'a t = { contended : 'a @@ contended; } [@@unboxed] end
module type S = sig type 'a key : value mod contended portable end
type t : value mod portable
Lines 11-13, characters 15-3:
11 | ...............struct
12 |   type 'a key = int * t Contended.t
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a key = int * t Contended.t end
       is not included in
         S
       Type declarations do not match:
         type 'a key = int * t Contended.t
       is not included in
         type 'a key : value mod contended portable
       The kind of the first is immutable_data with t Contended.t
         because it's a tuple type.
       But the kind of the first must be a subkind of
           value mod contended portable
         because of the definition of key at line 6, characters 2-44.
|}]


module Portable = struct
  type 'a t = { portable : 'a @@ portable } [@@unboxed]
end

module type S = sig
  type 'a key : value mod portable contended
end

type t : value mod contended

module M : S = struct
  type 'a key = int * t Portable.t
end
[%%expect{|
module Portable :
  sig type 'a t = { portable : 'a @@ portable; } [@@unboxed] end
module type S = sig type 'a key : value mod contended portable end
type t : value mod contended
Lines 11-13, characters 15-3:
11 | ...............struct
12 |   type 'a key = int * t Portable.t
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a key = int * t Portable.t end
       is not included in
         S
       Type declarations do not match:
         type 'a key = int * t Portable.t
       is not included in
         type 'a key : value mod contended portable
       The kind of the first is immutable_data with t Portable.t
         because it's a tuple type.
       But the kind of the first must be a subkind of
           value mod contended portable
         because of the definition of key at line 6, characters 2-44.
|}]

module Portable = struct
  type 'a t = { portable : 'a @@ portable } [@@unboxed]
end

module type S = sig
  type 'a key : value mod portable contended
end

type t : value mod contended = unit -> unit

module M : S = struct
  type 'a key = int * t Portable.t
end
[%%expect{|
module Portable :
  sig type 'a t = { portable : 'a @@ portable; } [@@unboxed] end
module type S = sig type 'a key : value mod contended portable end
type t = unit -> unit
Lines 11-13, characters 15-3:
11 | ...............struct
12 |   type 'a key = int * t Portable.t
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a key = int * t Portable.t end
       is not included in
         S
       Type declarations do not match:
         type 'a key = int * t Portable.t
       is not included in
         type 'a key : value mod contended portable
       The kind of the first is value mod portable immutable non_float
         because it's a tuple type.
       But the kind of the first must be a subkind of
           value mod contended portable
         because of the definition of key at line 6, characters 2-44.
|}]
