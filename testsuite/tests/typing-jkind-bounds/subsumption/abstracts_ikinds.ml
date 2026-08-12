(* TEST
    flags = "-extension layouts_alpha";
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


(* This is a regression test of a failure that used to occur in DLS. *)

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
module type S = sig type 'a key : value mod portable contended end
module M : S
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
module type S = sig type 'a key : value mod portable contended end
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
module type S = sig type 'a key : value mod portable contended end
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
module type S = sig type 'a key : value mod portable contended end
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
module type S = sig type 'a key : value mod portable contended end
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
module type S = sig type 'a key : value mod portable contended end
type t : value mod portable
module M : S
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
module type S = sig type 'a key : value mod portable contended end
type t : value mod contended
module M : S
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
module type S = sig type 'a key : value mod portable contended end
type t = unit -> unit
module M : S
|}]
