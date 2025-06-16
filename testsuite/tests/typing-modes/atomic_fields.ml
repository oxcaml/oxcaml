(* TEST
    expect;
*)

type r = { mutable f : int ref [@atomic] }
[%%expect{|
type r = { mutable(<non-legacy>) f : int ref; }
|}]

let get (r @ contended) = r.f
[%%expect{|
val get : r @ contended -> int ref @ contended = <fun>
|}]

type r = { mutable f : (unit -> unit) [@atomic] }
[%%expect{|
type r = { mutable(<non-legacy>) f : unit -> unit; }
|}]

let set r v = r.f <- v
[%%expect{|
val set : r -> (unit -> unit) @ portable -> unit = <fun>
|}]

let set (r @ contended) v = r.f <- v
[%%expect{|
val set : r @ contended -> (unit -> unit) @ portable -> unit = <fun>
|}]

let set (r @ contended) (v @ portable) = r.f <- v
[%%expect{|
val set : r @ contended -> (unit -> unit) @ portable -> unit = <fun>
|}]

let set (r @ uncontended) (v @ nonportable) = r.f <- v
(* CR atomic-fields: This should be accepted *)
[%%expect{|
Line 1, characters 53-54:
1 | let set (r @ uncontended) (v @ nonportable) = r.f <- v
                                                         ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let set (r @ contended) (v @ nonportable) = r.f <- v
[%%expect{|
Line 1, characters 51-52:
1 | let set (r @ contended) (v @ nonportable) = r.f <- v
                                                       ^
Error: This value is "nonportable" but expected to be "portable".
|}]

type r = { mutable f : (unit -> unit) @@ portable [@atomic] }
[%%expect{|
type r = { mutable(<non-legacy>) f : unit -> unit @@ portable; }
|}]

let set r v = r.f <- v
[%%expect{|
val set : r -> (unit -> unit) @ portable -> unit = <fun>
|}]


module Atomic : sig @@ portable
  type ('a : value_or_null) t : mutable_data with 'a @@ global
  val get : ('a : value_or_null). 'a t @ contended local -> 'a @ contended

  val set
    : ('a : value_or_null mod contended).
       'a t @ contended local
    -> 'a @ portable
    -> unit
end = struct
  type ('a : value_or_null) t = { mutable contents : 'a @@ global [@atomic] }
  let get { contents } = contents
  let set t v = t.contents <- v
end
[%%expect{|
module Atomic :
  sig
    type 'a t : mutable_data with 'a @@ unyielding
    val get : 'a t @ local contended -> 'a @ contended @@ portable
    val set :
      ('a : value_or_null mod contended).
        'a t @ local contended -> 'a @ portable -> unit
      @@ portable
  end
|}]
