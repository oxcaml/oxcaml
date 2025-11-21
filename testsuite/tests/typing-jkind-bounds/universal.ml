(* TEST
    expect;
*)
(* Tests for universally quantified types *)

(******************************************)

(* Define some utilities for following tests *)

type 'a ignore_type = unit
let require_immutable_data (_ : (_ : immutable_data)) = ()

type ('a, 'b) eq = Eq : ('a, 'a) eq
module Abs : sig
  type t
  val eq : (t, unit) eq
end = struct
  type t = unit
  let eq = Eq
end
[%%expect {|
type 'a ignore_type = unit
val require_immutable_data : ('a : immutable_data). 'a -> unit = <fun>
type ('a, 'b) eq = Eq : ('a, 'a) eq
module Abs : sig type t val eq : (t, unit) eq end
|}]

(******************************************)

(* This type used to cause an infinite loop between [Ctype.estimate_type_jkind]
   and [Jkind.normalize]. *)
type 'a t = { f : 'b. 'b t }
[%%expect {|
type 'a t = { f : 'b. 'b t; }
|}]

let rec v : 'a. 'a t = { f = v }
let () = require_immutable_data v
(* CR layouts v2.8: This should be accepted in principal mode. *)
[%%expect {|
val v : 'a t = {f = <cycle>}
|}, Principal{|
val v : 'a t = {f = <cycle>}
Line 2, characters 32-33:
2 | let () = require_immutable_data v
                                    ^
Error: This expression has type "'a t" but an expression was expected of type
         "('b : immutable_data)"
       The kind of 'a t is immutable_data with 'b. 'b t
         because of the definition of t at line 1, characters 0-28.
       But the kind of 'a t must be a subkind of immutable_data
         because of the definition of require_immutable_data at line 2, characters 27-58.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}]

type 'a t : immutable_data = { f : 'b. 'b t }
(* CR layouts v2.8: This should be accepted. Internal ticket 5746. *)
[%%expect {|
Line 1, characters 0-45:
1 | type 'a t : immutable_data = { f : 'b. 'b t }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'b. 'b t
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}, Principal{|
Line 1, characters 0-45:
1 | type 'a t : immutable_data = { f : 'b. 'b t }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'b. 'b t/2
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
       Note: I gave up trying to find the simplest kind for the first,
       as it is very large or deeply recursive.
|}]

(******************************************)

type t : immutable_data = { foo : 'a. 'a ignore_type }

type t = { foo : 'a. 'a ignore_type }
let f (t : t) = require_immutable_data t
[%%expect{|
type t = { foo : 'a. unit; }
type t = { foo : 'a. unit; }
val f : t -> unit = <fun>
|}]

(******************************************)

type t : immutable_data with Abs.t = { foo : 'a. (Abs.t * 'a ignore_type) }

type t = { foo : 'a. (Abs.t * 'a ignore_type) }
let f (t : t) =
  match Abs.eq with
  | Eq ->
    require_immutable_data t
(* CR layouts v2.8: This should be accepted in principal mode. *)
[%%expect{|
type t = { foo : 'a. Abs.t * unit; }
type t = { foo : 'a. Abs.t * unit; }
val f : t -> unit = <fun>
|}, Principal{|
type t = { foo : 'a. Abs.t * unit; }
type t = { foo : 'a. Abs.t * unit; }
Line 7, characters 27-28:
7 |     require_immutable_data t
                               ^
Error: This expression has type "t" but an expression was expected of type
         "('a : immutable_data)"
       The kind of t is immutable_data with Abs.t
         because of the definition of t at line 3, characters 0-47.
       But the kind of t must be a subkind of immutable_data
         because of the definition of require_immutable_data at line 2, characters 27-58.
|}]

(******************************************)

type 'a u
type t : immutable_data with (type : value) u = { foo : 'a. 'a u }
(* CR layouts v2.8: This should be accepted. Internal ticket 5770. *)
[%%expect{|
type 'a u
Line 2, characters 0-66:
2 | type t : immutable_data with (type : value) u = { foo : 'a. 'a u }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a. 'a u
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of
           immutable_data with (type : value) u
         because of the annotation on the declaration of the type t.
|}]

(******************************************)

type 'a u = Foo of int [@@unboxed]
type t : immutable_data = { foo : 'a. 'a u } [@@unboxed]
[%%expect {|
type 'a u = Foo of int [@@unboxed]
type t = { foo : 'a. 'a u; } [@@unboxed]
|}]
