(* TEST
    expect;
*)
(* Tests for universally quantified types *)

(******************************************)

(* This type used to cause an infinite loop between [Ctype.estimate_type_jkind]
   and [Jkind.normalize]. *)
(* CR layouts v2.8: It's not clear what kind this type should get. At the
   moment, it gets value. But should we give it immutable_data? *)
type 'a t = { f : 'b. 'b t }
[%%expect {|
Uncaught exception: Stack overflow
|}]

type 'a t : immutable_data = { f : 'b. 'b t }
[%%expect {|
Uncaught exception: Stack overflow
|}]

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
Line 1, characters 0-75:
1 | type t : immutable_data with Abs.t = { foo : 'a. (Abs.t * 'a ignore_type) }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a. Abs.t * unit
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data with Abs.t
         because of the annotation on the declaration of the type t.
|}]

(******************************************)

type 'a u
type t : immutable_data with (type : value) u = { foo : 'a. 'a u }
(* CR layouts v2.8: This should be accepted. *)
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
