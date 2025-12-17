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
(* CR layouts v2.8: This should be accepted. *)
[%%expect {|
val v : 'a t = {f = <cycle>}
|}]

type 'a t : immutable_data = { f : 'b. 'b t }
(* CR layouts v2.8: This should be accepted. Internal ticket 5746. *)
[%%expect {|
type 'a t = { f : 'b. 'b t; }
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
|}]

(******************************************)

type 'a u
type t : immutable_data with (type : value) u = { foo : 'a. 'a u }
(* CR layouts v2.8: This should be accepted. Internal ticket 5770. *)
[%%expect{|
type 'a u
type t = { foo : 'a. 'a u; }
|}]

(******************************************)

type 'a u = Foo of int [@@unboxed]
type t : immutable_data = { foo : 'a. 'a u } [@@unboxed]
[%%expect {|
type 'a u = Foo of int [@@unboxed]
type t = { foo : 'a. 'a u; } [@@unboxed]
|}]
