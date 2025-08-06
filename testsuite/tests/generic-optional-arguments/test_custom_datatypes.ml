(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

type 'a t =
  | None
  | Some of 'a
let f (?x : 'a t) () = x
[%%expect{|
type 'a t = None | Some of 'a
Line 4, characters 12-16:
4 | let f (?x : 'a t) () = x
                ^^^^
Error: Generic optional arguments require types with the [@option_like] attribute.
       Type "t" is not marked as option-like
|}]

type 'a t =
  | None
  | Some of 'a
[@@option_like]
[%%expect{|
type 'a t = None | Some of 'a
|}]

let f (?x : 'a t) () = x
let v = f ()
let v = f ~x:3 ()
let v = f ?x:(Some 3) ()
let v = f ?x:(None) ()
let g (?(x = 2) : _ t) () = x
let v = g()
let v = g ~x:3 ()
let v = g ?x:(Some 3) ()
let v = g ?x:(None) ()
[%%expect{|
val f : (?x):'a t -> unit -> 'a t = <fun>
val v : 'a t = None
val v : int t = Some 3
val v : int t = Some 3
val v : 'a t = None
val g : (?x):int t -> unit -> int = <fun>
val v : int = 2
val v : int = 3
val v : int = 3
val v : int = 2
|}]

type 'a t =
  | S of 'a
  | N
[@@option_like]
[%%expect{|
type 'a t = S of 'a | N
|}]

let f (?x : 'a t) () = x
let v = f ()
let v = f ~x:3 ()
let v = f ?x:(S 3) ()
let v = f ?x:(N) ()
let g (?(x = 2) : _ t) () = x
let v = g()
let v = g ~x:3 ()
let v = g ?x:(S 3) ()
let v = g ?x:(N) ()
[%%expect{|
val f : (?x):'a t -> unit -> 'a t = <fun>
val v : 'a t = N
val v : int t = S 3
val v : int t = S 3
val v : 'a t = N
val g : (?x):int t -> unit -> int = <fun>
val v : int = 2
val v : int = 3
val v : int = 3
val v : int = 2
|}]

(* Valid: gadt *)
type 'a or_null =
  | N
  | S : 'a -> 'a or_null
[@@option_like]

let f (?x : 'a or_null) () = x
let v = f ()
let v = f ~x:3 ()
let v = f ?x:(S 3) ()
let v = f ?x:(N) ()
let g (?(x = 2) : _ or_null) () = x
let v = g()
let v = g ~x:3 ()
let v = g ?x:(S 3) ()
let v = g ?x:(N) ()
[%%expect {|
type 'a or_null = N | S : 'a -> 'a or_null
val f : (?x):'a or_null -> unit -> 'a or_null = <fun>
val v : '_weak1 or_null = N
val v : int or_null = S 3
val v : int or_null = S 3
val v : '_weak2 or_null = N
val g : (?x):int or_null -> unit -> int = <fun>
val v : int = 2
val v : int = 3
val v : int = 3
val v : int = 2
|}]

(* Option-like types in modules *)
module My_option = struct
  type 'a t =
    | Empty
    | Value of 'a
  [@@option_like]
end

let h (?x : 'a My_option.t) () = x
let v1 = h ()
let v2 = h ~x:42 ()

[%%expect{|
module My_option : sig type 'a t = Empty | Value of 'a end @@ stateless
val h : (?x):'a My_option.t -> unit -> 'a My_option.t = <fun>
val v1 : 'a My_option.t = My_option.Empty
val v2 : int My_option.t = My_option.Value 42
|}]

(* Checking structure against signature tests *)
module type S1 = sig
  type 'a t =
    | Empty
    | Value of 'a
  [@@option_like]
  val f : (?x):'a t -> unit -> 'a t
end

module M1 : S1 = struct
  type 'a t =
    | Empty
    | Value of 'a
  [@@option_like]
  let f (?x : 'a t) () = x
end

[%%expect{|
module type S1 =
  sig type 'a t = Empty | Value of 'a val f : (?x):'a t -> unit -> 'a t end
module M1 : S1 @@ stateless
|}]

(* Signature subsumption test *)

module type S2 = sig
  type 'a t =
    | Empty
    | Value of 'a
  [@@option_like]
  val g : (?x):'a t -> (?y):'b t -> unit -> 'a t * 'b t
  val h : (?x):'a t -> (?y):'b t -> unit -> 'a t * 'b t
end

module type S2_restricted = sig
  type 'a t =
    | Empty
    | Value of 'a
  [@@option_like]
  val g : (?x):'a t -> (?y):'b t -> unit -> 'a t * 'b t
end
module M2 : S2 = struct
  type 'a t =
    | Empty
    | Value of 'a
  [@@option_like]
  let g (?x : 'a t) (?y : 'b t) () = (x, y)
  let h (?x : 'a t) (?y : 'b t) () = (x, y)
end
module M2_restricted : S2_restricted = M2
[%%expect{|
module type S2 =
  sig
    type 'a t = Empty | Value of 'a
    val g : (?x):'a t -> (?y):'b t -> unit -> 'a t * 'b t
    val h : (?x):'a t -> (?y):'b t -> unit -> 'a t * 'b t
  end
module type S2_restricted =
  sig
    type 'a t = Empty | Value of 'a
    val g : (?x):'a t -> (?y):'b t -> unit -> 'a t * 'b t
  end
module M2 : S2 @@ stateless
module M2_restricted : S2_restricted @@ stateless
|}]
