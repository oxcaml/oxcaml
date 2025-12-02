(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any) t = Nope | Yeah of 'a
[%%expect{|
type ('a : any) t = { fst : 'a; snd : 'a; }
|}]

let to_option t = match t with Yeah a -> Some a | Nope -> None
[%%expect{|
Line 1, characters 12-17:
1 | let fst t = t.fst
                ^^^^^
Error: Cannot access record with unrepresentable field.
       The record has type 'a t, whose field fst is not representable.
|}]

let to_option (t : int t) = match t with Yeah a -> Some a | Nope -> None
[%%expect{|
val fst : int t -> int = <fun>
|}]

let to_option (type a : value) (t : a t) =
  match t with Yeah a -> Some a | Nope -> None
[%%expect{|
val fst : 'a t -> 'a = <fun>
|}]

let to_option (t : int64# t) = match t with Yeah a -> Some a | Nope -> None
[%%expect{|
val fst : int64# t -> int64# = <fun>
|}]

let to_option (type a : bits64) (t : a t) =
  match t with Yeah a -> Some a | Nope -> None
[%%expect{|
val fst : ('a : bits64). 'a t -> 'a = <fun>
|}]

let nope = Nope
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let nope : 'a. 'a t = Nope
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let yeah a = Yeah a
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let yeah (a : int) = Yeah a
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let yeah a : int t = Yeah a
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let yeah (type a : value) (a : a) = Yeah a
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let yeah (type a : value) a : a t = Yeah a
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let yeah (a : int64#) = Yeah a
[%%expect{|
val make : int64# -> int64# -> int64# t = <fun>
|}]

let yeah (type a : bits64) (a : a) = Yeah a
[%%expect{|
val make : ('a : bits64). 'a -> 'a -> 'a t = <fun>
|}]
