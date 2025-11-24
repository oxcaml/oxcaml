(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any) t = { fst : 'a; snd : 'a }
[%%expect{|
type ('a : any) t = { fst : 'a; snd : 'a; }
|}]

let fst t = t.fst
[%%expect{|
Line 1, characters 12-17:
1 | let fst t = t.fst
                ^^^^^
Error: Cannot access record with unrepresentable field.
       The record has type 'a t, whose field fst is not representable.
|}]

let fst (t : int t) = t.fst
[%%expect{|
val fst : int t -> int = <fun>
|}]

let fst (type a : value) (t : a t) = t.fst
[%%expect{|
val fst : 'a t -> 'a = <fun>
|}]

let fst (t : int64# t) = t.fst
[%%expect{|
val fst : int64# t -> int64# = <fun>
|}]

let fst (t : int64# t) =
  match t with { fst; _ } -> fst
[%%expect{|
val fst : int64# t -> int64# = <fun>
|}]

let fst (type a : bits64) (t : a t) = t.fst
[%%expect{|
val fst : ('a : bits64). 'a t -> 'a = <fun>
|}]

let fst (type a : bits64) (t : a t) =
  match t with { fst; _ } -> fst
[%%expect{|
val fst : ('a : bits64). 'a t -> 'a = <fun>
|}]

let make fst snd = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (fst : int) snd = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make (fst : int) (snd : int) = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make fst snd : int t = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make (type a : value) (fst : a) (snd : a) = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (type a : value) fst snd : a t = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (fst : int64#) snd = { fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t = <fun>
|}]

let make (fst : int64#) (snd : int64#) = { fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t = <fun>
|}]

let make (type a : bits64) (fst : a) (snd : a) = { fst; snd }
[%%expect{|
val make : ('a : bits64). 'a -> 'a -> 'a t = <fun>
|}]
